/-
Copyright (c) 2026 Elimia. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Elimia (Sehun Kim)
-/
import Lean.Parser
import Lean.Elab

open Lean
open Lean.Parser
open Lean.Elab.Command

namespace DocGen4.Process

/--
Try to find the path of the source file.
-/
def getSrc (n:Name) :IO (Option System.FilePath):= do
  let fp ← Lean.getSrcSearchPath
  tryCatch (some <$> (Lean.findLean fp n)) (fun _ ↦ do
    let rp := [(← Lean.findSysroot)/"src"/"lean" ]
    tryCatch (some <$> (Lean.findLean rp n)) (fun _ ↦ do pure none)
  )

/--
Moves the Position of the string to the next line.
-/
partial def nextLine (str : String) (pos : String.Pos.Raw) : String.Pos.Raw :=
  if String.Pos.Raw.atEnd str pos
  then
    str.rawEndPos
  else
    if String.Pos.Raw.get str pos == '\n' then
      String.Pos.Raw.next str pos
    else
      nextLine str (String.Pos.Raw.next str pos)

def getActiveScopes : CommandElabM (Array Name) := do
  let env ← getEnv
  let senvexts ← scopedEnvExtensionsRef.get
  match senvexts[0]? with
  | none => return #[]
  | some senvext =>
    let st := senvext.ext.getState (asyncMode := .local) env
    match st.stateStack with
    | [] => return #[]
    | scope :: _ =>
      let ns := scope.activeScopes
      return ns.toArray

/--
The state for the scope parser.
-/
structure DefScopeState where
  ictx : InputContext
  pos : String.Pos.Raw
  defInfo : Std.HashMap Name (Array Name)
  errorStack : Array String

def DefScopeState.addError (pse : DefScopeState) (msg : String) : DefScopeState :=
  {pse with errorStack := pse.errorStack.push (ToString.toString (pse.ictx.fileMap.toPosition pse.pos) ++ " " ++ msg)}

/--
Snapshots the scope Information and add it to the scope information of the given name.
-/
def DefScopeState.captureDefScope (pse : DefScopeState) (n:Name) : CommandElabM DefScopeState := do
  let s ← getActiveScopes
  pure {pse with defInfo := pse.defInfo.insertIfNew n s}

def DefScopeState.setPos (pse : DefScopeState) (npos : String.Pos.Raw) : DefScopeState :=
  {pse with pos := npos}

def DefScopeState.setToNextLine (pse : DefScopeState) : DefScopeState :=
  {pse with pos := nextLine pse.ictx.inputString pse.pos}

def DefScopeState.atEnd (pse : DefScopeState) : Bool :=
  pse.ictx.atEnd pse.pos

def getParserModuleContext : CommandElabM ParserModuleContext := do
  return { env := ← getEnv, options := {}, openDecls := ← getOpenDecls , currNamespace := ← getCurrNamespace }

def mkParserStateFromDefScopeState (ipse : DefScopeState) :ParserState := { cache := initCacheForInput ipse.ictx.inputString, pos := ipse.pos }

/--
The parser for the header of the file, which stipulates what modules to import.
-/
def headParser (ictx : InputContext) : CommandElabM DefScopeState := do
  let headParser := whitespace >> Module.header.fn
  let dummyEnv ← mkEmptyEnvironment
  let tokens := Module.updateTokens (getTokenTable dummyEnv)
  let pmctx := { env := dummyEnv, options := {} }
  let pstate := headParser.run ictx pmctx tokens (mkParserState ictx.inputString)
  match pstate.errorMsg with
  | some e => return ⟨ictx,pstate.pos,Std.HashMap.emptyWithCapacity,#[e.toString]⟩
  | none => return ⟨ictx,pstate.pos,Std.HashMap.emptyWithCapacity,#[]⟩

/--
The elaboration step of a parsed syntax.

If the syntax is related to the scope action (open, end, namespace, etc.) then execute the elaboration to change the scope.

If the syntax defines something then it captures the information of the scope.

The other syntax are ignored, therefore it estimates the scope without execute the entire syntax.
-/
partial def updateDefScopeState (stx: Syntax) (pse : DefScopeState) : CommandElabM DefScopeState := do
  if stx.isOfKind `Lean.Parser.Command.declaration then
    let stx1 := stx[1]
    let id : Name ←
      if stx1.isOfKind `Lean.Parser.Command.instance then
        let id' := stx1[3][0][0].getId
        if id'.isAnonymous then
          pure .anonymous
        else
          pure id'
      else if stx1.isOfKind `Lean.Parser.Command.example then
        pure .anonymous
      else
        pure stx1[1][0].getId
    if !id.isAnonymous then
      pse.captureDefScope (Lean.Name.append (← getCurrNamespace) id)
    else
      return pse
  else if stx.isOfKind `Lean.Parser.Command.in then
    if stx[0].isOfKind `Lean.Parser.Command.open then
      let env ← getEnv
      elabOpen stx[0]
      let nse ← updateDefScopeState stx[2] pse
      modifyEnv (fun _ ↦ env)
      return nse

    else
      updateDefScopeState stx[2] pse
  else if stx.isOfKind `lemma then
    let id : Name := stx[1][1][0].getId
    if !id.isAnonymous then
      pse.captureDefScope (Lean.Name.append (← getCurrNamespace) id)
    else
      return pse
  else
    try
      if stx.isOfKind `Lean.Parser.Command.open then
        elabOpen stx
      else if stx.isOfKind `Lean.Parser.Command.section then
        elabSection stx
      else if stx.isOfKind `Lean.Parser.Command.namespace then
        elabNamespace stx
      else if stx.isOfKind `Lean.Parser.Command.end then
        elabEnd stx
      return pse
    catch e =>
      return pse.addError ("Command failed with " ++ (← e.toMessageData.toString))

/--
The main parsing step of the file.
-/
partial def bodyParser (ipse : DefScopeState) : CommandElabM DefScopeState := do
  if ipse.atEnd then
    return ipse
  else
    let mainParserFn := whitespace >> topLevelCommandParserFn
    let nps := mainParserFn.run ipse.ictx (← getParserModuleContext) (getTokenTable (← getEnv)) (mkParserStateFromDefScopeState ipse)
    match nps.errorMsg with
    | some e =>
      if e.unexpected == "unexpected end of input" then
        return ipse
      else
        bodyParser ((ipse.addError e.toString).setToNextLine)
    | none =>
      if nps.stxStack.isEmpty then
        bodyParser ((ipse.addError "Syntax Stack is Empty").setToNextLine)
      else
        let rstx := nps.stxStack.back
        let npse ← (updateDefScopeState rstx ipse)
        let npse' := npse.setPos nps.pos
        bodyParser npse'

/--
Parse the given file and extract the scope informations.
-/
def collectScopeInfo
(fname : String) (contents : String) : CommandElabM DefScopeState := do
  let env ← getEnv
  let ictx := mkInputContext contents fname
  let istate ← headParser ictx
  let fstate ← bodyParser istate
  modifyEnv (fun _ ↦ env)
  return fstate

/--
It collect the scopes of the all definition therefore it include the scopes ever opened in the source file.
-/
def maximalScope (sinfo : Std.HashMap Name (Array Name)): Array Name :=
  Std.HashMap.fold (fun r _ nl ↦ Array.foldl (fun r' n ↦ if r'.contains n then r' else r'.push n) r nl) #[] sinfo

/--
Parse the given module and extract the scope informations.

Note: the scope informations are just an estimation since we don't execute every elaborations.
Doing so will be the exact way to extract the scope information but it takes a lot of resources.

-/
def getScopeInfo (modName : Name) : MetaM (Std.HashMap Name (Array Name) × Format):= do
  let fname? ← getSrc modName
  match fname? with
  | some fname =>
    let contents ← IO.FS.readFile fname
    let fs ←
      Lean.liftCommandElabM (collectScopeInfo fname.toString contents)
    -- Lean.Meta.resetSynthInstanceCache
    let fmt : Format := if fs.errorStack.isEmpty then
      "Parsing For " ++ modName.toString ++ " Successed"
    else
      Array.foldl (fun f m ↦ f ++ .line ++ .text m)
        ("WARNING: Parsing For " ++ modName.toString ++ " Failed : ") fs.errorStack
    return ⟨fs.defInfo,fmt⟩
  | none => return ⟨Std.HashMap.emptyWithCapacity, "WARNING: Cannot Find a Source File For Module " ++ modName.toString⟩


end DocGen4.Process
