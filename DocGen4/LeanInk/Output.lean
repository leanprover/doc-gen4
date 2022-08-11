/-
Copyright (c) 2022 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving, Xubai Wang
-/
import DocGen4.Output.Base
import DocGen4.Output.ToHtmlFormat
import Lean.Data.Json
import LeanInk.Annotation.Alectryon

namespace LeanInk.Annotation.Alectryon

open DocGen4 Output
open scoped DocGen4.Jsx

structure AlectryonContext where
  counter : Nat

abbrev AlectryonT := StateT AlectryonContext
abbrev AlectryonM := AlectryonT HtmlM

def getNextButtonLabel : AlectryonM String := do
  let val ← get
  let newCounter := val.counter + 1
  set { val with counter := newCounter }
  pure s!"plain-lean4-lean-chk{val.counter}"

def TypeInfo.toHtml (tyi : TypeInfo) : AlectryonM Html := do
  pure
    <div class="alectryon-type-info-wrapper">
      <small class="alectryon-type-info">
        <div class="alectryon-goals">
          <blockquote class="alectryon-goal">
            <div class="goal-hyps">
              <span class="hyp-type">
                <var>{tyi.name}</var>
                <b>: </b>
                <span>[←DocGen4.Output.infoFormatToHtml tyi.type.fst]</span>
              </span>
            </div>
          </blockquote>
        </div>
      </small>
    </div>

def Token.processSemantic (t : Token) : Html :=
  match t.semanticType with
  | some "Name.Attribute" => <span class="na">{t.raw}</span>
  | some "Name.Variable" => <span class="nv">{t.raw}</span>
  | some "Keyword" => <span class="k">{t.raw}</span>
  | _ => Html.text t.raw

def Token.toHtml (t : Token) : AlectryonM Html := do
  -- Right now t.link is always none from LeanInk, ignore it
  -- TODO: render docstring
  let mut parts := #[]
  if let some tyi := t.typeinfo then
    parts := parts.push <| ←tyi.toHtml

  parts := parts.push t.processSemantic

  pure
    -- TODO: Show rest of token
    <span class="alectryon-token">
      [parts]
    </span>

def Contents.toHtml : Contents → AlectryonM Html
  | .string value =>
    pure
      <span class="alectryon-wsp">
        {value}
      </span>
  | .experimentalTokens values => do
    let values ← values.mapM Token.toHtml
    pure
      <span class="alectryon-wsp">
        [values]
      </span>

def Hypothesis.toHtml (h : Hypothesis) : AlectryonM Html := do
  let mut hypParts := #[<var>[h.names.intersperse ", " |>.map Html.text |>.toArray]</var>]
  if h.body.snd != "" then
    hypParts := hypParts.push
      <span class="hyp-body">
        <b>:= </b>
        <span>[←infoFormatToHtml h.body.fst]</span>
      </span>
  hypParts := hypParts.push
      <span class="hyp-type">
        <b>: </b>
        <span >[←infoFormatToHtml h.type.fst]</span>
      </span>

  pure
    <span>
      [hypParts]
    </span>

def Goal.toHtml (g : Goal) : AlectryonM Html := do
  let mut hypotheses := #[]
  for hyp in g.hypotheses do
    let rendered ← hyp.toHtml
    hypotheses := hypotheses.push rendered
    hypotheses := hypotheses.push <br/>
  let conclusionHtml ←
    match g.conclusion with
    | .typed info _ => infoFormatToHtml info
    | .untyped str => pure <| #[Html.text str]

  pure
    <blockquote class="alectryon-goal">
      <div class="goal-hyps">
        [hypotheses]
      </div>
      <span class="goal-separator">
        <hr><span class="goal-name">{g.name}</span></hr>
      </span>
      <div class="goal-conclusion">
        [conclusionHtml]
      </div>
    </blockquote>

def Message.toHtml (m : Message) : AlectryonM Html := do
  pure
    <blockquote class="alectryon-message">
      -- TODO: This might have to be done in a fancier way
      {m.contents}
    </blockquote>

def Sentence.toHtml (s : Sentence) : AlectryonM Html := do
  let messages :=
    if s.messages.size > 0 then
      #[
        <div class="alectryon-messages">
          [←s.messages.mapM Message.toHtml]
        </div>
      ]
    else
      #[]
  
  let goals :=
    if s.goals.size > 0 then
      -- TODO: Alectryon has a "alectryon-extra-goals" here, implement it
      #[
        <div class="alectryon-goals">
          [←s.goals.mapM Goal.toHtml]
        </div>
      ]
    else
      #[]

  let buttonLabel ← getNextButtonLabel

  pure
    <span class="alectryon-sentence">
      <input class="alectryon-toggle" id={buttonLabel} style="display: none" type="checkbox"/>
      <label class="alectryon-input" for={buttonLabel}>
        {←s.contents.toHtml}
      </label>
      <small class="alectryon-output">
        [messages]
        [goals]
      </small>
    </span>

def Text.toHtml (t : Text) : AlectryonM Html := t.contents.toHtml

def Fragment.toHtml : Fragment → AlectryonM Html
  | .text value => value.toHtml
  | .sentence value => value.toHtml

def baseHtml (content : Array Html) : AlectryonM Html := do
  let banner :=
    <div «class»="alectryon-banner">
      Built with <a href="https://github.com/leanprover/doc-gen4">doc-gen4</a>, running Lean4.
      Bubbles (<span class="alectryon-bubble"></span>) indicate interactive fragments: hover for details, tap to reveal contents.
      Use <kbd>Ctrl+↑</kbd> <kbd>Ctrl+↓</kbd> to navigate, <kbd>Ctrl+🖱️</kbd> to focus.
      On Mac, use <kbd>Cmd</kbd> instead of <kbd>Ctrl</kbd>.
    </div>

  pure
    <html lang="en" class="alectryon-standalone">
      <head>
        <meta charset="UTF-8"/>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>

        <link rel="stylesheet" href={s!"{←getRoot}src/alectryon.css"}/>
        <link rel="stylesheet" href={s!"{←getRoot}src/pygments.css"}/>
        <link rel="stylesheet" href={s!"{←getRoot}src/docutils_basic.css"}/>
        <link rel="shortcut icon" href={s!"{←getRoot}favicon.ico"}/>

        <script defer="true" src={s!"{←getRoot}src/alectryon.js"}></script>
      </head>
      <body>
        <article class="alectryon-root alectryon-centered">
          {banner}
          <pre class="alectryon-io highlight">
            [content]
          </pre>
        </article>
      </body>
    </html>

def annotationsToFragments (as : List Annotation.Annotation) : AnalysisM (List Fragment) := do
  let config ← read
  annotateFileWithCompounds [] config.inputFileContents as

-- TODO: rework monad mess
def renderAnnotations (as : List Annotation.Annotation) : HtmlT AnalysisM Html := do
  let fs ← annotationsToFragments as
  let (html, _) ← fs.mapM Fragment.toHtml >>= (baseHtml ∘ List.toArray) |>.run { counter := 0 }
  pure html

end LeanInk.Annotation.Alectryon
