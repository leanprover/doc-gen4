/-
Copyright (c) 2026 Samuel Schlesinger. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Samuel Schlesinger
-/

-- Standalone Lake configuration for test-src-uri-job-dependency.sh.
-- No doc-gen4 or external package is built by this diagnostic.
import Lake
import Std.Sync.Mutex

open System Lake DSL

package srcUriSchedulingProbe

def threadCount : IO Nat := do
  return (← ("/proc/self/task" : FilePath).readDir).size

def probe (blocking failUri : Bool) : FetchM (Job Unit) := do
  let count := 8
  let mode := if blocking then "blocking" else "composed"
  let release ← IO.Promise.new (α := Unit)
  let constructed ← IO.Promise.new (α := Unit)
  let entered ← Std.Mutex.new 0
  let completed ← Std.Mutex.new 0
  let baseline ← threadCount
  let uriJob : Job String ← Job.async do
    discard <| IO.wait release.result!
    if failUri then error "URI dependency failed"
    return "https://example.org/blob/frozen/Module.lean"
  let controller ← IO.asTask (prio := .dedicated) do
    try
      discard <| IO.wait constructed.result!
      let mut attempts := 0
      while (← entered.atomically get) < count && attempts < 400 do
        IO.sleep 5
        attempts := attempts + 1
      let started ← entered.atomically get
      let doneBefore ← completed.atomically get
      let pending := !(← IO.hasFinished uriJob.task)
      let waiting ← threadCount
      IO.println s!"PROBE mode={mode} failure={failUri} baseline={baseline} waiting={waiting} entered={started} doneBefore={doneBefore} pending={pending}"
      return started == count && doneBefore == 0 && pending
    finally
      release.resolve ()
  let mut consumers : Array (Job String) := #[]
  for i in [:count] do
    let work := fun uri => do
      completed.atomically (modify (· + 1))
      if uri != "https://example.org/blob/frozen/Module.lean" then
        error "URI value changed"
      IO.FS.writeFile s!"marker-{mode}-{failUri}-{i}" uri
      return uri
    let ready := Job.pure ()
    let consumer ← if blocking then
      ready.mapM fun _ => do
        entered.atomically (modify (· + 1))
        let uri ← uriJob.await
        work uri
    else
      ready.bindM fun _ => do
        entered.atomically (modify (· + 1))
        uriJob.mapM work
    consumers := consumers.push consumer
  constructed.resolve ()
  (Job.collectArray consumers).mapM fun values => do
    unless ← IO.ofExcept controller.get do error "No real dependency wait was observed"
    unless values.size == count && (← completed.atomically get) == count do
      error "Consumer count changed"
    logInfo s!"PROBE completed mode={mode} count={values.size}"

target blocking : Unit := probe true false
target composed : Unit := probe false false
target blockingFailure : Unit := probe true true
target composedFailure : Unit := probe false true

def appendEvent (file : FilePath) : IO Unit :=
  IO.FS.withFile file .append fun handle => handle.putStrLn "event"

def markerProbe (blocking : Bool) : FetchM (Job FilePath) := do
  let mode := if blocking then "blocking" else "composed"
  let marker : FilePath := s!"cached-marker-{mode}"
  let lookup : SpawnM (Job String) := Job.async do
    appendEvent s!"queries-{mode}"
    if (← IO.getEnv "PROBE_URI_FAIL") == some "1" then
      error "URI dependency failed"
    return "https://example.org/blob/frozen/Module.lean"
  let generate := fun uri => do
    appendEvent s!"generations-{mode}"
    IO.FS.writeFile marker uri
  let ready := Job.pure ()
  if blocking then
    ready.mapM fun _ => do
      buildFileUnlessUpToDate' marker do
        let uriJob ← lookup
        let uri ← uriJob.await
        generate uri
      return marker
  else
    ready.bindM fun _ => do
      let uriJob ← lookup
      uriJob.mapM fun uri => do
        buildFileUnlessUpToDate' marker do
          generate uri
        return marker

target cacheBlocking : FilePath := markerProbe true
target cacheComposed : FilePath := markerProbe false
