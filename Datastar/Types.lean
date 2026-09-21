namespace Datastar

inductive EventType where
  | patchElements
  | patchSignals
deriving DecidableEq, Repr

def EventType.toString : EventType → String
  | .patchElements => "datastar-patch-elements"
  | .patchSignals => "datastar-patch-signals"

instance : ToString EventType := ⟨EventType.toString⟩

inductive ElementPatchMode where
  | outer
  | inner
  | remove
  | replace
  | prepend
  | append
  | before
  | after
deriving DecidableEq, Repr

def ElementPatchMode.toString : ElementPatchMode → String
  | .outer => "outer"
  | .inner => "inner"
  | .remove => "remove"
  | .replace => "replace"
  | .prepend => "prepend"
  | .append => "append"
  | .before => "before"
  | .after => "after"

instance : ToString ElementPatchMode := ⟨ElementPatchMode.toString⟩

inductive ElementNamespace where
  | html
  | svg
  | mathml
deriving DecidableEq, Repr

def ElementNamespace.toString : ElementNamespace → String
  | .html => "html"
  | .svg => "svg"
  | .mathml => "mathml"

instance : ToString ElementNamespace := ⟨ElementNamespace.toString⟩

def defaultRetryDuration : Nat := 1000

def defaultPatchMode : ElementPatchMode := .outer

def defaultUseViewTransition : Bool := false

def defaultOnlyIfMissing : Bool := false

def defaultAutoRemove : Bool := true

def defaultNamespace : ElementNamespace := .html

/--
Internal representation of a rendered SSE event.

Users don't construct these directly; use `patchElements`, `patchSignals` or `executeScript`.
-/
structure DatastarEvent where
  eventType : EventType
  eventId : Option String := none
  retry : Nat := defaultRetryDuration
  dataLines : Array String := #[]
deriving DecidableEq, Repr

class ToEvent (α : Type) where
  toEvent : α → DatastarEvent

export ToEvent (toEvent)

instance : ToEvent DatastarEvent := ⟨id⟩

/--
Split lines like Haskell's `Data.Text.lines`; a trailing newline does
not produce an empty line.

* `lines "" = #[]`
* `lines "a\n" = #["a"]`
* `lines "a\n\nb" = #["a", "", "b"]`
-/
def lines (s : String) : Array String :=
  let parts := (s.splitOn "\n").toArray
  if parts.back? == some "" then parts.pop else parts

end Datastar
