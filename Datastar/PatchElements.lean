import Datastar.Types

namespace Datastar

structure PatchElements where
  elements : Option String := none
  selector : Option String := none
  mode : ElementPatchMode := defaultPatchMode
  useViewTransition : Bool := defaultUseViewTransition
  ns : ElementNamespace := defaultNamespace
  eventId : Option String := none
  retryDuration : Nat := defaultRetryDuration
deriving DecidableEq, Repr

/--
Build a `PatchElements` event with sensible defaults.
-/
def patchElements
    (html : String)
    (selector : Option String := none)
    (mode : ElementPatchMode := defaultPatchMode)
    (useViewTransition : Bool := defaultUseViewTransition)
    (ns : ElementNamespace := defaultNamespace)
    (eventId : Option String := none)
    (retryDuration : Nat := defaultRetryDuration) : PatchElements :=
  { elements := if html.isEmpty then none else some html
    selector
    mode
    useViewTransition
    ns
    eventId
    retryDuration
  }

def removeElements
    (selector : String)
    (useViewTransition : Bool := defaultUseViewTransition)
    (eventId : Option String := none)
    (retryDuration : Nat := defaultRetryDuration) : PatchElements :=
  { selector := some selector, mode := .remove, useViewTransition, eventId, retryDuration }

instance : ToEvent PatchElements where
  toEvent pe :=
    { eventType := .patchElements
      eventId := pe.eventId
      retry := pe.retryDuration
      dataLines :=
        (match pe.selector with
          | some s => #["selector " ++ s]
          | none => #[])
        ++ (if pe.mode != defaultPatchMode then #["mode " ++ pe.mode.toString] else #[])
        ++ (if pe.useViewTransition then #["useViewTransition true"] else #[])
        ++ (if pe.ns != defaultNamespace then #["namespace " ++ pe.ns.toString] else #[])
        ++ (match pe.elements with
          | some html => (lines html).map ("elements " ++ ·)
          | none => #[]) }

end Datastar
