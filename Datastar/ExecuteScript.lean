import Datastar.Types

namespace Datastar

structure ExecuteScript where
  script : String
  autoRemove : Bool := defaultAutoRemove
  attributes : Array String := #[]
  eventId : Option String := none
  retryDuration : Nat := defaultRetryDuration
deriving DecidableEq, Repr

def executeScript
    (script : String)
    (autoRemove : Bool := defaultAutoRemove)
    (attributes : Array String := #[])
    (eventId : Option String := none)
    (retryDuration : Nat := defaultRetryDuration) : ExecuteScript :=
  { script, autoRemove, attributes, eventId, retryDuration }

namespace ExecuteScript

private def openTag (es : ExecuteScript) : String :=
  "<script"
    ++ (if es.autoRemove then " data-effect=\"el.remove()\"" else "")
    ++ es.attributes.foldl (fun acc attr => acc ++ " " ++ attr) ""
    ++ ">"

private def closeTag : String := "</script>"

def scriptLines (es : ExecuteScript) : Array String :=
  match lines es.script with
  | #[] => #["elements " ++ es.openTag ++ closeTag]
  | #[single] => #["elements " ++ es.openTag ++ single ++ closeTag]
  | multiple =>
    #["elements " ++ es.openTag]
      ++ multiple.map ("elements " ++ ·)
      ++ #["elements " ++ closeTag]

end ExecuteScript

instance : ToEvent ExecuteScript where
  toEvent es :=
    { -- Correct, there is no dedicated execute-script event type, see the ADR:
      -- https://github.com/starfederation/datastar/blob/develop/sdk/ADR.md
      eventType := .patchElements
      eventId := es.eventId
      retry := es.retryDuration
      dataLines := #["selector body", "mode append"] ++ es.scriptLines }

end Datastar
