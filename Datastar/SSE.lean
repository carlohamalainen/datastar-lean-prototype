import Datastar.Types

namespace Datastar

def renderEvent (event : DatastarEvent) : String :=
  "event: " ++ event.eventType.toString ++ "\n"
    ++ (match event.eventId with
      | some eid => "id: " ++ eid ++ "\n"
      | none => "")
    ++ (if event.retry != defaultRetryDuration then "retry: " ++ toString event.retry ++ "\n" else "")
    ++ event.dataLines.foldl (fun acc line => acc ++ "data: " ++ line ++ "\n") ""
    ++ "\n"

def render [ToEvent α] (x : α) : String :=
  renderEvent (toEvent x)

end Datastar
