import Datastar.Types

namespace Datastar

structure PatchSignals where
  signals : String
  onlyIfMissing : Bool := defaultOnlyIfMissing
  eventId : Option String := none
  retryDuration : Nat := defaultRetryDuration
deriving DecidableEq, Repr

def patchSignals
    (signals : String)
    (onlyIfMissing : Bool := defaultOnlyIfMissing)
    (eventId : Option String := none)
    (retryDuration : Nat := defaultRetryDuration) : PatchSignals :=
  { signals, onlyIfMissing, eventId, retryDuration }

instance : ToEvent PatchSignals where
  toEvent ps :=
    { eventType := .patchSignals
      eventId := ps.eventId
      retry := ps.retryDuration
      dataLines :=
        (if ps.onlyIfMissing then #["onlyIfMissing true"] else #[])
        ++ (lines ps.signals).map ("signals " ++ ·) }

end Datastar
