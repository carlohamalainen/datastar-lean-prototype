import Std.Http
import Lean.Data.Json
import Datastar.Types
import Datastar.SSE

open Std Async Http
open Lean (FromJson)

namespace Datastar

/--
An opaque handle for sending SSE events to the browser.

Obtain one from the callback passed to `sseResponse`.

The handle is safe to share between tasks.
-/
structure ServerSentEventGenerator where
  private mk ::
  private stream : Body.Stream
  private lock : Std.Semaphore

namespace ServerSentEventGenerator

private def withLock (gen : ServerSentEventGenerator) (action : Async α) : Async α := do
  let p ← gen.lock.acquire
  let res : Option Unit ← await p.result?

  match res with
    | none => throw (IO.userError "SSE generator lock was dropped")
    | some _ => try action finally gen.lock.release

def send [ToEvent α] (gen : ServerSentEventGenerator) (x : α) : Async Unit := do
  let event := toEvent x
  let text := renderEvent event
  gen.withLock do
    gen.stream.send { data := text.toUTF8 }

end ServerSentEventGenerator

private def cacheControl : Header.Name := .mk "cache-control"

def sseResponse (callback : ServerSentEventGenerator → ContextAsync Unit) : ContextAsync (Response Body.Any) := do
  let ctx ← ContextAsync.getContext
  let lock ← Semaphore.new 1
  let builder := Response.ok
        |>.header cacheControl (.mk "no-cache")
        |>.header Header.Name.contentType (.mk "text/event-stream")

  builder.stream fun stream =>
    ContextAsync.runIn ctx (callback {stream, lock})

private def decodeJson [FromJson α] (raw : String) : Except String α := do
  let j ← Lean.Json.parse raw
  Lean.fromJson? j

def signalsFromQuery [FromJson α] (req : Request β) : Except String α := do
  let some (some encoded) := req.line.uri.query.find? "datastar"
    | throw "missing 'datastar' query parameter"
  let some raw := encoded.decode
    | throw "'datastar' query parameter is not valid percent-encoded UTF-8"
  decodeJson raw

def signalsFromBody [FromJson α] (req : Request Body.Stream) : ContextAsync (Except String α) := do
  try
    let raw : String ← req.body.readAll
    return decodeJson raw
  catch e =>
    return .error s!"could not read request body: {e}"

def readSignals [FromJson α] (req : Request Body.Stream) : ContextAsync (Except String α) :=
  if req.line.method == .get || req.line.method == .delete then
    pure (signalsFromQuery req)
  else
    signalsFromBody req

end Datastar
