module Comm.Server where

import Prelude (void, Unit, bind, (<>), ($), pure, unit)
import Control.Monad.Eff
import Node.HTTP (HTTP, listen, createServer, setHeader, requestMethod, Request, Response,
                  responseAsStream, requestAsStream, setStatusCode)
import Node.Stream (end, writeString, pipe)
import Node.Encoding (Encoding(..))
import Data.Foldable (foldMap)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))

import Partial.Unsafe (unsafeCrashWith)

handler :: forall eff. Request -> Response -> Eff (http :: HTTP | eff) Unit
handler req res = do
  _ <- setStatusCode res 200
  -- Notice we dont have to do let/in here.  The inputStream and outputStream are in scope without the 'in'
  let inputStream  = requestAsStream req  -- Convert the Request and Response objects as node streams
      outputStream = responseAsStream res
  -- FIXME: Change this to a better routing model.
  case requestMethod req of
    "GET" -> do
      let html = foldMap (_ <> "\n")
            [ "<form method='POST' action='/'>"
            , "  <input name='text' type='text'>"
            , "  <input type='submit'>"
            , "</form>"
            ]
      _ <- setHeader res "Content-Type" "text/html"
      -- Use the Response object as a Stream.  Takes the stream, an encoding, the string
      _ <- writeString outputStream UTF8 html (pure unit)
      end outputStream (pure unit)
    "POST" -> void $ pipe inputStream outputStream
    -- FIXME: Add all other request types like HEAD
    _ -> unsafeCrashWith "Unexpected HTTP method"

testBasic :: forall eff. Eff (console :: CONSOLE, http :: HTTP | eff) Unit
testBasic = do
  server <- createServer handler
  listen server { hostname: "localhost", port: 8080, backlog: Nothing } $ void do
    log "Listening on port 8080."
