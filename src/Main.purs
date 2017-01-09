module Main where

import Prelude
import Control.Monad.Eff.Ref (newRef, modifyRef, readRef)
import Node.Buffer (toString)
import Node.ChildProcess (spawn, defaultSpawnOptions, stdout)
import Node.Encoding (Encoding(..))
import Node.Stream (onData)
import Control.Monad.Eff.Console (log)

test =
    newRef "Bar" >>= \st ->
      spawn "iostat" ["2", "4"] defaultSpawnOptions >>= \proc -> do
        let dataSave buff = do
              bdata <- toString UTF8 buff
              modifyRef st \current -> current <> bdata
              --readRef st >>= \saved -> log $ "Data so far:\n" <> saved
              pure unit
        onData (stdout proc) dataSave
        pure st

main = do
  test
