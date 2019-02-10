module Test.Main where

import Prelude

import Effect
import Effect.Console
import Main (apple)

main = do
  if apple == "apple"
    then log "yes"
    else log "no"
