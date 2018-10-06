{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Turtle as T

-- | Commands that this program handles
data Command
  = Hello

hello :: IO ()
hello = do
  putStrLn "hello"

parser :: T.Parser Command
parser
      = Hello <$ localSetup
  where
    localSetup = T.subcommand "hello" "hello" $ pure ()

main :: IO ()
main = do
  options <- T.options "purp" parser
  case options of
    Hello -> hello
