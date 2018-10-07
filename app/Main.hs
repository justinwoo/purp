{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Turtle as T

-- | Commands that this program handles
data Command
  = Build
  | Test

run :: Command -> IO ()
run Build = do
  let
    cmd = "psc-package build"
  code <- T.shell cmd T.empty
  case code of
    T.ExitSuccess ->
      T.echo . T.unsafeTextToLine $ "Build succeeded"
    T.ExitFailure n ->
      T.die $ "Failed to build: " <> T.repr n

run Test = do
  let
    cmd
       = "psc-package build -- "
      <> "test/Main.purs"
      <> "&& node -e 'require(\"./output/Test.Main\").main()'"
  code <- T.shell cmd T.empty
  case code of
    T.ExitSuccess ->
      T.echo . T.unsafeTextToLine $ "Tests succeeded."
    T.ExitFailure n ->
      T.die $ "Tests failed: " <> T.repr n

parser :: T.Parser Command
parser
      = Build <$ build
  T.<|> Test <$ test
  where
    build = T.subcommand "build" "build" $ pure ()
    test = T.subcommand "test" "test" $ pure ()

main :: IO ()
main = do
  options <- T.options "purp" parser
  run options
