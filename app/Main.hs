{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Maybe as Maybe
import qualified Turtle as T

type ModuleName = T.Text
type TargetPath = T.Text

-- | Commands that this program handles
data Command
  = Build
  | Test
  | Bundle (Maybe ModuleName) (Maybe TargetPath)

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

run (Bundle mModuleName mTargetPath) = do
  let
    moduleName = Maybe.fromMaybe "Main" mModuleName
    targetPath = Maybe.fromMaybe "index.js" mTargetPath
    cmd
       = "purs bundle './output/*/*.js' "
      <> "-m " <> moduleName <> " "
      <> "--main " <> moduleName <> " "
      <> "-o " <> targetPath
  code <- T.shell cmd T.empty
  case code of
    T.ExitSuccess ->
      T.echo . T.unsafeTextToLine $ "Bundle succeeded and output file to " <> targetPath
    T.ExitFailure n ->
      T.die $ "Bundle failed: " <> T.repr n

parser :: T.Parser Command
parser
      = build
  T.<|> test
  T.<|> bundle
  where
    build = T.subcommand "build" "Build the project." $ pure Build
    test = T.subcommand "test" "Test the project with Test.Main." $ pure Test
    bundle = T.subcommand "bundle" "Bundle the project, with optional main and target path arguments." $ Bundle
        <$> T.optional (T.optText "main" 'm' "The main module to bundle")
        <*> T.optional (T.optText "to" 't' "The target file path")

main :: IO ()
main = do
  options <- T.options "purp" parser
  run options
