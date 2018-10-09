{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Version as Version
import qualified Paths_purp as Paths
import qualified Turtle as T

type ModuleName = T.Text
type TargetPath = T.Text

data WithMain = WithMain | WithoutMain

-- | Commands that this program handles
data Command
  = Version
  | Build
  | Test (Maybe ModuleName)
  | Bundle WithMain (Maybe ModuleName) (Maybe TargetPath)
  | MakeModule (Maybe ModuleName) (Maybe TargetPath)

run :: Command -> IO ()
run Version = do
  T.echo . T.unsafeTextToLine . Text.pack $ Version.showVersion Paths.version

run Build = do
  let
    cmd = "psc-package build"
  code <- T.shell cmd T.empty
  case code of
    T.ExitSuccess ->
      T.echo . T.unsafeTextToLine $ "Build succeeded"
    T.ExitFailure n ->
      T.die $ "Failed to build: " <> T.repr n

run (Test mModuleName) = do
  let
    moduleName = Maybe.fromMaybe "Test.Main" mModuleName
    cmd
       = "psc-package build -- './test/**/*.purs'"
      <> "&& node -e 'require(\"./output/" <> moduleName <> "\").main()'"
  code <- T.shell cmd T.empty
  case code of
    T.ExitSuccess ->
      T.echo . T.unsafeTextToLine $ "Tests succeeded."
    T.ExitFailure n ->
      T.die $ "Tests failed: " <> T.repr n

run (Bundle withMain mModuleName mTargetPath) = do
  let
    (moduleName, targetPath) = prepareBundleDefaults mModuleName mTargetPath
    main = case withMain of
      WithMain -> "--main " <> moduleName <> " "
      WithoutMain -> ""
    cmd
       = "purs bundle './output/*/*.js' "
      <> "-m " <> moduleName <> " "
      <> main
      <> "-o " <> targetPath
  code <- T.shell cmd T.empty
  case code of
    T.ExitSuccess ->
      T.echo . T.unsafeTextToLine $ "Bundle succeeded and output file to " <> targetPath
    T.ExitFailure n ->
      T.die $ "Bundle failed: " <> T.repr n

run (MakeModule mModuleName mTargetPath) = do
  let
    (moduleName, targetPath) = prepareBundleDefaults mModuleName mTargetPath
    cmd = "echo 'module.exports = PS."<> moduleName <> ";' >> " <> targetPath
  T.echo "Bundling first..."
  run $ Bundle WithoutMain (Just moduleName) (Just targetPath)
  code <- T.shell cmd T.empty
  case code of
    T.ExitSuccess ->
      T.echo . T.unsafeTextToLine $ "Make module succeeded and output file to " <> targetPath
    T.ExitFailure n ->
      T.die $ "Make module failed: " <> T.repr n

prepareBundleDefaults mModuleName mTargetPath = (moduleName, targetPath)
  where
    moduleName = Maybe.fromMaybe "Main" mModuleName
    targetPath = Maybe.fromMaybe "index.js" mTargetPath

parser :: T.Parser Command
parser
      = build
  T.<|> test
  T.<|> bundle
  T.<|> makeModule
  T.<|> version
  where
    version = T.subcommand "version" "Print version" $ pure Version
    build = T.subcommand "build" "Build the project" $ pure Build
    test = T.subcommand "test" "Test the project with some module, default Test.Main" $ Test
        <$> T.optional (T.optText "module" 'm' "The module to target for test")
    bundle = T.subcommand "bundle" "Bundle the project, with optional main and target path arguments" $ Bundle WithMain
        <$> T.optional (T.optText "main" 'm' "The main module to bundle")
        <*> T.optional (T.optText "to" 't' "The target file path")
    makeModule = T.subcommand "make-module" "Make a CommonJS module by running bundle first" $ MakeModule
        <$> T.optional (T.optText "main" 'm' "The main module to bundle")
        <*> T.optional (T.optText "to" 't' "The target file path")

main :: IO ()
main = do
  options <- T.options "purp" parser
  run options
