module Main where

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Text as Text
import qualified System.Environment as Env
import qualified System.Process as Proc

newtype ModuleName = ModuleName String
newtype OutFilePath = OutFilePath String
type BundleArgs = (Maybe ModuleName, Maybe OutFilePath)

help :: IO ()
help = putStrLn helpMessage

build :: [String] -> IO ()
build args = do
  let passthrough = List.intercalate " " args
  Proc.callCommand $ "psc-package build -- " ++ passthrough
  putStrLn "build successful."

test :: Maybe ModuleName -> IO ()
test mModuleName = do
  Proc.callCommand buildCmd
  Proc.callCommand runCmd
  putStrLn "tests succeeded."
  where
    buildCmd = "psc-package build -- 'test/**/*.purs'"
    (ModuleName moduleName) = Maybe.fromMaybe (ModuleName "Test.Main") mModuleName
    runCmd
      = Text.unpack
      . Text.replace (Text.pack "moduleName") (Text.pack moduleName)
      . Text.pack
      $ "node -e 'require(\"./output/moduleName\").main()'"

bundle :: BundleArgs -> IO ()
bundle bundleArgs = do
  let
    (ModuleName moduleName, OutFilePath targetPath) = prepareBundleDefaults bundleArgs
    cmd
      = Text.unpack
      . Text.replace (Text.pack "moduleName") (Text.pack moduleName)
      . Text.replace (Text.pack "targetPath") (Text.pack targetPath)
      . Text.pack
      $ "purs bundle './output/*/*.js' -m moduleName --main moduleName -o targetPath"
  Proc.callCommand cmd
  putStrLn $ "bundled " <> moduleName <> " to " <> targetPath

makeModule :: BundleArgs -> IO ()
makeModule bundleArgs = do
  let
    (ModuleName moduleName, OutFilePath targetPath) = prepareBundleDefaults bundleArgs
    replacePlaceholders
      = Text.unpack
      . Text.replace (Text.pack "moduleName") (Text.pack moduleName)
      . Text.replace (Text.pack "targetPath") (Text.pack targetPath)
      . Text.pack
    cmd = replacePlaceholders "purs bundle './output/*/*.js' -m moduleName -o targetPath"
    cmd2 = replacePlaceholders "echo 'module.exports = PS[\"moduleName\"];' >> targetPath"
  Proc.callCommand cmd
  Proc.callCommand cmd2
  putStrLn $ "made module " <> moduleName <> " to " <> targetPath

prepareBundleDefaults :: BundleArgs -> (ModuleName, OutFilePath)
prepareBundleDefaults (mModuleName, mTargetPath) =
  ( Maybe.fromMaybe (ModuleName "Main") mModuleName
  , Maybe.fromMaybe (OutFilePath "index.js") mTargetPath
  )

main :: IO ()
main  = do
  args <- Env.getArgs
  case args of
    [] -> help
    ["help"] -> help
    "build" : passthrough -> build passthrough
    "test" : testArgs -> test (parseTestArgs testArgs)
    "bundle" : bundleArgs -> bundle (parseBundleArgs (Nothing, Nothing) bundleArgs)
    "make-module" : bundleArgs -> makeModule (parseBundleArgs (Nothing, Nothing) bundleArgs)
    _ -> fail $ "unknown args: " <> show args

parseTestArgs :: [String] -> Maybe ModuleName
parseTestArgs ("-m" : moduleName : _) = Just (ModuleName moduleName)
parseTestArgs xs@(_ : _) = fail $ "Unknown args passed to test: " <> show xs
parseTestArgs [] = Nothing

parseBundleArgs :: BundleArgs -> [String] -> BundleArgs
parseBundleArgs (_, r) ("-m" : moduleName : xs) = parseBundleArgs (Just (ModuleName moduleName), r) xs
parseBundleArgs (l, _) ("-o" : out : xs) = parseBundleArgs (l, Just (OutFilePath out)) xs
parseBundleArgs lr (_ : xs) = parseBundleArgs lr xs
parseBundleArgs lr [] = lr

helpMessage :: String
helpMessage = "\
\Usage: purp (build | test | bundle | make-module)\n\
\\n\
\Commands:\n\
\  build [[passthrough args]]\n\
\    Build the project, with passthrough args to purp.\n\
\  test [-m Your.Test.Module.Name]\n\
\    Test the project with some module, default Test.Main.\n\
\  bundle [bundle options]\n\
\    Bundle the project, with optional main and target path arguments\n\
\  make-module [bundle options]\n\
\    Make a CommonJS module by running bundle first\n\
\Bundle options (for bundle and make-module):\n\
\  -m\n\
\    Specify main module e.g. Main\n\
\  -o\n\
\    Specify output path e.g. index.js"
