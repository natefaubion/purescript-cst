module Main where

import Prelude
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.MVar
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_, foldl', find)
import qualified Data.Text.IO as IO
import qualified Language.PureScript.CST.Convert as CST
import qualified Language.PureScript.CST.Lexer as CST
import qualified Language.PureScript.CST.Monad as CST
import qualified Language.PureScript.CST.Parser as CST
import qualified Language.PureScript.CST.Print as CST
import qualified Language.PureScript.Errors as Errs
import qualified Language.PureScript.Make as Make
import qualified Language.PureScript.Names as Names
import qualified Language.PureScript.Parser.Declarations as Parser
import Language.PureScript.Options (defaultOptions)
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import System.FilePath.Glob (glob)
import System.IO
import Text.Pretty.Simple (pPrint)

actions :: MVar () -> Make.MakeActions Make.Make
actions var = Make.MakeActions
  { getInputTimestamp = const (pure (Left Make.RebuildAlways))
  , getOutputTimestamp = const (pure Nothing)
  , readExterns = error "readExterns not implemented"
  , codegen = \_ _ _ -> pure ()
  , ffiCodegen = \_ -> pure ()
  , progress = \(Make.CompilingModule md) ->
      liftIO . withMVar var $ \_ ->
        IO.putStrLn $ "Compiling " <> Names.runModuleName md
  }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  filePaths <- case find ((/= '-') . head) args of
    Nothing -> die "File glob required."
    Just gs -> do
      paths <- glob gs
      when (null paths) $ die "Glob did not match any files."
      pure paths

  mbModules <- forConcurrently filePaths $ \path -> do
    src <- IO.readFile path
    let
      cst = CST.parse src
      res = (\a -> (path, src, a, CST.convertModule path a)) <$> cst
    pure (path, src, res)

  let
    modules = foldl' go id mbModules $ Right []
    go k = \case
      (path, src, Left errs) -> do
        let errs' = (path, src,) <$> errs
        \case
          Left errs'' -> k $ Left (errs' <> errs'')
          Right _ -> k $ Left errs'
      (_, _, Right md) ->
        \case
          Left errs'' -> k $ Left errs''
          Right mds -> k $ Right (md : mds)

  case modules of
    Left errs ->
      for_ errs $ \(path, _, err) ->
        putStrLn $ ex <> " " <> path <> " " <> CST.prettyPrintError err
    Right ms ->
      if "--make" `elem` args then do
        var <- newMVar ()
        res <- Make.runMake defaultOptions $ Make.make (actions var) ((\(_, _, _, a) -> a) <$> ms)
        case res of
          (Left errs, _) -> do
            hPutStrLn stderr (Errs.prettyPrintMultipleErrors Errs.defaultPPEOptions errs)
            exitFailure
          (Right _, _) -> do
            putStrLn $ check <> " OK"
      else if "--cst" `elem` args then
        for_ ms $ \(path, _, a, _) -> do
          putStrLn $ check <> " " <> path
          pPrint a
      else if "--ast" `elem` args then
        for_ ms $ \(path, _, _, b) -> do
          putStrLn $ check <> " " <> path
          pPrint b
      else if "--old" `elem` args then
        for_ ms $ \(path, src, _, _) ->
          case Parser.parseModuleFromFile id (path, src) of
            Left err -> putStrLn $ show err
            Right (_, b) -> do
              putStrLn $ check <> " " <> path
              pPrint b
      else
        for_ ms $ \(path, _, _, _) -> do
          putStrLn $ check <> " " <> path

check :: String
check = "\x1b[32m✓\x1b[0m"

ex :: String
ex = "\x1b[31m✗\x1b[0m"
