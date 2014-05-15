module Data.API.MigrationTool
    ( main
    ) where

import           Data.API.Changes
import           Data.API.JSON
import           Data.API.Parse
import           Data.API.Types

import qualified Data.Aeson as JS
import qualified Data.Aeson.Encode.Pretty as JS
import qualified Data.ByteString.Lazy as BS
import           System.Environment
import           System.Exit
import           System.IO


----------------------------
-- Main, prototype testing

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["migrate", startApiFile, endApiFile, inDataFile, outDataFile] ->
       migrate startApiFile endApiFile inDataFile outDataFile

      ["compare", file1, file2] ->
       compareJSON file1 file2

      ["reformat", file1, file2] ->
       reformatJSON file1 file2

      ["parse", file] ->
       parse file

      _ -> do putStrLn "Usage: migration-tool migrate  start.api end.api start.json end.json"
              putStrLn "       migration-tool compare  file1.json file2.json"
              putStrLn "       migration-tool reformat input.json output.json"
              putStrLn "       migration-tool parse    schema.api"
              return ()

migrate :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
migrate startApiFile endApiFile
        inDataFile outDataFile = do

    (startApi, startChangelog) <- readApiFile startApiFile
    (endApi, endChangelog)     <- readApiFile endApiFile
    inData                     <- readJsonFile inDataFile
    let Release startApiVer = changelogVersion startChangelog
    case migrateDataDump (startApi, startApiVer) (endApi, DevVersion)
                         endChangelog customMigrations root CheckAll inData of
      Left err                  -> do
        hPutStrLn stderr (prettyMigrateFailure err)
        exitFailure
      Right (outData, warnings) -> do
        putStrLn . unlines . map show $ warnings
        writeJsonFile outDataFile outData

root :: TypeName
root = TypeName "DatabaseSnapshot"

readJsonFile :: FromJSONWithErrs b => FilePath -> IO b
readJsonFile  file = either (fail . prettyJSONErrorPositions) return
                   . decodeWithErrs =<< BS.readFile file

writeJsonFile :: JS.ToJSON a => FilePath -> a -> IO ()
writeJsonFile file = BS.writeFile file . JS.encodePretty

readApiFile :: FilePath -> IO APIWithChangelog
readApiFile file = fmap parseAPIWithChangelog (readFile file)

data ChangeTag = None
    deriving (Read, Show)

customMigrations :: CustomMigrations ChangeTag ChangeTag ChangeTag
customMigrations = CustomMigrations (nope JS.Object) (const noSchemaChanges)
                                    (nope id)        (const noSchemaChanges)
                                    (nope id)
  where
    nope toVal _ v = Left (CustomMigrationError "No custom migrations defined" (toVal v))

compareJSON :: FilePath -> FilePath -> IO ()
compareJSON file1 file2 = do
  js1 <- readJsonFile file1
  js2 <- readJsonFile file2
  print (js1 == (js2 :: JS.Value))

reformatJSON :: FilePath -> FilePath -> IO ()
reformatJSON file1 file2 = do
  js <- readJsonFile file1
  writeJsonFile file2 (js :: JS.Value)

parse :: FilePath -> IO ()
parse file = do
  s <- readFile file
  print (parseAPIWithChangelog s)
