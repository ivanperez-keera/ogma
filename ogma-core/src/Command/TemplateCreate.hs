{-# LANGUAGE OverloadedStrings #-}

module Command.TemplateCreate where

import           Control.Monad
import           Data.Aeson           ( decode )
import qualified Data.ByteString.Lazy as B
import qualified Data.Map             as M
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           System.Directory
import           System.Environment
import           System.FilePath

import Command.Errors ( ErrorCode, ErrorTriplet (..) )
import Command.Result ( Result (..) )

import Data.Location  (Location (..))

data CommandOpts = CommandOpts
  { templateCreateSource      :: FilePath
  , templateCreateReplacement :: FilePath
  , templateCreateTargetDir   :: FilePath
  }

command :: CommandOpts -> IO (Result ErrorCode)
command c = do
  execute
    (templateCreateReplacement c)
    (templateCreateSource c)
    (templateCreateTargetDir c)

execute :: FilePath -> FilePath -> FilePath -> IO (Result ErrorCode)
execute json dir target = do
  mM <- decode <$> B.readFile json :: IO (Maybe (M.Map T.Text T.Text))
  case mM of
    Nothing -> return $ Error ecTemplateCreateError wrongJSON LocationNothing
    Just m  -> do
      dir'  <- makeAbsolute dir
      files <- getAllRecursiveFiles dir
      mapM_ (processFile m dir target) files
      return Success

-- | Copy a file to a destination applying a substitution to it.
processFile :: M.Map T.Text T.Text -> FilePath -> FilePath -> FilePath -> IO ()
processFile m dir target path = do
    createDirectoryIfMissing True targetDir
    T.readFile path >>= T.writeFile targetPath . rep m
  where
    e          = takeFileName path
    subdir     = makeRelative dir (takeDirectory path)
    targetDir  = target </> subdir
    targetPath = targetDir </> repStr m e

-- * Error codes

-- | Error: the JSON file cannot be read due to it being unreadable or the
-- format being incorrect.
ecTemplateCreateError :: ErrorCode
ecTemplateCreateError = 1

wrongJSON :: String
wrongJSON = "Wrong JSON"

-- * Auxiliary functions

concatMapM f l = concat <$> mapM f l

repStr :: M.Map T.Text T.Text -> String -> String
repStr m = T.unpack . rep m . T.pack

rep :: M.Map T.Text T.Text -> T.Text -> T.Text
rep m = foldr (.) id [T.replace k v | (k,v) <- M.toList m]

getAllRecursiveFiles :: FilePath -> IO [FilePath]
getAllRecursiveFiles dir = do
  putStrLn $ "Checking: " ++ dir
  isDir <- doesDirectoryExist dir
  if not isDir
    then return [dir]
    else do
      entries <- listDirectory dir
      concatMapM getAllRecursiveFiles $ map (dir </>) entries
