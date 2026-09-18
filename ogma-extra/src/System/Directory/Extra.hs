{-# LANGUAGE OverloadedStrings #-}
-- Copyright 2020 United States Government as represented by the Administrator
-- of the National Aeronautics and Space Administration. All Rights Reserved.
--
-- Disclaimers
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may
-- not use this file except in compliance with the License. You may obtain a
-- copy of the License at
--
--      https://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.
--
-- | Auxiliary functions for working with directories.
module System.Directory.Extra
    ( copyTemplate
    , CopyTemplateException(..)
    )
  where

-- External imports
import           Control.Exception       ( Exception, IOException, catch,
                                           throwIO )
import           Control.Monad           ( forM )
import           Data.Aeson              ( Value (..) )
import qualified Data.ByteString.Lazy    as B
import           Data.List               ( isInfixOf )
import           Data.Text.Lazy          ( pack, unpack )
import           Data.Text.Lazy.Encoding ( encodeUtf8 )
import           System.Directory        ( createDirectoryIfMissing,
                                           doesDirectoryExist, listDirectory )
import           System.FilePath         ( takeDirectory, takeFileName, (</>) )
import           Text.Microstache        ( MustacheException (..), Template,
                                           compileMustacheFile,
                                           compileMustacheText, renderMustache )
import           Text.Parsec.Error       ( Message (..), errorMessages,
                                           errorPos )
import           Text.Parsec.Pos         ( sourceColumn, sourceLine )

{- HLINT ignore "Redundant <$>" -}
-- | Copy a template directory into a target location, expanding variables
-- provided in a map in a JSON value, both in the file contents and in the
-- filepaths themselves.
copyTemplate :: FilePath -> Value -> FilePath -> IO ()
copyTemplate templateDir subst targetDir = do
  -- Get all files and directories in the template dir.
  tree <- getDirectoryContentsRecursiveE templateDir
  let expansionTree = expandTree tree subst
  writeExpansionTree templateDir targetDir expansionTree

-- * Expansion trees

-- | A directory tree with variable expansion.
data ExpansionTree
  = EDir  FilePath FilePath Value [ExpansionTree]
  | EFile FilePath FilePath Value

-- | Given a template in a 'FileTree' and a JSON replacement, calculate the
-- 'ExpansionTree's that it would expand to.
expandTree :: FileTree -> Value -> ExpansionTree
expandTree (File name) value =
    EFile basename new value
  where
    basename = takeFileName name
    new      = renderMustacheS basename value

expandTree (Dir name xs) value =
    EDir basename new value (map (`expandTree` value) xs)
  where
    basename = takeFileName name
    new      = renderMustacheS basename value

-- | Write an expansion tree from a source template directory to a target
-- directory.
writeExpansionTree :: FilePath -> FilePath -> ExpansionTree -> IO ()
writeExpansionTree src dst (EDir old new _ xs) = do
  let src' = src </> old
      dst' = dst </> new
  createDirectoryIfMissingE True dst'
  mapM_ (writeExpansionTree src' dst') xs

writeExpansionTree src dst (EFile old new v) = do
  let src' = src </> old
      dst' = dst </> new
  contents <- encodeUtf8 <$>
                (renderMustache <$> compileMustacheFileE src' <*> pure v)
  createDirectoryIfMissingE True (takeDirectory dst')
  writeFileE dst' contents

-- | Exception detected during the template expansion process.
newtype CopyTemplateException = CopyTemplateException String

instance Show CopyTemplateException where
  show (CopyTemplateException s) = s

instance Exception CopyTemplateException

-- | Wrap 'getDirectoryContentsRecursive' and throw any 'IOException' as a
-- 'CopyTemplateException'.
getDirectoryContentsRecursiveE :: FilePath -> IO FileTree
getDirectoryContentsRecursiveE s =
    catch (getFileTreeRecursive s) handler
  where
    handler :: IOException -> IO FileTree
    handler e = throwIO (CopyTemplateException (show e))

-- | Wrap 'createDirectoryIfMissing' and throw any 'IOException' as a
-- 'CopyTemplateException', possibly making the error message more
-- user-friendly.
createDirectoryIfMissingE :: Bool -> FilePath -> IO ()
createDirectoryIfMissingE parents fp =
    catch (createDirectoryIfMissing parents fp) handler
  where
    handler :: IOException -> IO ()
    handler e
      | "createDirectory: permission denied" `isInfixOf` show e
      = throwIO $ CopyTemplateException $
          fp ++ ": " ++ "Error creating target directory (permission denied)"

      | otherwise
      = throwIO $ CopyTemplateException $ fp ++ ": " ++ show e

-- | Wrap 'writeFile' and throw any 'IOException' as a 'CopyTemplateException',
-- possibly making the error message more user-friendly.
writeFileE :: FilePath -> B.ByteString -> IO ()
writeFileE fp contents =
    catch (B.writeFile fp contents) handler
  where
    handler :: IOException -> IO ()
    handler e
      | "permission denied" `isInfixOf` show e
      = throwIO $ CopyTemplateException $
          fp ++ ": " ++ "Error creating target file (permission denied)"

      | "resource exhausted" `isInfixOf` show e
      = throwIO $ CopyTemplateException $
          fp ++ ": " ++ "No space left on device"

      | otherwise
      = throwIO $ CopyTemplateException $ fp ++ ": " ++ show e

-- | Wrap 'compileMustacheFile' and throw any 'IOException' or
-- 'MustacheException' as a 'CopyTemplateException', possibly making the error
-- message more user-friendly.
compileMustacheFileE :: FilePath -> IO Template
compileMustacheFileE fp = do
    catch (catch (compileMustacheFile fp) handler) handlerIO
  where
    handler :: MustacheException -> IO Template
    handler (MustacheParserException p) = do
      let pos      = errorPos p
          line     = sourceLine pos
          column   = sourceColumn pos
          messages = keepHead $ map showMessage $ errorMessages p
      throwIO $ CopyTemplateException $
        fp ++ ":" ++ show line ++ ":" ++ show column ++ ": " ++ messages

    handler e = do
      throwIO $ CopyTemplateException $ fp ++ ": " ++ show e

    handlerIO :: IOException -> IO Template
    handlerIO e
      | "hGetContents: invalid argument" `isInfixOf` show e
      = throwIO $ CopyTemplateException $
          fp ++ ": " ++ "Invalid UTF-8 byte sequence"

      | "invalid byte sequence" `isInfixOf` show e
      = throwIO $ CopyTemplateException $
          fp ++ ": " ++ "Invalid UTF-8 byte sequence"

      | "openFile: permission denied" `isInfixOf` show e
      = throwIO $ CopyTemplateException $ fp ++ ": " ++ "Permission denied"

      | otherwise
      = throwIO $ CopyTemplateException $ fp ++ ": " ++ show e

-- | Show a parse message.
showMessage :: Message -> String
showMessage (SysUnExpect s) = "Unexpected " ++ s
showMessage (UnExpect s)    = "Unexpected " ++ s
showMessage (Expect s)      = "Expected " ++ s
showMessage (Message s)     = s

-- | Keep the first element of a list of strings, returning the empty string if
-- the list is empty.
keepHead :: [String] -> String
keepHead (a:_) = a
keepHead _     = ""

-- * Auxiliary

-- ** Directory trees

-- | Plain directory tree.
data FileTree
  = Dir FilePath [FileTree]
  | File FilePath

-- | Return the file tree in a file path.
getFileTreeRecursive :: FilePath -> IO FileTree
getFileTreeRecursive path = do
 isDir <- doesDirectoryExist path
 if isDir
   then Dir path <$> do
     names <- filter (`notElem` [".", ".."]) <$> listDirectory path
     forM names $ \name -> do
       let path' = path </> name
       getFileTreeRecursive path'
   else pure $ File path

-- ** Mustache

-- | Expand value in filepath using mustache template.
--
-- Does not expand arrays (and filepaths cannot iterate over arrays anyway).
renderMustacheS :: String -> Value -> String
renderMustacheS string v =
  either (const string)
         (unpack . (`renderMustache` v))
         (compileMustacheText "fp" (pack string))
