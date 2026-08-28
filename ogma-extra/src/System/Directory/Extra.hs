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
import           Control.Exception         ( Exception, IOException, catch,
                                             throwIO )
import           Control.Monad             ( forM )
import           Data.Aeson                ( Value (..) )
import           Data.Aeson.Key            ( Key, toText, toString )
import qualified Data.Aeson.KeyMap         as KeyMap
import qualified Data.ByteString.Lazy      as B
import           Data.List                 ( isInfixOf )
import           Data.Maybe                ( listToMaybe )
import           Data.Text.Lazy            ( fromStrict, pack, replace,
                                             unpack )
import           Data.Text.Lazy.Encoding   ( encodeUtf8 )
import qualified Data.Vector               as V
import           Distribution.Simple.Utils ( getDirectoryContentsRecursive )
import           System.Directory          ( createDirectoryIfMissing,
                                             doesDirectoryExist,
                                             listDirectory )
import           System.FilePath           ( takeDirectory, takeFileName,
                                             (</>) )
import           Text.Microstache          ( MustacheException (..), Template,
                                             compileMustacheFile,
                                             compileMustacheText,
                                             renderMustache )
import           Text.Parsec.Error         ( Message (..), errorMessages,
                                             errorPos )
import           Text.Parsec.Pos           ( sourceColumn, sourceLine )

import Debug.Pretty.Simple
import Text.Pretty.Simple

copyTemplate :: FilePath -> Value -> FilePath -> IO ()
copyTemplate templateDir subst targetDir = do
  tree <- getTree templateDir
  let expandedTrees = expandTree tree subst
  mapM_ (writeTree templateDir targetDir) expandedTrees

-- * Expanded trees

-- | A directory tree with variable expansion.
data ExpandedTree
  = EDir FilePath FilePath Value [ExpandedTree]
  | EFile FilePath FilePath Value
  deriving (Show)

-- | Given a template in a 'Tree' and a JSON replacement, calculate the
-- 'ExpandedTree's that it would expand to.
--
-- Because a file or directory name can mention a value that is mapped to an
-- array in JSON, and the result may be more than one tree.
expandTree :: Tree -> Value -> [ExpandedTree]
expandTree (File name) v =
  [ EFile (takeFileName name) new v'
  | (_, new, v') <- expandName (takeFileName name) v
  ]

expandTree (Dir name xs) v =
  [ EDir (takeFileName name) new v'
      (concatMap (`expandTree` v') xs)
  | (_, new, v') <- expandName (takeFileName name) v
  ]

-- | Given a filepath and a JSON replacement, calculate the FilePath
-- that it would expand to.
expandName :: FilePath -> Value -> [(FilePath, FilePath, Value)]
expandName path v = res
  where
    res = case findArray path v of
            Nothing -> [(path, render path v, v)]
            Just (k, xs) ->
              concatMap (\x ->
                expandName (removeTag k path) (promoteValue k x v)) xs

-- | Write an expanded tree from a source template directory to a target
-- directory.
writeTree :: FilePath -> FilePath -> ExpandedTree -> IO ()
writeTree src dst (EDir old new _ xs) = do
  let src' = src </> old
      dst' = dst </> new
  createDirectoryIfMissingE True dst'
  mapM_ (writeTree src' dst') xs

writeTree src dst (EFile old new v) = do
  let src' = src </> old
      dst' = dst </> new
  contents <- encodeUtf8 <$>
                (renderMustache <$> compileMustacheFileE src' <*> pure v)
  createDirectoryIfMissingE True (takeDirectory dst')
  writeFileE dst' contents

-- | Expand value in filepath using mustache template.
--
-- Does not expand arrays (and filepaths cannot iterate over arrays anyway).
render :: FilePath -> Value -> FilePath
render path v =
  either (const path)
         (unpack . (`renderMustache` v))
         (compileMustacheText "fp" (pack path))

-- | Remove a tag from a filepath.
removeTag :: Key -> FilePath -> FilePath
removeTag k =
  unpack . replace ("{{#" <> fromStrict (toText k) <> "}}") "" . pack

-- | Promote a value up in JSON
promoteValue :: Key -> Value -> Value -> Value
promoteValue k (Object x) (Object o) =
  Object $ KeyMap.union x (KeyMap.delete k o)
promoteValue k x (Object o) =
  Object $ KeyMap.insert k x (KeyMap.delete k o)
promoteValue _ _ v = v

-- | Find a key mentioned in a filepath.
findArray :: FilePath -> Value -> Maybe (Key, [Value])
findArray path (Object o) =
  listToMaybe
    [ (k, V.toList xs)
    | (k, Array xs) <- KeyMap.toList o
    -- , ("{{#" <> unpack (fromStrict (toText k)) <> "}}") `isInfixOf` path
    , ("{{#" <> toString k <> "}}") `isInfixOf` path
    ]
findArray _ _ = Nothing

-- * Directory trees

-- | Plain directory tree.
data Tree
  = Dir FilePath [Tree]
  | File FilePath
  deriving (Show)

-- | Read a directory name and return the file tree in that path.
getTree :: FilePath -> IO Tree
getTree dir = Dir dir <$> do
  names <- filter (`notElem` [".", ".."]) <$> listDirectory dir
  forM names $ \name -> do
    let path = dir </> name
    b <- doesDirectoryExist path
    if b
      then getTree path
      else pure $ File path

-- | Exception detected during the template expansion process.
newtype CopyTemplateException = CopyTemplateException String

instance Show CopyTemplateException where
  show (CopyTemplateException s) = s

instance Exception CopyTemplateException

-- | Wrap 'getDirectoryContentsRecursive' and throw any 'IOException' as a
-- 'CopyTemplateException'.
getDirectoryContentsRecursiveE :: FilePath -> IO [FilePath]
getDirectoryContentsRecursiveE s =
    catch (getDirectoryContentsRecursive s) handler
  where
    handler :: IOException -> IO [FilePath]
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

--   let subst1 = removeKeys [ "package_extra_depend", "monitors", "impl_extra_header", "testingApps", "target_extra_dependencies", "copilot", "testingVariables", "variables" ] subst
--
--       removeKeys :: [Key] -> Value -> Value
--       removeKeys keys (Object o) =
--         Object $ foldr KeyMap.delete o keys
--       removeKeys _ v = v
