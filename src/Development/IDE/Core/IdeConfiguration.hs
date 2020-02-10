{-# LANGUAGE DuplicateRecordFields #-}
module Development.IDE.Core.IdeConfiguration
  ( IdeConfiguration(..)
  , registerIdeConfiguration
  , parseConfiguration
  , parseWorkspaceFolder
  , isWorkspaceFile
  , modifyWorkspaceFolders
  )
where

import           Control.Concurrent.Extra
import           Control.Monad
import           Data.HashSet                   (HashSet, singleton)
import           Data.Text                      (Text, isPrefixOf)
import           Development.IDE.Core.Shake
import           Development.IDE.Types.Location
import           Development.Shake
import           Language.Haskell.LSP.Types

-- | Lsp client relevant configuration details
data IdeConfiguration = IdeConfiguration
  { workspaceFolders :: HashSet Uri
  }
  deriving (Show)

newtype IdeConfigurationVar = IdeConfigurationVar {unIdeConfigurationRef :: Var IdeConfiguration}

instance IsIdeGlobal IdeConfigurationVar

registerIdeConfiguration :: ShakeExtras -> IdeConfiguration -> IO ()
registerIdeConfiguration extras =
  addIdeGlobalExtras extras . IdeConfigurationVar <=< newVar

getIdeConfiguration :: Action IdeConfiguration
getIdeConfiguration =
  getIdeGlobalAction >>= liftIO . readVar . unIdeConfigurationRef

parseConfiguration :: InitializeRequest -> IdeConfiguration
parseConfiguration RequestMessage { _params = InitializeParams {..} } =
  IdeConfiguration { .. }
 where
  workspaceFolders =
    foldMap singleton _rootUri
      <> (foldMap . foldMap)
           (singleton . parseWorkspaceFolder)
           _workspaceFolders

parseWorkspaceFolder :: WorkspaceFolder -> Uri
parseWorkspaceFolder =
   Uri . (_uri :: WorkspaceFolder -> Text)

modifyWorkspaceFolders
  :: IdeState -> (HashSet Uri -> HashSet Uri) -> IO ()
modifyWorkspaceFolders ide f = do
  IdeConfigurationVar var <- getIdeGlobalState ide
  IdeConfiguration    ws  <- readVar var
  writeVar var (IdeConfiguration (f ws))

isWorkspaceFile :: NormalizedFilePath -> Action Bool
isWorkspaceFile file = do
  IdeConfiguration {..} <- getIdeConfiguration
  return $ any
    (\root -> getUri root `isPrefixOf` getUri (fromNormalizedUri $ filePathToUri' file))
    workspaceFolders
