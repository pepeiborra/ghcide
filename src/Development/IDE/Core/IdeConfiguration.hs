{-# LANGUAGE DuplicateRecordFields #-}
module Development.IDE.Core.IdeConfiguration where

import           Data.HashSet               (HashSet, singleton)
import           Data.Text                  (Text, isPrefixOf)
import           Development.IDE.Core.Shake
import           Development.IDE.Types.Location
import           Development.Shake
import           Language.Haskell.LSP.Types

-- | Lsp client relevant configuration details
data IdeConfiguration = IdeConfiguration
  { workspaceFolders :: HashSet NormalizedUri
  }
  deriving (Show)

instance IsIdeGlobal IdeConfiguration

getIdeConfiguration :: Action IdeConfiguration
getIdeConfiguration = getIdeGlobalAction

parseConfiguration :: InitializeRequest -> IdeConfiguration
parseConfiguration RequestMessage { _params = InitializeParams {..} } =
  IdeConfiguration { .. }
 where
  workspaceFolders =
    foldMap (singleton . toNormalizedUri) _rootUri
      <> (foldMap . foldMap)
           ( singleton
           . toNormalizedUri
           . Uri
           . (_uri :: WorkspaceFolder -> Text)
           )
           _workspaceFolders

isWorkspaceFile :: NormalizedFilePath -> Action Bool
isWorkspaceFile file = do
  IdeConfiguration{..} <- getIdeConfiguration
  let toText = getUri . fromNormalizedUri
  return $ any (\root -> toText root `isPrefixOf` toText (filePathToUri' file)) workspaceFolders