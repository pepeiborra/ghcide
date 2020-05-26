{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -Wno-orphans #-}

module Development.IDE.Plugin.CodeAction.Retrie
  (executeRetrieCommand, suggestRewrites
  , retrieCommandName
  ) where

import Control.Exception (try, Exception(..), throwIO)
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Coerce
import qualified Data.HashMap.Strict as HM
import Data.List (isSuffixOf)
import Data.Maybe (listToMaybe)
import qualified Data.Text as Text
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Development.IDE.Core.FileStore (getFileContents)
import Development.IDE.Core.RuleTypes (GetModIface (..), GetModSummary (..), GhcSession (..), HiFileResult (..))
import Development.IDE.Core.Rules
import Development.IDE.Core.Shake (use, use_)
import Development.IDE.GHC.Error
  ( isInsideSrcSpan,
    srcSpanToRange,
  )
import Development.IDE.GHC.Util (hscEnv, prettyPrint, runGhcEnv)
import Development.IDE.Types.Location
import GHC
  ( HsBindLR (FunBind),
    HsDecl (RuleD, ValD),
    HsModule (..),
    ParsedModule (..),
    ParsedSource,
    RuleDecl (HsRule),
    RuleDecls (HsRules),
    SrcSpan (..),
    fun_id,
    rds_rules,
    srcSpanFile,
  )
import GHC (ModSummary (ms_hspp_buf))
import GHC (parseModule)
import GHC (HscEnv)
import GHC (GenLocated (L))
import GHC (ModSummary (ModSummary))
import GHC (ModSummary (ms_mod))
import GHC (ModIface_ (mi_fixities))
import GHC.Generics (Generic)
import GhcPlugins (occNameFS, unpackFS)
import Language.Haskell.LSP.Types as J
import Retrie.CPP (CPP (NoCPP), parseCPP)
import Retrie.ExactPrint
  ( Annotated,
    fix,
    relativiseApiAnns,
    transformA,
    unsafeMkA,
  )
import Retrie.Fixity (mkFixityEnv)
import Retrie.Monad
  ( apply,
    getGroundTerms,
    runRetrie,
  )
import Retrie.Options (defaultOptions, getTargetFiles)
import qualified Retrie.Options as Retrie
import Retrie.Replace (Change (..), Replacement (..))
import Retrie.Rewrites
import Retrie.Util (Verbosity (Loud))
import StringBuffer (stringToStringBuffer)
import System.Directory (makeAbsolute)
import Data.Functor ((<&>))
import Data.Aeson (toJSON, Value(Null), ToJSON, FromJSON, fromJSON)
import Data.Aeson (Result(Success))
import Data.Text (Text)
import System.IO (stderr, hPutStrLn)

retrieCommandName :: Text
retrieCommandName = "retrieCommand"

-- | Parameters for the runRetrie PluginCommand.
data RunRetrieParams = RunRetrieParams
  { -- | rewrites for Retrie
    rewrites :: [RewriteSpec],
    -- | Originating file
    originatingFile :: String -- NormalizedFilePath
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

executeRetrieCommand :: a -> IdeState -> ExecuteCommandParams -> IO (Either ResponseError Value, Maybe (ServerMethod, ApplyWorkspaceEditParams))
executeRetrieCommand lsp state ExecuteCommandParams{..}
    -- _command is prefixed with a process ID, because certain clients
    -- have a global command registry, and all commands must be
    -- unique. And there can be more than one ghcide instance running
    -- at a time against the same client.
    | T.isSuffixOf retrieCommandName _command
    , Just (List [edit]) <- _arguments
    , Success wedit <- fromJSON edit
    = executeRetrieCmd lsp state wedit
    | otherwise
    = return (Right Null, Nothing)

executeRetrieCmd :: a -> IdeState -> RunRetrieParams -> IO (Either ResponseError Value, Maybe (ServerMethod, ApplyWorkspaceEditParams))
executeRetrieCmd _lsp state RunRetrieParams {..} = do
  let extendSource _ = return
  session <-
    runAction  state
      $ use_ GhcSession
      $ toNormalizedFilePath originatingFile
  edits <-
    callRetrie
      extendSource
      state
      (hscEnv session)
      rewrites
      (toNormalizedFilePath originatingFile)
  return $ case edits of
    Left err@CallRetrieInternalError{} ->
      ( Left (ResponseError InternalError (T.pack $ show err) Nothing), Nothing)
    Left err@NoParse{} ->
      ( Left (ResponseError ParseError (T.pack $ show err) Nothing), Nothing)
    Left err ->
      ( Left (ResponseError InvalidParams (T.pack $ show err) Nothing), Nothing)
    Right edits ->
      ( Right Null,
        ((WorkspaceApplyEdit,) . ApplyWorkspaceEditParams <$> edits)
      )

-------------------------------------------------------------------------------

suggestRewrites :: String -> Range -> ParsedModule -> [Command]
suggestRewrites nfp range pm = commands
  where
      ParsedModule {pm_mod_summary, pm_parsed_source} = pm
      L _ HsModule {hsmodDecls} = pm_parsed_source
      ModSummary {ms_mod} = pm_mod_summary

      pos = _start range -- TODO actions for selections

      topLevel =
        listToMaybe
          [ decl
            | L l decl <- hsmodDecls,
              pos `isInsideSrcSpan` l
          ]
      qualify x = prettyPrint ms_mod <> "." <> x
      results = case topLevel of
        Just (RuleD _ HsRules {rds_rules}) ->
          concat
            [ [ let rewrites =
                      [RuleForward (qualify ruleName)]
                 in ( "Apply rule " <> T.pack ruleName <> " forward",
                      RunRetrieParams {..}
                    ),
                let rewrites =
                      [RuleBackward (qualify ruleName)]
                 in ( "Apply rule " <> T.pack ruleName <> " backwards",
                      RunRetrieParams {..}
                    )
              ]
              | L _ (HsRule _ (L _ (_, rn)) _ _ _ _ _) <- rds_rules,
                let ruleName = unpackFS rn,
                let originatingFile = nfp
            ]
        Just (ValD _ FunBind {fun_id = L l' rdrName})
          | pos `isInsideSrcSpan` l' ->
            let pprName = prettyPrint rdrName
                pprNameText = T.pack pprName
                originatingFile = nfp
             in [ let rewrites = [Unfold (qualify pprName)]
                   in ("Unfold " <> pprNameText, RunRetrieParams {..}),
                  let rewrites = [Fold (qualify pprName)]
                   in ("Fold " <> pprNameText, RunRetrieParams {..})
                ]
        _ -> []
      commands = results <&> \(title, params) ->
        Command title retrieCommandName (Just $ List [toJSON params])

-------------------------------------------------------------------------------
data CallRetrieError
  = CallRetrieInternalError String NormalizedFilePath
  | NoParse NormalizedFilePath
  | NoTypeCheck NormalizedFilePath
  deriving (Eq, Typeable)

instance Show CallRetrieError where
  show (CallRetrieInternalError msg f) = msg <> " - " <> fromNormalizedFilePath f
  show (NoParse f) = "Cannot parse: " <> fromNormalizedFilePath f
  show (NoTypeCheck f) = "File does not typecheck: " <> fromNormalizedFilePath f

instance Exception CallRetrieError

callRetrie ::
  ( NormalizedFilePath ->
    Annotated ParsedSource ->
    IO (Annotated ParsedSource)
  ) ->
  IdeState ->
  HscEnv ->
  [RewriteSpec] ->
  NormalizedFilePath ->
  IO (Either CallRetrieError (Maybe WorkspaceEdit))
callRetrie extendSource state session rewrites origin = try $ do
  let reuseParsedModule f = do
        pm <-
          runAction state $
            useOrFail NoParse GetParsedModule f
        fixFixities f (fixAnns pm)
      getCPPmodule t = do
        nt <- toNormalizedFilePath' <$> makeAbsolute t
        let getParsedModule f contents = do
              modSummary <-
                runAction state $
                  useOrFail (CallRetrieInternalError "file not found") GetModSummary nt
              let ms' =
                    modSummary
                      { ms_hspp_buf =
                          Just (stringToStringBuffer contents)
                      }
              (_, parsed) <- runGhcEnv session (parseModule ms')
              extendSource nt =<< fixFixities f (fixAnns parsed)

        (_, mbContents) <-
          runAction state $ getFileContents nt

        forM mbContents $ \case
          contents
            | any (Text.isPrefixOf "#") (T.lines contents) ->
              parseCPP (getParsedModule nt) contents
            | otherwise -> do
              pm <- reuseParsedModule nt
              pure $ NoCPP pm

      -- retrie gives us an incomplete module path, e.g. 'Ide/Plugin/Retrie.hs'
      -- we'd need to look for it in all source folders, e.g. 'src/Ide/Plugin/Retrie.hs'
      -- but actually, we know it's going to be the origin module
      findModule filepath
        | filepath `isSuffixOf` fromNormalizedFilePath origin = origin
        | otherwise = error $ "findModule " <> filepath

  let dummyFixityEnv = mkFixityEnv []
      target = "." -- TODO cover all workspaceFolders
      retrieOptions :: Retrie.Options
      retrieOptions = (defaultOptions target) {Retrie.verbosity = Loud}

  retrie <-
    apply
      <$> parseRewriteSpecs
        (\f -> NoCPP <$> reuseParsedModule (findModule f))
        dummyFixityEnv
        rewrites

  targets <- getTargetFiles retrieOptions (getGroundTerms retrie)

  replacements <- forM targets $ \t -> do
    cpp <- getCPPmodule t
    case cpp of
      Nothing -> return NoChange
      Just cpp -> do
        let fixityEnv = mkFixityEnv [] -- TODO extract from GHC?
        (_user, _ast, change) <- runRetrie fixityEnv retrie cpp
        return change

  let edit = Just editParams
      editParams :: WorkspaceEdit
      editParams =
        WorkspaceEdit (Just $ HM.fromList $ asChanges replacements) Nothing

  hPutStrLn stderr $ "Edits returned by retrie: " <> show edit

  return edit
  where
    useOrFail mkException rule f =
      use rule f >>= maybe (liftIO $ throwIO $ mkException f) return
    fixFixities f pm = do
        HiFileResult {hirModIface} <- runAction state $ useOrFail NoTypeCheck GetModIface f
        let fixities = mkFixityEnv
                [ (fs, (fs, fixity))
                | (n, fixity) <- mi_fixities hirModIface,
                    let fs = occNameFS n
                ]
        transformA pm (fix fixities)
    fixAnns ParsedModule {..} =
      let ranns = relativiseApiAnns pm_parsed_source pm_annotations
       in unsafeMkA pm_parsed_source ranns 0

asChanges :: [Change] -> [(Uri, List TextEdit)]
asChanges cc = coerce $ HM.toList byModule
  where
    byModule =
      HM.fromListWith
        (++)
        [ (Uri spanLoc, [edit])
          | Change reps _ <- cc,
            Replacement {..} <- reps,
            s@(RealSrcSpan rspan) <- [replLocation],
            let spanLoc = Text.pack $ unpackFS $ srcSpanFile rspan,
            let edit = TextEdit (srcSpanToRange s) (Text.pack replReplacement)
        ]

-------------------------------------------------------------------------------

deriving instance Eq RewriteSpec

deriving instance Show RewriteSpec

deriving instance Generic RewriteSpec

deriving instance FromJSON RewriteSpec

deriving instance ToJSON RewriteSpec

