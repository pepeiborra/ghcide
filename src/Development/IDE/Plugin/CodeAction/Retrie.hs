{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS -Wno-orphans #-}

module Development.IDE.Plugin.CodeAction.Retrie
  ( executeRetrieCommand,
    suggestRewrites,
    retrieCommandName,
  )
where

import           Control.Exception              (Exception (..), throwIO, try)
import           Control.Monad                  (when, forM)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Data.Aeson                     (FromJSON, ToJSON, Value (Null),
                                                 fromJSON, toJSON)
import           Data.Aeson                     (Result (Success))
import           Data.Coerce
import           Data.Functor                   ((<&>))
import qualified Data.HashMap.Strict            as HM
import           Data.IORef                     (newIORef, readIORef)
import           Data.IORef.Extra               (atomicModifyIORef'_)
import           Data.List                      (isSuffixOf)
import           Data.Maybe                     (listToMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T
import           Data.Typeable                  (Typeable)
import           Development.IDE.Core.FileStore (getFileContents)
import           Development.IDE.Core.RuleTypes (GetModIface (..),
                                                 GetModSummary (..),
                                                 GetParsedModule (..),
                                                 GhcSession (..),
                                                 HiFileResult (..))
import           Development.IDE.Core.Service   (ideLogger, runAction)
import           Development.IDE.Core.Shake     (IdeState, use, use_)
import           Development.IDE.GHC.Error      (isInsideSrcSpan,
                                                 srcSpanToRange)
import           Development.IDE.GHC.Util       (hscEnv, prettyPrint, runGhcEnv)
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger
import           GHC                            (HsBindLR (FunBind),
                                                 HsDecl (RuleD, ValD),
                                                 HsModule (..),
                                                 ParsedModule (..),
                                                 ParsedSource,
                                                 RuleDecl (HsRule),
                                                 RuleDecls (HsRules),
                                                 SrcSpan (..), fun_id,
                                                 rds_rules, srcSpanFile)
import           GHC                            (GenLocated (L), HscEnv,
                                                 ModIface_ (mi_fixities),
                                                 ModSummary (ModSummary, ms_hspp_buf, ms_mod),
                                                 parseModule)
import           GHC.Generics                   (Generic)
import           GhcPlugins                     (occNameFS, unpackFS)
import           Language.Haskell.LSP.Types     (ApplyWorkspaceEditParams (..),
                                                 Command, Command (..),
                                                 ErrorCode (InternalError, InvalidParams, ParseError),
                                                 ExecuteCommandParams (..),
                                                 List (..), ResponseError (..),
                                                 ServerMethod (WorkspaceApplyEdit),
                                                 ServerMethod, TextEdit (..),
                                                 WorkspaceEdit (..),
                                                 toNormalizedFilePath)
import           Retrie.CPP                     (CPP (NoCPP), parseCPP)
import           Retrie.ExactPrint              (Annotated, fix,
                                                 relativiseApiAnns, transformA,
                                                 unsafeMkA)
import           Retrie.Fixity                  (mkFixityEnv)
import           Retrie.Monad                   (apply, getGroundTerms,
                                                 runRetrie)
import           Retrie.Options                 (defaultOptions, getTargetFiles)
import qualified Retrie.Options                 as Retrie
import           Retrie.Replace                 (Change (..), Replacement (..))
import           Retrie.Rewrites
import           Retrie.Util                    (Verbosity (Loud))
import           StringBuffer                   (stringToStringBuffer)
import           System.Directory               (makeAbsolute)
import           System.IO                      (hPutStrLn, stderr)
import Control.Exception.Safe (catch)
import Control.Exception.Safe (SomeException)

retrieCommandName :: Text
retrieCommandName = "retrieCommand"

-- | Parameters for the runRetrie PluginCommand.
data RunRetrieParams = RunRetrieParams
  { -- | rewrites for Retrie
    rewrites        :: [RewriteSpec],
    -- | Originating file
    originatingFile :: String -- NormalizedFilePath
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

executeRetrieCommand :: a -> IdeState -> ExecuteCommandParams -> IO (Either ResponseError Value, Maybe (ServerMethod, ApplyWorkspaceEditParams))
executeRetrieCommand lsp state ExecuteCommandParams {..}
  -- _command is prefixed with a process ID, because certain clients
  -- have a global command registry, and all commands must be
  -- unique. And there can be more than one ghcide instance running
  -- at a time against the same client.
  | T.isSuffixOf retrieCommandName _command,
    Just (List [edit]) <- _arguments,
    Success wedit <- fromJSON edit =
    executeRetrieCmd lsp state wedit
  | otherwise =
    return (Right Null, Nothing)

executeRetrieCmd :: a -> IdeState -> RunRetrieParams -> IO (Either ResponseError Value, Maybe (ServerMethod, ApplyWorkspaceEditParams))
executeRetrieCmd _lsp state RunRetrieParams {..} = do
  let extendSource _ = return
  session <-
    runAction state
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
    Left err@CallRetrieInternalError {} ->
      (Left (ResponseError InternalError (T.pack $ show err) Nothing), Nothing)
    Left err@NoParse {} ->
      (Left (ResponseError ParseError (T.pack $ show err) Nothing), Nothing)
    Left err ->
      (Left (ResponseError InvalidParams (T.pack $ show err) Nothing), Nothing)
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
  | GHCParseError NormalizedFilePath String
  | NoTypeCheck NormalizedFilePath
  deriving (Eq, Typeable)

instance Show CallRetrieError where
  show (CallRetrieInternalError msg f) = msg <> " - " <> fromNormalizedFilePath f
  show (NoParse f) = "Cannot parse: " <> fromNormalizedFilePath f
  show (GHCParseError f m) = "Cannot parse " <> fromNormalizedFilePath f <> " : " <> m
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
        (fixities, pm) <- fixFixities f (fixAnns pm)
        pm <- extendSource f pm
        return (fixities, pm)
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
                `catch` \e -> throwIO (GHCParseError nt (show @SomeException e))
              (fixities, parsed) <- fixFixities f (fixAnns parsed)
              parsed <- extendSource nt parsed
              return (fixities, parsed)

        contents <- do
          (_, mbContentsVFS) <- runAction state $ getFileContents nt
          case mbContentsVFS of
            Just contents -> return contents
            Nothing       -> T.readFile (fromNormalizedFilePath nt)
        if any (T.isPrefixOf "#if" . T.toLower) (T.lines contents)
          then do
            fixitiesRef <- newIORef mempty
            let parseModule x = do
                    (fix,res) <- getParsedModule nt x
                    atomicModifyIORef'_ fixitiesRef (fix<>)
                    return res
            res <- parseCPP parseModule contents
            fixities <- readIORef fixitiesRef
            return (fixities, res)
          else do
            (fixities, pm) <- reuseParsedModule nt
            return (fixities, NoCPP pm)

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
        (\f -> NoCPP . snd <$> reuseParsedModule (findModule f))
        dummyFixityEnv -- TODO extract from GHC
        rewrites

  targets <- getTargetFiles retrieOptions (getGroundTerms retrie)

  replacements <- forM targets $ \t -> do
    log $ "Parsing module" <> T.pack t
    (fixityEnv, cpp) <- getCPPmodule t
    log $ "Transforming module" <> T.pack t
    (_user, _ast, change) <- runRetrie fixityEnv retrie cpp
    return change

  let edit = Just editParams
      editParams :: WorkspaceEdit
      editParams =
        WorkspaceEdit (Just $ HM.fromList $ asChanges replacements) Nothing

  when (null replacements) $ log "done with no edits"

  return edit
  where
    log msg = logDebug (ideLogger state) $ "[retrie] " <> msg
    useOrFail mkException rule f =
      use rule f >>= maybe (liftIO $ throwIO $ mkException f) return
    fixFixities f pm = do
      HiFileResult {hirModIface} <- runAction state $ useOrFail NoTypeCheck GetModIface f
      let fixities =
            mkFixityEnv
              [ (fs, (fs, fixity))
                | (n, fixity) <- mi_fixities hirModIface,
                  let fs = occNameFS n
              ]
      res <- transformA pm (fix fixities)
      return (fixities, res)
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
            let spanLoc = T.pack $ unpackFS $ srcSpanFile rspan,
            let edit = TextEdit (srcSpanToRange s) (T.pack replReplacement)
        ]

-------------------------------------------------------------------------------

deriving instance Eq RewriteSpec

deriving instance Show RewriteSpec

deriving instance Generic RewriteSpec

deriving instance FromJSON RewriteSpec

deriving instance ToJSON RewriteSpec
