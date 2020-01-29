-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms       #-}

-- | A Shake implementation of the compiler service, built
--   using the "Shaker" abstraction layer for in-memory use.
--
module Development.IDE.Core.Rules(
    IdeState, GetDependencies(..), GetParsedModule(..), TransitiveDependencies(..),
    Priority(..),
    priorityTypeCheck,
    priorityGenerateCore,
    priorityFilesOfInterest,
    runAction, useE, useNoFileE, usesE,
    toIdeResult, defineNoFile,
    mainRule,
    getAtPoint,
    getDefinition,
    getDependencies,
    getParsedModule,
    generateCore,
    ) where

import Fingerprint

import Data.Binary
import Data.Bifunctor (second)
import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Development.IDE.Core.Compile
import Development.IDE.Core.OfInterest
import Development.IDE.Types.Options
import Development.IDE.Spans.Calculate
import Development.IDE.Import.DependencyInformation
import Development.IDE.Import.FindImports
import           Development.IDE.Core.FileExists
import           Development.IDE.Core.FileStore        (getFileContents)
import           Development.IDE.Types.Diagnostics
import Development.IDE.Types.Location
import Development.IDE.GHC.Compat as GHC hiding (parseModule, typecheckModule)
import Development.IDE.GHC.Util
import Data.Coerce
import Data.Either.Extra
import Data.Maybe
import           Data.Foldable
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List
import qualified Data.Set                                 as Set
import qualified Data.Text                                as T
import           Development.IDE.GHC.Error
import           Development.Shake                        hiding (Diagnostic)
import Development.IDE.Core.RuleTypes
import Development.IDE.Types.Logger (logDebug)
import Development.IDE.Spans.Type

import qualified GHC.LanguageExtensions as LangExt
import HscTypes
import DynFlags (xopt)
import GHC.Generics(Generic)

import qualified Development.IDE.Spans.AtPoint as AtPoint
import Development.IDE.Core.Service
import Development.IDE.Core.Shake
import Development.Shake.Classes

-- | This is useful for rules to convert rules that can only produce errors or
-- a result into the more general IdeResult type that supports producing
-- warnings while also producing a result.
toIdeResult :: Either [FileDiagnostic] v -> IdeResult v
toIdeResult = either (, Nothing) (([],) . Just)

-- | useE is useful to implement functions that aren’t rules but need shortcircuiting
-- e.g. getDefinition.
useE :: IdeRule k v => k -> NormalizedFilePath -> MaybeT Action v
useE k = MaybeT . use k

useNoFileE :: IdeRule k v => k -> MaybeT Action v
useNoFileE k = useE k ""

usesE :: IdeRule k v => k -> [NormalizedFilePath] -> MaybeT Action [v]
usesE k = MaybeT . fmap sequence . uses k

defineNoFile :: IdeRule k v => (k -> Action v) -> Rules ()
defineNoFile f = define $ \k file -> do
    if file == "" then do res <- f k; return ([], Just res) else
        fail $ "Rule " ++ show k ++ " should always be called with the empty string for a file"


------------------------------------------------------------
-- Exposed API

-- | Get all transitive file dependencies of a given module.
-- Does not include the file itself.
getDependencies :: NormalizedFilePath -> Action (Maybe [NormalizedFilePath])
getDependencies file = fmap transitiveModuleDeps <$> use GetDependencies file

-- | Try to get hover text for the name under point.
getAtPoint :: NormalizedFilePath -> Position -> Action (Maybe (Maybe Range, [T.Text]))
getAtPoint file pos = fmap join $ runMaybeT $ do
  opts <- lift getIdeOptions
  spans <- useE GetSpanInfo file
  return $ AtPoint.atPoint opts spans pos

-- | Goto Definition.
getDefinition :: NormalizedFilePath -> Position -> Action (Maybe Location)
getDefinition file pos = fmap join $ runMaybeT $ do
    opts <- lift getIdeOptions
    spans <- useE GetSpanInfo file
    lift $ AtPoint.gotoDefinition (getHieFile opts file) opts (spansExprs spans) pos

getHieFile
  :: IdeOptions
  -> NormalizedFilePath
  -> Module
  -> Action (Maybe (HieFile, FilePath))
getHieFile IdeOptions {..} file mod = do
  (deps, _) <- use_ GetLocatedImports file
  pkgState  <- hscEnv <$> use_ GhcSession file
  case find (\(L _ x, _) -> x == moduleName mod) deps of
    Just (_, Just (ArtifactsLocation ml)) ->
      case (ml_hie_file ml, ml_hs_file ml) of
        (hiePath, Just modPath) -> do
          hieFile <- use_ (GetHieFile hiePath) (toNormalizedFilePath modPath)
          return $ Just (hieFile, modPath)
        _ -> return Nothing
    _ -> do
      let unitId = moduleUnitId mod
      case lookupPackageConfig unitId pkgState of
        Just pkgConfig -> do
          hieFile <- liftIO $ optLocateHieFile optPkgLocationOpts pkgConfig mod
          path    <- liftIO $ optLocateSrcFile optPkgLocationOpts pkgConfig mod
          case (hieFile, path) of
            (Just hiePath, Just modPath) -> do
              hieFile <- useNoFile (GetPackageHieFile hiePath)
              return $ (, modPath) <$> hieFile
            _ -> return Nothing
        _ -> return Nothing

-- | Parse the contents of a daml file.
getParsedModule :: NormalizedFilePath -> Action (Maybe ParsedModule)
getParsedModule file = use GetParsedModule file

------------------------------------------------------------
-- Rules
-- These typically go from key to value and are oracles.

priorityTypeCheck :: Priority
priorityTypeCheck = Priority 0

priorityGenerateCore :: Priority
priorityGenerateCore = Priority (-1)

priorityFilesOfInterest :: Priority
priorityFilesOfInterest = Priority (-2)

getParsedModuleRule :: Rules ()
getParsedModuleRule =
    defineEarlyCutoff $ \GetParsedModule file -> do
        (_, contents) <- getFileContents file
        packageState <- hscEnv <$> use_ GhcSession file
        opt <- getIdeOptions
        (diag, res) <- liftIO $ parseModule opt packageState (fromNormalizedFilePath file) contents
        case res of
            Nothing -> pure (Nothing, (diag, Nothing))
            Just (contents, modu) -> do
                mbFingerprint <- if isNothing $ optShakeFiles opt
                    then pure Nothing
                    else liftIO $ Just . fingerprintToBS <$> fingerprintFromStringBuffer contents
                pure (mbFingerprint, (diag, Just modu))

getLocatedImportsRule :: Rules ()
getLocatedImportsRule =
    define $ \GetLocatedImports file -> do
        pm <- use_ GetParsedModule file
        let ms = pm_mod_summary pm
        let imports = [(False, imp) | imp <- ms_textual_imps ms] ++ [(True, imp) | imp <- ms_srcimps ms]
        env <- hscEnv <$> use_ GhcSession file
        let dflags = addRelativeImport file pm $ hsc_dflags env
        opt <- getIdeOptions
        (diags, imports') <- fmap unzip $ forM imports $ \(isSource, (mbPkgName, modName)) -> do
            diagOrImp <- locateModule dflags (optExtensions opt) getFileExists modName mbPkgName isSource
            case diagOrImp of
                Left diags -> pure (diags, Left (modName, Nothing))
                Right (FileImport path) -> pure ([], Left (modName, Just $ path))
                Right (PackageImport pkgId) -> liftIO $ do
                    diagsOrPkgDeps <- computePackageDeps env pkgId
                    case diagsOrPkgDeps of
                        Left diags -> pure (diags, Right Nothing)
                        Right pkgIds -> pure ([], Right $ Just $ pkgId : pkgIds)
        let (moduleImports, pkgImports) = partitionEithers imports'
        case sequence pkgImports of
            Nothing -> pure (concat diags, Nothing)
            Just pkgImports -> pure (concat diags, Just (moduleImports, Set.fromList $ concat pkgImports))


-- | Given a target file path, construct the raw dependency results by following
-- imports recursively.
rawDependencyInformation :: NormalizedFilePath -> Action RawDependencyInformation
rawDependencyInformation f = do
    let (initialId, initialMap) = getPathId (ArtifactsLocation $ ModLocation (Just $ fromNormalizedFilePath f) "" "") emptyPathIdMap
    go (IntSet.singleton $ getFilePathId initialId)
       (RawDependencyInformation IntMap.empty initialMap)
  where
    go fs rawDepInfo =
        case IntSet.minView fs of
            -- Queue is empty
            Nothing -> pure rawDepInfo
            -- Pop f from the queue and process it
            Just (f, fs) -> do
                let fId = FilePathId f
                importsOrErr <- use GetLocatedImports $ idToPath (rawPathIdMap rawDepInfo) fId
                case importsOrErr of
                  Nothing ->
                    -- File doesn’t parse
                    let rawDepInfo' = insertImport fId (Left ModuleParseError) rawDepInfo
                    in go fs rawDepInfo'
                  Just (modImports, pkgImports) -> do
                    let f :: PathIdMap -> (a, Maybe ArtifactsLocation) -> (PathIdMap, (a, Maybe FilePathId))
                        f pathMap (imp, mbPath) = case mbPath of
                            Nothing -> (pathMap, (imp, Nothing))
                            Just path ->
                                let (pathId, pathMap') = getPathId path pathMap
                                in (pathMap', (imp, Just pathId))
                    -- Convert paths in imports to ids and update the path map
                    let (pathIdMap, modImports') = mapAccumL f (rawPathIdMap rawDepInfo) modImports
                    -- Files that we haven’t seen before are added to the queue.
                    let newFiles =
                            IntSet.fromList (coerce $ mapMaybe snd modImports')
                            IntSet.\\ IntMap.keysSet (rawImports rawDepInfo)
                    let rawDepInfo' = insertImport fId (Right $ ModuleImports modImports' pkgImports) rawDepInfo
                    go (newFiles `IntSet.union` fs) (rawDepInfo' { rawPathIdMap = pathIdMap })

getDependencyInformationRule :: Rules ()
getDependencyInformationRule =
    define $ \GetDependencyInformation file -> do
       rawDepInfo <- rawDependencyInformation file
       pure ([], Just $ processDependencyInformation rawDepInfo)

reportImportCyclesRule :: Rules ()
reportImportCyclesRule =
    define $ \ReportImportCycles file -> fmap (\errs -> if null errs then ([], Just ()) else (errs, Nothing)) $ do
        DependencyInformation{..} <- use_ GetDependencyInformation file
        let fileId = pathToId depPathIdMap file
        case IntMap.lookup (getFilePathId fileId) depErrorNodes of
            Nothing -> pure []
            Just errs -> do
                let cycles = mapMaybe (cycleErrorInFile fileId) (toList errs)
                -- Convert cycles of files into cycles of module names
                forM cycles $ \(imp, files) -> do
                    modNames <- forM files $ \fileId -> do
                        let file = idToPath depPathIdMap fileId
                        getModuleName file
                    pure $ toDiag imp $ sort modNames
    where cycleErrorInFile f (PartOfCycle imp fs)
            | f `elem` fs = Just (imp, fs)
          cycleErrorInFile _ _ = Nothing
          toDiag imp mods = (fp , ShowDiag , ) $ Diagnostic
            { _range = (_range :: Location -> Range) loc
            , _severity = Just DsError
            , _source = Just "Import cycle detection"
            , _message = "Cyclic module dependency between " <> showCycle mods
            , _code = Nothing
            , _relatedInformation = Nothing
            }
            where loc = srcSpanToLocation (getLoc imp)
                  fp = toNormalizedFilePath $ srcSpanToFilename (getLoc imp)
          getModuleName file = do
           pm <- use_ GetParsedModule file
           pure (moduleNameString . moduleName . ms_mod $ pm_mod_summary pm)
          showCycle mods  = T.intercalate ", " (map T.pack mods)

-- returns all transitive dependencies in topological order.
-- NOTE: result does not include the argument file.
getDependenciesRule :: Rules ()
getDependenciesRule =
    defineEarlyCutoff $ \GetDependencies file -> do
        depInfo@DependencyInformation{..} <- use_ GetDependencyInformation file
        let allFiles = reachableModules depInfo
        _ <- uses_ ReportImportCycles allFiles
        opts <- getIdeOptions
        let mbFingerprints = map (fingerprintString . fromNormalizedFilePath) allFiles <$ optShakeFiles opts
        return (fingerprintToBS . fingerprintFingerprints <$> mbFingerprints, ([], transitiveDeps depInfo file))

-- Source SpanInfo is used by AtPoint and Goto Definition.
getSpanInfoRule :: Rules ()
getSpanInfoRule =
    define $ \GetSpanInfo file -> do
        tc <- use_ TypeCheck file
        deps <- maybe (TransitiveDependencies [] []) fst <$> useWithStale GetDependencies file
        tms <- mapMaybe (fmap fst) <$> usesWithStale GetParsedModule (transitiveModuleDeps deps)
        (fileImports, _) <- use_ GetLocatedImports file
        packageState <- hscEnv <$> use_ GhcSession file
        x <- liftIO $ getSrcSpanInfos packageState (fmap (second (fmap modLocationToNormalizedFilePath)) $ fileImports) (tmrModule tc) tms
        return ([], Just x)

-- Typechecks a module.
typeCheckRule :: Rules ()
typeCheckRule = define $ \TypeCheck file -> do
  pm     <- use_ GetParsedModule file
  logger <- actionLogger
  liftIO
    $  logDebug logger
    $  T.pack
    $  "Typechecking file "
    <> fromNormalizedFilePath file
  deps <- use_ GetDependencies file
  hsc  <- hscEnv <$> use_ GhcSession file
  -- Figure out whether we need TemplateHaskell or QuasiQuotes support
  let graph_needs_th_qq = needsTemplateHaskellOrQQ $ hsc_mod_graph hsc
      file_uses_th_qq   = uses_th_qq $ ms_hspp_opts (pm_mod_summary pm)
      any_uses_th_qq    = graph_needs_th_qq || file_uses_th_qq
  mirs      <- uses_ GetModIface (transitiveModuleDeps deps)
  bytecodes <- if any_uses_th_qq
    then -- If we use TH or QQ, we must obtain the bytecode
      fmap Just <$> uses_ GenerateByteCode (transitiveModuleDeps deps)
    else
      pure $ repeat Nothing

  setPriority priorityTypeCheck
  IdeOptions { optDefer = defer } <- getIdeOptions

  res <- liftIO $ typecheckModule defer hsc (zipWith unpack mirs bytecodes) pm

  whenJust (snd res) $ \tcm -> do
    (_, contents) <- getFileContents file
    whenJust contents $ \sb ->
      liftIO $ generateAndWriteHieFile hsc (stringBufferToByteString sb) (tmrModule tcm)

  return res
 where
  unpack HiFileResult{..} bc = (hirModSummary, (hirModIface, bc))
  uses_th_qq dflags =
    xopt LangExt.TemplateHaskell dflags || xopt LangExt.QuasiQuotes dflags


generateCore :: NormalizedFilePath -> Action (IdeResult (SafeHaskellMode, CgGuts, ModDetails))
generateCore file = do
    deps <- use_ GetDependencies file
    (tm:tms) <- uses_ TypeCheck (file:transitiveModuleDeps deps)
    setPriority priorityGenerateCore
    packageState <- hscEnv <$> use_ GhcSession file
    liftIO $ compileModule packageState [(tmrModSummary x, tmrModInfo x) | x <- tms] tm

generateCoreRule :: Rules ()
generateCoreRule =
    define $ \GenerateCore -> generateCore

generateByteCodeRule :: Rules ()
generateByteCodeRule =
    define $ \GenerateByteCode file -> do
      deps <- use_ GetDependencies file
      (tm : tms) <- uses_ TypeCheck (file: transitiveModuleDeps deps)
      session <- hscEnv <$> use_ GhcSession file
      (_, guts, _) <- use_ GenerateCore file
      liftIO $ generateByteCode session [(tmrModSummary x, tmrModInfo x) | x <- tms] (tmrModSummary tm, tmrModInfo tm) guts

-- A local rule type to get caching. We want to use newCache, but it has
-- thread killed exception issues, so we lift it to a full rule.
-- https://github.com/digital-asset/daml/pull/2808#issuecomment-529639547
type instance RuleResult GhcSessionIO = GhcSessionFun

data GhcSessionIO = GhcSessionIO deriving (Eq, Show, Typeable, Generic)
instance Hashable GhcSessionIO
instance NFData   GhcSessionIO
instance Binary   GhcSessionIO

newtype GhcSessionFun = GhcSessionFun (FilePath -> Action HscEnvEq)
instance Show GhcSessionFun where show _ = "GhcSessionFun"
instance NFData GhcSessionFun where rnf !_ = ()


loadGhcSession :: Rules ()
loadGhcSession = do
    defineNoFile $ \GhcSessionIO -> do
        opts <- getIdeOptions
        liftIO $ GhcSessionFun <$> optGhcSession opts
    defineEarlyCutoff $ \GhcSession file -> do
        GhcSessionFun fun <- useNoFile_ GhcSessionIO
        val <- fun $ fromNormalizedFilePath file
        opts <- getIdeOptions
        return ("" <$ optShakeFiles opts, ([], Just val))

getPackageHieFileRule :: Rules ()
getPackageHieFileRule =
    defineNoFile $ \(GetPackageHieFile f) -> do
    liftIO $ loadHieFile f

getHieFileRule :: Rules ()
getHieFileRule =
    define $ \(GetHieFile hie_f) f -> do
    logger <- actionLogger
    mbHieTimestamp <- use GetModificationTime $ toNormalizedFilePath hie_f
    srcTimestamp <- use_ GetModificationTime f
    case (mbHieTimestamp, srcTimestamp) of
      (Just (ModificationTime hie), ModificationTime src) | hie > src -> do
        hf  <- liftIO $ loadHieFile hie_f
        liftIO $ logDebug logger $ T.pack $ "Loaded .hie file " <> hie_f
        return ([], Just hf)
      _ -> do
        if isJust mbHieTimestamp
          then liftIO $ logDebug logger $ T.pack $ "skipping stale .hie file: " <> hie_f
          else liftIO $ logDebug logger $ T.pack $ "failed to load .hie missing file: " <> hie_f
        return ([], Nothing)

getHiFileRule :: Rules ()
getHiFileRule = define $ \GetHiFile f -> do
  session <- hscEnv <$> use_ GhcSession f
  logger  <- actionLogger
  pm      <- use_ GetParsedModule f
  -- TODO find the hi file without relying on the parsed module
  --      it should be possible to construct a ModSummary parsing just the imports
  --      (see HeaderInfo in the GHC package)
  let hiFile = ml_hi_file $ ms_location ms
      ms     = pm_mod_summary pm
  gotHiFile <- getFileExists $ toNormalizedFilePath hiFile
  if gotHiFile
    then do
      r <- liftIO $ loadInterface session hiFile (ms_mod ms)
      case r of
        Right iface -> do
          let result = HiFileResult ms iface
          liftIO $ logDebug logger $ T.pack $ "Loaded interface file " <> hiFile
          return ([], Just result)
        Left err -> do
          let d = ideErrorText f errMsg
              errMsg = T.pack err
          liftIO
            $  logDebug logger
            $  T.pack ("Failed to load interface file " <> hiFile <> ": ")
            <> errMsg
          return ([d], Nothing)
    else do
      liftIO $ logDebug logger $ T.pack ("Missing or stale interface file for" <> hiFile)
      pure ([], Nothing)

getModIfaceRule :: Rules ()
getModIfaceRule = define $ \GetModIface f -> do
    filesOfInterest <- getFilesOfInterest
    let useHiFile =
          -- Interface files do not carry location information, so
          -- never use interface files if .hie files are not available
          GHC.supportsHieFiles &&
          -- Never load interface files for files of interest
          f `notElem` filesOfInterest
    mbHiFile <- if useHiFile then use GetHiFile f else return Nothing
    case mbHiFile of
        Just x ->
            return ([], Just x)
        Nothing -> do
            TcModuleResult{tmrModInfo, tmrModSummary} <- use_ TypeCheck f
            let iface = hm_iface tmrModInfo
            return ([], Just $ HiFileResult tmrModSummary iface)

-- | A rule that wires per-file rules together
mainRule :: Rules ()
mainRule = do
    getParsedModuleRule
    getLocatedImportsRule
    getDependencyInformationRule
    reportImportCyclesRule
    getDependenciesRule
    typeCheckRule
    getSpanInfoRule
    generateCoreRule
    generateByteCodeRule
    loadGhcSession
    getPackageHieFileRule
    getHieFileRule
    getHiFileRule
    getModIfaceRule
