{-# LANGUAGE CPP                  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Development.IDE.Core.FileExists
  ( fileExistsRules
  , modifyFileExists
  , getFileExists
  )
where

import           Control.Concurrent.Extra
import           Control.Exception
import           Control.Monad.Extra
import qualified Data.Aeson                              as A
import           Data.Binary
import qualified Data.ByteString.Char8                   as BS
import qualified Data.ByteString.Lazy                    as LBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe
import qualified Data.Text                               as T
import           Development.IDE.Core.FileStore
import           Development.IDE.Core.IdeConfiguration
import           Development.IDE.Core.Shake
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger
import           Development.Shake
import           Development.Shake.Classes
import           Foreign.Ptr
import           GHC.Generics
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Types.Capabilities
import           Language.Haskell.LSP.VFS
import qualified System.Directory                        as Dir
import           System.IO.Error

#ifdef mingw32_HOST_OS
import           Data.Time
#else
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal                         (alloca)
import           Foreign.Storable
import qualified System.Posix.Error                      as Posix
#endif

data FileExistsVersion = FileExists Int | FileDoesNotExist

isFileExists :: FileExistsVersion -> Bool
isFileExists FileExists{} = True
isFileExists _            = False

fileExistsFromDoesFileExist :: Bool -> FileExistsVersion
fileExistsFromDoesFileExist False = FileDoesNotExist
fileExistsFromDoesFileExist True  = FileExists 1

instance Semigroup FileExistsVersion where
  FileExists a <> FileExists b = FileExists (a+b)
  FileExists a <> FileDoesNotExist = FileExists a
  FileDoesNotExist <> FileExists a = FileExists a
  FileDoesNotExist <> FileDoesNotExist = FileDoesNotExist

-- | A map for tracking the file existence
type FileExistsMap = (HashMap NormalizedFilePath FileExistsVersion)

-- | A wrapper around a mutable 'FileExistsMap'
newtype FileExistsMapVar = FileExistsMapVar (Var FileExistsMap)

instance IsIdeGlobal FileExistsMapVar

-- | Grab the current global value of 'FileExistsMap' without acquiring a dependency
getFileExistsMapUntracked :: Action FileExistsMap
getFileExistsMapUntracked = do
  FileExistsMapVar v <- getIdeGlobalAction
  liftIO $ readVar v

-- | Modify the global store of file exists
modifyFileExistsAction :: (FileExistsMap -> IO FileExistsMap) -> Action ()
modifyFileExistsAction f = do
  FileExistsMapVar var <- getIdeGlobalAction
  liftIO $ modifyVar_ var f

fileChangeToFileExistsVersion :: FileChangeType -> FileExistsVersion
fileChangeToFileExistsVersion FcCreated = FileExists 1
fileChangeToFileExistsVersion FcChanged = FileExists 1
fileChangeToFileExistsVersion FcDeleted = FileDoesNotExist

-- | Modify the global store of file exists
modifyFileExists :: IdeState -> [(NormalizedFilePath, FileChangeType)] -> IO ()
modifyFileExists state changes = do
  FileExistsMapVar var <- getIdeGlobalState state
  changesMap           <- evaluate $ HashMap.fromList changes

  -- Masked to ensure that the previous values are flushed together with the map update
  mask $ \_ -> do
    -- update the map
    modifyVar_ var $ evaluate . HashMap.unionWith (<>) (fileChangeToFileExistsVersion <$> changesMap)
    -- flush previous values
    forM_ changes $ \(fp,_) -> do
      deleteValue state GetFileExists fp
      deleteValue state GetModificationTime fp

-------------------------------------------------------------------------------------

type instance RuleResult GetFileExists = Bool

data GetFileExists = GetFileExists
    deriving (Eq, Show, Typeable, Generic)

instance NFData   GetFileExists
instance Hashable GetFileExists
instance Binary   GetFileExists

-- | Returns True if the file exists
--   Note that a file is not considered to exist unless it is saved to disk.
--   In particular, VFS existence is not enough.
--   Consider the following example:
--     1. The file @A.hs@ containing the line @import B@ is added to the files of interest
--        Since @B.hs@ is neither open nor exists, GetLocatedImports finds Nothing
--     2. The editor creates a new buffer @B.hs@
--        Unless the editor also sends a @DidChangeWatchedFile@ event, ghcide will not pick it up
--        Most editors, e.g. VSCode, only send the event when the file is saved to disk.
getFileExists :: NormalizedFilePath -> Action Bool
getFileExists fp = use_ GetFileExists fp

-- | Installs the 'getFileExists' and 'getModificationTime' rules.
--   Uses fast implementations if client supports dynamic watched files.
--   Creates a global state as a side effect in that case.
fileExistsRules :: IO LspId -> ClientCapabilities -> VFSHandle -> Rules ()
fileExistsRules getLspId ClientCapabilities{_workspace} vfs = do
  -- Create the global always, although it should only be used if we have fast rules.
  -- But there's a chance someone will send unexpected notifications anyway,
  -- e.g. https://github.com/digital-asset/ghcide/issues/599
  addIdeGlobal . FileExistsMapVar =<< liftIO (newVar mempty)
  case () of
    _ | Just WorkspaceClientCapabilities{_didChangeWatchedFiles} <- _workspace
      , Just DidChangeWatchedFilesClientCapabilities{_dynamicRegistration} <- _didChangeWatchedFiles
      , Just True <- _dynamicRegistration -> do
          fileExistsRulesFast getLspId vfs
          getModificationTimeRule (\f _ -> getModTimeFromEvents f) vfs
      | otherwise -> do
        logger <- logger <$> getShakeExtrasRules
        liftIO $ logDebug logger "Warning: Client does not support watched files. Falling back to OS polling"
        fileExistsRulesSlow vfs
        let getModTimeIOAlwaysRerun f f' = do
              alwaysRerun
              liftIO $ getModTimeIO f f'
        getModificationTimeRule getModTimeIOAlwaysRerun vfs

-------------------------------------------------------------------------------------
--   Requires an lsp client that provides WatchedFiles notifications.
fileExistsRulesFast :: IO LspId -> VFSHandle -> Rules ()
fileExistsRulesFast getLspId vfs =
  defineEarlyCutoff $ \GetFileExists file -> do
    isWf <- isWorkspaceFile file
    if isWf
        then fileExistsFast getLspId vfs file
        else fileExistsSlow vfs file

fileExistsFast :: IO LspId -> VFSHandle -> NormalizedFilePath -> Action (Maybe BS.ByteString, ([a], Maybe Bool))
fileExistsFast getLspId vfs file = do
    fileExistsMap <- getFileExistsMapUntracked
    let mbFilesWatched = HashMap.lookup file fileExistsMap
    case mbFilesWatched of
      Just (isFileExists -> fv) -> pure (summarizeExists fv, ([], Just fv))
      Nothing -> do
        exist                   <- liftIO $ getFileExistsVFS vfs file
        ShakeExtras { eventer } <- getShakeExtras
        let fileExists = fileExistsFromDoesFileExist exist

        -- add a listener for VFS Create/Delete file events,
        -- taking the FileExistsMap lock to prevent race conditions
        -- that would lead to multiple listeners for the same path
        modifyFileExistsAction $ \x -> do
          case HashMap.alterF (,Just fileExists) file x of
            (Nothing, x') -> do
            -- if the listener addition fails, we never recover. This is a bug.
              addListener eventer file
              return x'
            (Just _, _) ->
              -- if the key was already there, do nothing
              return x
        pure (summarizeExists exist, ([], Just exist))
 where
  addListener eventer fp = do
    reqId <- getLspId
    let req = mkRegistrationEvent reqId fp
    eventer $ ReqRegisterCapability req

mkRegistrationEvent :: LspId -> NormalizedFilePath -> RequestMessage ServerMethod RegistrationParams resp
mkRegistrationEvent reqId fp =
  RequestMessage "2.0" reqId ClientRegisterCapability regParams
 where
  fpAsId       = T.pack $ fromNormalizedFilePath fp
  regParams    = RegistrationParams (List [registration])
  registration = Registration fpAsId
                              WorkspaceDidChangeWatchedFiles
                              (Just (A.toJSON regOptions))
  regOptions =
    DidChangeWatchedFilesRegistrationOptions { _watchers = List [watcher] }
  watcher = FileSystemWatcher { _globPattern = fromNormalizedFilePath fp
                              , _kind        = Nothing -- All event kinds
                              }

summarizeExists :: Bool -> Maybe BS.ByteString
summarizeExists x = Just $ if x then BS.singleton '1' else BS.empty

fileExistsRulesSlow:: VFSHandle -> Rules ()
fileExistsRulesSlow vfs =
  defineEarlyCutoff $ \GetFileExists file -> fileExistsSlow vfs file

fileExistsSlow :: VFSHandle -> NormalizedFilePath -> Action (Maybe BS.ByteString, ([a], Maybe Bool))
fileExistsSlow vfs file = do
    alwaysRerun
    exist <- liftIO $ getFileExistsVFS vfs file
    pure (summarizeExists exist, ([], Just exist))

getFileExistsVFS :: VFSHandle -> NormalizedFilePath -> IO Bool
getFileExistsVFS vfs file = do
    -- we deliberately and intentionally wrap the file as an FilePath WITHOUT mkAbsolute
    -- so that if the file doesn't exist, is on a shared drive that is unmounted etc we get a properly
    -- cached 'No' rather than an exception in the wrong place
    handle (\(_ :: IOException) -> return False) $
        (isJust <$> getVirtualFile vfs (filePathToUri' file)) ||^
        Dir.doesFileExist (fromNormalizedFilePath file)

--------------------------------------------------------------------------------------------------

getModificationTimeRule :: (NormalizedFilePath -> FilePath -> Action (Maybe BS.ByteString, IdeResult FileVersion)) -> VFSHandle -> Rules ()
getModificationTimeRule getModTime vfs =
    defineEarlyCutoff $ \GetModificationTime file -> do
        let file' = fromNormalizedFilePath file
        mbVirtual <- liftIO $ getVirtualFile vfs $ filePathToUri' file
        case mbVirtual of
            Just (virtualFileVersion -> ver) ->
              pure (Just $ BS.pack $ show ver, ([], Just $ WatchedFileVersion ver))
            Nothing ->
              getModTime file file'

getModTimeFromEvents :: NormalizedFilePath -> Action (Maybe BS.ByteString, ([FileDiagnostic], Maybe FileVersion))
getModTimeFromEvents file = do
    fileExistsMap <- getFileExistsMapUntracked
    let fileDoesNotExistError = ideErrorText file (T.pack msg)
        msg = "File does not exists: " ++ file'
        file' = fromNormalizedFilePath file
    case HashMap.lookup file fileExistsMap of
      Just (FileExists n) ->
        pure (summarizeVersion n, ([], Just (VFSVersion n)))
      Just FileDoesNotExist ->
        pure (Nothing, ([fileDoesNotExistError], Nothing))
      Nothing -> do
        exists <- getFileExists file
        if exists
          then pure (summarizeVersion 0,   ([], Just (VFSVersion 0)))
          else pure (Nothing, ([fileDoesNotExistError], Nothing))
  where
    summarizeVersion :: Int -> Maybe BS.ByteString
    summarizeVersion = Just . LBS.toStrict . encode

getModTimeIO :: NormalizedFilePath -> FilePath -> IO (Maybe BS.ByteString, ([FileDiagnostic], Maybe FileVersion))
getModTimeIO file file' = fmap wrap (getModTime file') `catch`
  \(e :: IOException) -> do
    let err | isDoesNotExistError e = "File does not exist: " ++ file'
            | otherwise = "IO error while reading " ++ file' ++ ", " ++ displayException e
    return (Nothing, ([ideErrorText file $ T.pack err], Nothing))
  where
    wrap time@(l,s) = (Just $ BS.pack $ show time, ([], Just $ ModificationTime l s))
    -- Dir.getModificationTime is surprisingly slow since it performs
    -- a ton of conversions. Since we do not actually care about
    -- the format of the time, we can get away with something cheaper.
    -- For now, we only try to do this on Unix systems where it seems to get the
    -- time spent checking file modifications (which happens on every change)
    -- from > 0.5s to ~0.15s.
    getModTime :: FilePath -> IO (Int,Int)
    getModTime f =
#ifdef mingw32_HOST_OS
        do time <- Dir.getModificationTime f
           let !day = fromInteger $ toModifiedJulianDay $ utctDay time
               !dayTime = fromInteger $ diffTimeToPicoseconds $ utctDayTime time
           pure (day, dayTime)
#else
        withCString f $ \f' ->
        alloca $ \secPtr ->
        alloca $ \nsecPtr -> do
            Posix.throwErrnoPathIfMinus1Retry_ "getmodtime" f $ c_getModTime f' secPtr nsecPtr
            sec <- peek secPtr
            nsec <- peek nsecPtr
            pure (fromEnum sec, fromIntegral nsec)

-- Sadly even unix’s getFileStatus + modificationTimeHiRes is still about twice as slow
-- as doing the FFI call ourselves :(.
foreign import ccall "getmodtime" c_getModTime :: CString -> Ptr CTime -> Ptr CLong -> IO Int
#endif
