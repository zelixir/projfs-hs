{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module System.Win32.ProjFS.SimpleProvider (Range, Provider, mount) where

import System.Win32.ProjFS
import qualified Data.ByteString.Lazy as LB
import Prelude hiding (fail)
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Algebra
import Control.Carrier.Error.Either (runError, throwError)
import Control.Carrier.State.Strict
import System.Directory
import System.IO
import Control.Concurrent
import System.Win32.Types
import Foreign.Ptr
import qualified Data.Map as M
import Control.Concurrent.MVar
import Data.Either
import Data.Bits
import Data.List
import Data.List.Split
import Data.Default
import Data.Char
import Control.Arrow
import GHC.Int
import Data.Functor
import Data.Function

data Range = Range { offset :: Int, len :: Int }

-- 不支持提供文件夹的时间
data FileInfo = FileInfo { fileInfoTime :: Integer, fileInfoSize :: Int }
data Provider a = Provider { files :: GenDir a, 
  getFileData :: Range -> a -> IO (Maybe LB.ByteString),
  getFileInfo :: a -> FileInfo,
  logging_f :: forall m . MonadIO m => Maybe (String -> m ()) }


data ScanSession a = ScanSession { fullSession, currentSession :: [(String, GenDir a)] }
data ProjContext a = ProjContext { index :: GenDir a, sessions :: MVar (M.Map String (ScanSession a)) }

new_session list = ScanSession list list
rescan ScanSession{..} = ScanSession fullSession fullSession
session_pop ScanSession{..} = second (ScanSession fullSession) <$> uncons currentSession

orElse m e = maybe (throwError e) return m

mount root provider = do
  ensure_placeholder root
  cb <- liftIO $ create_callback_table provider
  prjStartVirtualizing' root cb Nothing Nothing

create_callback_table Provider {..} = do
  ctx <- ProjContext files <$> newMVar M.empty
  startDirectoryEnumerationCallback <- mkPRJ_START_DIRECTORY_ENUMERATION_CB $ sdec ctx
  endDirectoryEnumerationCallback <- mkPRJ_END_DIRECTORY_ENUMERATION_CB $ edec ctx
  getDirectoryEnumerationCallback <- mkPRJ_GET_DIRECTORY_ENUMERATION_CB $ gdec ctx
  getPlaceholderInfoCallback <- mkPRJ_GET_PLACEHOLDER_INFO_CB $ gpic ctx
  getFileDataCallback <- mkPRJ_GET_FILE_DATA_CB $ gfdc ctx
  let queryFileNameCallback = nullFunPtr 
  let notificationCallback = nullFunPtr 
  let cancelCommandCallback = nullFunPtr
  return PRJ_CALLBACKS { .. }
  where
  logging str = maybe (return ()) ($ str) logging_f
  sdec ProjContext {..} PRJ_CALLBACK_DATA {..} (show -> enumid) = do
    path <- liftIO $ peekTString filePathName
    logging $ "start enum: " ++ path
    logging $ "start enumid: " ++ enumid
    let list = new_session . flip enumDir index $ path
    modifyMVar_ sessions (return . M.insert enumid list)
    return hr_Ok
  edec ProjContext {..} PRJ_CALLBACK_DATA {..} (show -> enumid) = do
    logging $ "end enum: " ++ enumid
    modifyMVar_ sessions (return . M.delete enumid)
    return hr_Ok
  gdec ProjContext {..} PRJ_CALLBACK_DATA {..} (show -> enumid) searchExpression buf = mecb $ do
      search <- liftIO $ peekTString searchExpression
      logging $ "get enum: " ++ enumid
      logging $ "search :" ++ search
      s1 <- liftIO $ withMVar sessions (return . M.lookup enumid) 
      s2 <- s1 `orElse` hr_InternalError
      if flags .&. pRJ_CB_DATA_FLAG_ENUM_RESTART_SCAN > 0 then logging "rescan" else return ()
      let s3 = if flags .&. pRJ_CB_DATA_FLAG_ENUM_RESTART_SCAN > 0 then rescan s2 else s2
      (s, r) <- runState s3 (fill_dir buf)
      liftIO $ modifyMVar_ sessions (return . M.insert enumid s)
      return r
  fill_dir h = do
    s <- get
    case session_pop s of
      Nothing -> return hr_Ok
      Just ((filename, fileinfo -> entry), s') -> do
        logging $ "put: " ++ filename
        -- print_storable entry
        r <- liftIO $ prjFillDirEntryBuffer filename (Just entry) h
        if r == 0 then put s' >> fill_dir h else return hr_Ok
  gpic ProjContext {..} PRJ_CALLBACK_DATA {..} = mecb $ do
    path <- liftIO $ peekTString filePathName
    logging $ "get_place_holder_info: " ++ path
    item <- getItem path index `orElse` hr_FileNotFound
    let info = def { fileBasicInfo = fileinfo item }
    -- logging $ show info
    liftIO $ prjWritePlaceholderInfo namespaceVirtualizationContext path info
  gfdc ProjContext {..} PRJ_CALLBACK_DATA {..} offset (fromIntegral -> len) = mecb $ do 
    path <- liftIO $ peekTString filePathName
    logging $ "get_file_data: " ++ path
    let rng = Range (fromIntegral offset) (fromIntegral len)
    maybeContent <- liftIO $ getFile path index <&> getFileData rng & sequence <&> join
    content <- maybeContent `orElse` hr_FileNotFound
    -- 写入文件数据时要求buffer对齐, 但不知道对齐参数
    -- 需要调用创建buffer的api来创建一个已对齐的buffer
    -- 预设buffer最大为64k
    let chunk_size = min len $ 64 * 1024
    buffer <- liftIO $ prjAllocateAlignedBuffer namespaceVirtualizationContext $ fromIntegral chunk_size
    let chunks = chunksBuffer (fromIntegral chunk_size) content
    runState offset $ forM chunks $ \c -> do
      o <- get
      write_lazy_bytestring c buffer
      let len = LB.length c
      prjWriteFileData' namespaceVirtualizationContext dataStreamId (buffer, len) o
      put $ o + fromIntegral len
    return hr_Ok
  mecb = (>>= (\x -> print x >> return x)) . fmap (either id id) . runError --monad error callback
  fileinfo (Dir _) = def {
    isDirectory = 1, 
    fileAttributes = 16 .|. 1 -- dir | readonly
  }
  fileinfo (File (getFileInfo -> FileInfo{..})) = PRJ_FILE_BASIC_INFO {
    isDirectory = 0, 
    fileSize = fromIntegral fileInfoSize, 
    creationTime = time, 
    lastAccessTime = time, 
    lastWriteTime = time, 
    changeTime = time, 
    fileAttributes = 128 .|. 1 -- normal | readonly
  } where time = fromInteger fileInfoTime
  

chunksBuffer :: Int64 -> LB.ByteString -> [LB.ByteString]
chunksBuffer _ b | LB.null b = []
chunksBuffer n b = uncurry (:) . second (chunksBuffer n) . LB.splitAt n $ b



eRROR_NOT_A_REPARSE_POINT = 0x126
iO_REPARSE_TAG_PROJFS = 0x9000001C

-- todo: 完善检查, 目前默认是如果目录存在那么肯定是ProjFS
place_holder_info :: MonadIO m => FilePath -> m (Maybe (Either Int Int))
place_holder_info path = liftIO $ do
  de <- doesDirectoryExist path
  return $ if de then Just (Right iO_REPARSE_TAG_PROJFS) else Nothing

ensure_placeholder root = do
  dir_should_not_be_a_file root
  info <- place_holder_info root
  case info of
    Nothing -> liftIO (createDirectoryIfMissing True root) *> mark root
    Just (Left x) | x == eRROR_NOT_A_REPARSE_POINT -> mark root
    Just (Left x) -> fail $ root ++ " query dir info error: " ++ show x
    Just (Right x) | x == iO_REPARSE_TAG_PROJFS -> return ()
    Just (Right _) -> fail $ root ++ " is not a proj_fs dir"


mark root = new_guid >>= prjMarkDirectoryAsPlaceholder' root Nothing Nothing


dir_should_not_be_a_file path = liftIO $ do
  fe <- doesFileExist path
  if fe then fail $ path ++ " is a file!" else return ()
  
file_should_exist path = liftIO $ do
  fe <- doesFileExist path
  if not fe then fail $ path ++ " does not exist!" else return ()


