{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, AllowAmbiguousTypes, TypeFamilies, GADTs, FunctionalDependencies #-}
module System.Win32.ProjFS.Helper where

import System.Win32.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.StaticArray
import Data.Array.Unboxed
import System.Entropy
import Data.ByteString as BS (ByteString, pack, unpack, length)
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy as LB
import System.Win32.ProjFS.Types
import Data.Data
import Prelude hiding (fail)
import Control.Monad.Fail
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Data.List.Split
import Foreign.C.Types


peek_ahead :: forall a b . Storable a => Ptr b -> IO (Ptr b, a)
peek_ahead (flip alignPtr (alignment (undefined :: a)) -> ptr) = 
  (plusPtr ptr (sizeOf (undefined :: a)),) <$> peek (castPtr ptr)

poke_ahead :: forall a b . Storable a => a -> Ptr b -> IO (Ptr b)
poke_ahead a ptr = do
  let ptr' = castPtr . alignPtr ptr . alignment $ a
  poke ptr' a
  return . plusPtr ptr' . sizeOf $ a

data PeekContext s a = PeekContext (Ptr s) a

class AutoPeek a b where
  auto_peek' :: a -> b

instance AutoPeek (PeekContext a a) (IO a) where
  auto_peek' (PeekContext _ a) = return a

instance (Storable b, AutoPeek (PeekContext a c) (IO d)) => AutoPeek (PeekContext a (b -> c)) (IO d) where
  auto_peek' (PeekContext ptr bc) = do
    (ptr2, b) <- peek_ahead ptr
    auto_peek' $ PeekContext ptr2 (bc b)

auto_peek :: forall b c . (Storable b, AutoPeek (PeekContext b c) (IO b)) => c -> Ptr b -> IO b
auto_peek = (auto_peek' .) . flip PeekContext



class ApiResult a r | a -> r where
  to_result :: MonadFail m => String -> a -> m r

instance ApiResult HRESULT () where
  to_result name r = if r == 0 then return () else
    fail $ "error call " ++ name ++ " return " ++ show r
instance ApiResult (HRESULT, r) r where
  to_result name (r, x) = x <$ to_result name r

convert_result :: (MonadFail m, MonadIO m, ApiResult a r) => String -> IO a -> m r
convert_result name x = liftIO x >>= to_result name

write_lazy_bytestring :: MonadIO m => LB.ByteString -> Ptr () -> m ()
write_lazy_bytestring (LB.toStrict -> bs) p = liftIO $ withByteStringPtrLen bs $ \p1 -> memcpy (castPtr p) (castPtr p1)

withByteStringPtr bs f = withByteStringPtrLen bs $ \p _ -> f p


-- hack: 这里的storable主要是在binding代码里面取size, 其实应该换一个class
instance Storable ByteString where
  sizeOf = BS.length
  peek = undefined
  poke = undefined
  alignment = const 8

instance Integral i => Storable (Ptr a, i) where
  sizeOf = fromIntegral . snd
  peek = undefined
  poke = undefined
  alignment = const 8
  

class IsBuffer a where
  withByteStringPtrLen :: a -> (Ptr () -> Int -> IO b) -> IO b

instance IsBuffer ByteString where
  withByteStringPtrLen (PS b o l) f = withForeignPtr b $ \b' -> f (b' `plusPtr` o) l

instance Integral i => IsBuffer (Ptr a, i) where
  withByteStringPtrLen (ptr, len) f = f (castPtr ptr) (fromIntegral len)


withTStringMaybe :: Maybe String -> (LPTSTR -> IO a) -> IO a
withTStringMaybe Nothing = ($ nullPtr)
withTStringMaybe (Just s) = withTString s

withMaybe :: Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybe Nothing = ($ nullPtr)
withMaybe (Just s) = withPtr s

withPtr :: Storable a => a -> (Ptr a -> IO b) -> IO b
withPtr a f = alloca' $ \ptr -> poke ptr a >> f ptr

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap
comp2 a b = (a .) . b
  

pack_storable :: Storable a => a -> IO ByteString
pack_storable a = withPtr a $ \ptr ->   
  create len (\x -> memcpy x (castPtr ptr) len)
  where len = sizeOf a

show_hex :: ByteString -> String
show_hex = (map_join "\n" . map_join "  " . map_join " " $ i8_to_hex) . chunksOf 2 . chunksOf 8 . unpack
  where map_join x f = intercalate x . map f

print_storable :: MonadIO m => Storable a => a -> m ()
print_storable a = liftIO $ pack_storable a >>= Prelude.putStrLn . show_hex

i8_to_hex ((`divMod`16) . fromIntegral -> (h,l)) = [f h, f l] where f = toUpper . intToDigit

-- alloca + memset
alloca' :: forall a b . Storable a => (Ptr a -> IO b) -> IO b
alloca' f = alloca $ \ptr -> memset (castPtr ptr) 0 (CSize . fromIntegral $ sizeOf' @a) >> f ptr


sizeOf' :: forall a . Storable a => Int
sizeOf' = sizeOf (undefined :: a)

alignment' :: forall a . Storable a => Int
alignment' = alignment (undefined :: a)

sizeOfStruct :: [(Int, Int)] -> Int
sizeOfStruct = (\(IntPtr i) -> i) . ptrToIntPtr . foldl f nullPtr 
  where f ptr (a, s) = ptr `alignPtr` a `plusPtr` s `alignPtr` a

alignmentOfStruct :: [(Int, Int)] -> Int
alignmentOfStruct = maximum . map fst

new_guid :: MonadIO m => m GUID
new_guid = liftIO $ listStaticArray . unpack <$> getEntropy 16

pack_guid :: GUID -> ByteString
pack_guid = pack . elems . toArray
