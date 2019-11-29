{-# LANGUAGE DataKinds, CPP, Trustworthy, FlexibleInstances #-}

module System.Win32.ProjFS.Types where
import System.Win32.Types
import Foreign.Ptr
import Foreign.Marshal.StaticArray
import Data.Array.Unboxed
import Data.Default
import Data.Ix.Static
import GHC.TypeLits

type UINT8 = BYTE
type PCWSTR = LPCWSTR
type BOOLEAN = BYTE
type GUID = StaticArray UArray 16 UINT8

type PRJ_NAMESPACE_VIRTUALIZATION_CONTEXT = HANDLE
type PRJ_DIR_ENTRY_BUFFER_HANDLE = HANDLE

hr_Ok = 0 :: HRESULT
hr_Pending = -2147023899 :: HRESULT
hr_OutOfMemory = -2147024882 :: HRESULT
hr_InsufficientBuffer = -2147024774 :: HRESULT
hr_FileNotFound = -2147024894 :: HRESULT
hr_VirtualizationUnavaliable = -2147024527 :: HRESULT
hr_InternalError = -2147023537 :: HRESULT
hr_AlreadyInitialized = -2147023649 :: HRESULT
hr_AccessDenied = -2147024891 :: HRESULT
hr_CannotDelete = -805306079 :: HRESULT
hr_Directory = -2147024629 :: HRESULT
hr_DirNotEmpty = -2147024751 :: HRESULT
hr_Handle = -2147024890 :: HRESULT
hr_InvalidArg = -2147024809 :: HRESULT
hr_PathNotFound = -2147024893 :: HRESULT
hr_ReparsePointEncountered = -2147020501 :: HRESULT
hr_VirtualizationInvalidOp = -2147024511 :: HRESULT

instance Default (FunPtr a) where def = nullFunPtr
instance Default (Ptr a) where def = nullPtr

instance (KnownNat d, IArray b e) => Default (StaticArray b d e) where
  def = listStaticArray []

