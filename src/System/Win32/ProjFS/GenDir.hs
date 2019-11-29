module System.Win32.ProjFS.GenDir where

import qualified Data.Map as M
import Data.Char
import Data.List
import Data.List.Split
import Data.Function
import Control.Arrow
import System.Win32.ProjFS.Helper

data GenDir f = Dir (M.Map String (GenDir f)) | File f  deriving (Show)

newGenDir :: [(FilePath, f)] -> GenDir f
newGenDir = newGenDir' . map (first splitPath) . sortOn fst . nubBy ((==) `on` fst)
  where
  -- match 失败的话可能是文件名重复
  newGenDir' [([], f)] = File f
  newGenDir' x = Dir . M.fromList .
    map (head . fst . head &&& newGenDir' . map (first tail)) .
    groupBy ((==) `on` (head . fst)) $ x

enumDir :: FilePath -> GenDir f -> [(String, GenDir f)]
enumDir = (sortOn (map toLower . fst) . maybe [] M.toList . (>>= unDir)) `comp2` getItem

getFile :: FilePath -> GenDir f -> Maybe f
getFile = (>>= unFile) `comp2` getItem

getItem :: FilePath -> GenDir f -> Maybe (GenDir f)
getItem = getItem' . splitPath
  where 
  getItem' (x:xs) (Dir m) = M.lookup x m >>= getItem' xs
  getItem' [] x = Just x
  getItem' _ _ = Nothing
  
splitPath :: FilePath -> [String]
splitPath = filter (not.null) . splitOneOf "/\\"

unDir :: GenDir f -> Maybe (M.Map String (GenDir f))
unDir (Dir m) = Just m
unDir _ = Nothing
unFile :: GenDir f -> Maybe f
unFile (File m) = Just m
unFile _ = Nothing

flat_top_dir :: GenDir f -> GenDir f
flat_top_dir (Dir m) | M.size m == 1 = snd . head . M.toList $ m
flat_top_dir x = x

