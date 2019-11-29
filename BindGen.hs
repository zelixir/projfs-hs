{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ViewPatterns, RebindableSyntax, 
    ImplicitPrelude, TypeApplications, DeriveDataTypeable, ImplicitParams, RankNTypes #-}

module BindGen where

import Text.Parsec
import Text.Parsec.String
import Data.Char
import Data.Maybe
import Data.Proxy
import Data.Bool
import Data.Data
import Data.List (find, intercalate, isSuffixOf, isPrefixOf, dropWhileEnd)
import Data.List.Split (splitOn)
import Debug.Trace
import Control.Arrow
import Prelude hiding ((>>))

----------------------parser-------------------------------
type Name = String
type Value = String
newtype Type = Type String deriving (Show, Eq, Data)
data Struct = Struct Name [StructItem] deriving (Show, Eq, Data)
data CEnum = CEnum Name [(Name, Value)] deriving (Show, Eq, Data)
data Field = Field Type Name deriving (Show, Eq, Data)
data Union = Union Name [Struct] deriving (Show, Eq, Data)
data Macro = Macro Name [Value] deriving (Show, Eq, Data)
data FuncType = FuncType Type Name [Arg] deriving (Show, Eq, Data)
data Func = Func Type Name [Arg] deriving (Show, Eq, Data)
type Arg = ([Macro], Field) 
type Decl = Struct :+: Union :+: CEnum :+: FuncType :+: Func
type StructItem = Field :+: Union :+: Struct
data HType = IN | PTR | OUT | INOUT | OPT | OPTPTR | BSIZE | BUFFER | STR HType deriving (Show, Eq, Data) -- todo: Fun

field_type (Field (Type t) _) = t
field_name (Field _ n) = n
struct_name (Struct n _) = n
struct_fields (Struct _ f) = f
union_name (Union n _) = n
func_type (FuncType _ n _) = n
macro_name (Macro n _) = n

prewalk f = prewalk' (mkT f)
prewalk' :: Data a => (forall b. Data b => b -> b) -> a -> a
prewalk' f x = f (gmapT (prewalk' f) x) 

mkT f = fromMaybe id (cast f)

data (f :+: g) = Inl f | Inr g  deriving (Show, Eq, Data)
infixr 1 :+:

class sub :<: sup where
  inj :: sub -> sup
  rej :: sup -> Maybe sub
  rejs :: Proxy sub -> [sup] -> [sub]
  rejs _ = mapMaybe rej
instance f :<: f where
  inj = id
  rej = Just
instance {-# OVERLAPPING #-} f :<: (f :+: g) where
  inj = Inl
  rej (Inl x) = Just x
  rej _ = Nothing
instance (f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj
  rej (Inr x) = rej x
  rej _ = Nothing

(<:+:>) :: (a -> c) -> (b -> c) -> (a :+: b) -> c
(<:+:>) f _ (Inl x) = f x
(<:+:>) _ g (Inr x) = g x

infixr 9 <:+:>

choice1 = choice . map try

file_p = spaces *> many1 (decl_p <* spaces) <* eof

decl_p :: Parser Decl
decl_p = typedef <|> func_p
  where typedef = kw "typedef" $ choice1 [enum_p, fst struct_p, fst union_p, func_type_p]

struct_p :: (Struct :<: f) => (Parser f, Parser f)
struct_p = block_p "struct" Struct struct_item_p
struct_item_p = many1 $ choice1 . map (<* spaces) $ [field_p <* spaces <* char ';', snd struct_p, snd union_p]
enum_p = fst $ block_p "enum" CEnum (colon_list p)
  where p = (,) <$> ident_p <*>~ (char '=' *> spaces *> ident_p)
field_p :: (Field :<: f) => Parser f
field_p = fmap inj $ Field . Type <$> type_p <*>~ ident_p
union_p :: (Union :<: f) => (Parser f, Parser f)
union_p = block_p "union" Union (many1 $ snd struct_p <* spaces)

func_p = fmap inj . until_char ';' $ Func . Type . to_type <$> macro_p (words "STDAPI_ STDAPI") <*>~ ident_p <*>~ bracket "()" (sepBy arg_p colon_sep)
  where to_type (Macro "STDAPI" _) = "HRESULT"
        to_type (Macro "STDAPI_" [x]) = x
func_type_p =  head $ FuncType . Type <$> type_p <*>~ bracket "()" (ident_p *> spaces *> ident_p) <*>~ bracket "()" (sepBy arg_p colon_sep)
  where head = fmap inj . until_char ';' . (macro_p ["_Function_class_"] *> spaces *>)

kw name p = string name *> spaces *> p
ident_p = many1 $ satisfy (\x -> isDigit x || isAlpha x || x `elem` "_[]" )
type_p = (++) <$> ident_p <*> option "" (try $ spaces *> string "*") -- fixme: multi *
macro_p macros = Macro <$> (choice1 . map string) macros <*> 
  option [] (bracket "()" $ sepBy type_p colon_sep) -- fixme: donot use type_p
arg_p = (,) <$> many (macro_p arg_macros <* spaces) <*> field_p
arg_macros = words "const _In_opt_ _Out_opt_ _In_reads_bytes_ _Inout_ _In_ _Out_ _Outptr_"

a <$>~ b = a <$> (spaces *> b)
a <*>~ b = a <*> (spaces *> b)
infixl 4 <$>~
infixl 4 <*>~

block_p keyword ctor items = (block_frame $ ctor <$> ident_p <*>~ bracket "{}" items,
                              block_frame $ flip ctor <$> bracket "{}" items <*>~ ident_p)
  where block_frame = fmap inj . until_char ';' . kw keyword

bracket [a,b] = between (char a <* spaces) (spaces *> char b)
colon_list = flip sepEndBy1 colon_sep
colon_sep = try $ spaces *> char ',' <* spaces
until_char c p = p <* many (satisfy (/=c)) <* char c

c_parse p (runP p () "" -> Right r) = r

----------------generator---------------------------
class GenH decl where
  to_h :: (?decls :: [Decl]) => decl -> String

instance (GenH f, GenH g) => GenH (f :+: g) where
  to_h (Inl f) = to_h f
  to_h (Inr f) = to_h f

instance GenH CEnum where
  to_h (CEnum name values) = do
    "-- Enum " ++ name
    "type " ++ name ++ " = DWORD"
    unlines $ map write_field values
    where write_field (upperIdent -> n, v) = do
            "type " ++ n ++ " = " ++ v
            lowerIdent n ++ " = " ++ v ++ " :: " ++ name

instance GenH Struct where
  to_h struct@(Struct oname@(upperIdent -> name) fields) = do
    inner_decl @Struct Proxy
    inner_decl @Union Proxy
    "-- Struct " ++ oname
    "data " ++ name ++ " = " ++ inner_struct_to_h name items
    "  deriving (Show, Generic)"
    "instance Default " ++ name
    "instance Storable " ++ name ++ " where"
    "  sizeOf _ = sizeOfStruct " ++ align_size_codes
    "  alignment _ = alignmentOfStruct " ++ align_size_codes
    poke_struct_code struct
    "  peek = auto_peek " ++ name
    "\n"
    where inner_decl :: (GenH a, a :<: StructItem) => Proxy a -> String
          inner_decl proxy = unlines . map to_h . rejs proxy $ fields
          items = struct_items fields
          align_size_codes = align_size_code items

poke_struct_code (Struct (upperIdent -> name) fields) = do
  concat ["  poke ptr_ ", name, "{..} = "]
  concat ["    void $ ", intercalate " >=> " . map (("poke_ahead "++).lowerIdent.item_name) $ fields, " $ ptr_"]
  where item_name = lowerIdent . head . splitOn "[" . field_name <:+:> union_name <:+:> struct_name
align_size_code = (++"]").("["++).intercalate ", " . map (("(alignment &&& sizeOf) (undefined :: "++).(++")").snd) 
inner_struct_to_h' (Struct name fields) = inner_struct_to_h (upperIdent name) (struct_items fields)
inner_struct_to_h name items = do
  name ++ " {"
  intercalate ", \n" . map (("  " ++) . (\(a, b) -> a ++ " :: " ++ b)) $ items
  "}"

struct_items :: (?decls :: [Decl]) => [StructItem] -> [(String, String)]
struct_items = map $ gen_field <:+:> gen_union <:+:> gen_struct
  where gen_field (Field t name) | last name == ']' = 
          (lowerIdent name1, "StaticArray UArray " ++ length ++ " " ++ to_h t)
          where [name1, length] = splitOn "[" $ init name
        gen_field (Field t name) = (lowerIdent name, to_h t)
        gen_union (Union t _) = (lowerIdent t, upperIdent t)
        gen_struct (Struct t _) = (lowerIdent t, upperIdent t)

instance GenH Union where
  to_h (Union oname@(upperIdent -> name) structs) = do
    "-- Union " ++ oname
    "data " ++ name ++ " = " ++ (intercalate " | " . map inner_struct_to_h' $ structs)
    "  deriving (Show, Generic)"
    "instance Default " ++ name ++ " where def = undefined"
    "instance Storable " ++ name ++ " where"
    "  sizeOf _ = maximum . map sizeOfStruct $ " ++ align_size_codes
    "  alignment _ = maximum . map alignmentOfStruct $ " ++ align_size_codes
    -- cannot auto peek union
    "  peek = undefined"
    unlines . map poke_struct_code $ structs
    where align_size_codes = (++"]") . ("["++) . intercalate ", " . map (align_size_code . struct_items . struct_fields) $ structs


instance GenH Func where
  to_h (Func t oname@(lowerIdent -> name) args) = do
    -- import interface
    "-- Func " ++ oname ++ "\n"
    "foreign import ccall unsafe \"" 
    oname
    "\" c_"
    oname
    " :: \n  "
    raw_sign t args
    "\n\n"

    -- result wrapper
    if t == Type "HRESULT" then do
      name
      "'"
      args_code
      " = \n  convert_result \""
      name
      "\" $ "
      name
      args_code
      "\n\n"
    else ""
    -- wrapper
    name
    args_code
    " = "
    -- alloc params
    concatMap allocPtrCode . filter (is_ht_ptr.fst) $ htypes
    -- raw call
    if any (is_ht_out . fst) htypes then do
      "do\n  ret_ <- "
      raw_call
      "\n"
      -- collectOutput and return
      unlines . collectOutput $ htypes
      "  return "
      returnArgs htypes
    else "\n  " ++ raw_call
    "\n\n"
    where (>>) = (++)
          htypes = to_htypes args
          raw_call = do { "c_"; oname; rawParamsCode htypes}
          args_code = concatMap ((' ':) . lowerIdent . field_name . snd) . 
                      map snd . filter is_in_arg . zip htypes $ args
          is_in_arg ((x,_),_) = x `notElem` [OUT, STR OUT, BSIZE]

instance GenH FuncType where
  to_h (FuncType t name args) = do
    let sign = raw_sign t args
    "-- FuncType " ++ name
    "type " ++ name ++ " = " ++ sign
    "foreign import ccall \"wrapper\" mk0" ++ name ++ " :: " ++ name ++ " -> IO (FunPtr " ++ name ++ ")"
    -- todo: size, bool, string, option
    "mk" ++ name ++ " fun_ = mk0" ++ name ++ " $ \\" ++ arg_names ++ " -> do"
    intercalate "\n" . map (\(field_name -> name) -> "  " ++ name ++ "_v <- peek " ++ name ++ "") . filter (is_ptr . field_type) . map snd $ args
    "  fun_" ++ concatMap ((" "++) . uncurry (++) . fmap (bool "" "_v") . (field_name &&& is_ptr . field_type) . snd) args
    ""
    where arg_names = concatMap ((" "++).field_name.snd) args

allocPtrCode (ht, Field _ name) = ("\n  " ++) . (++x) . uncurry (++) . fmap ($ (" "++name)) . allocPtrFunc $ ht
  where x = concat [" $ \\", name, "_ptr -> "]
allocPtrFunc x | x `elem` [PTR, INOUT] = ("withPtr", id)
allocPtrFunc (STR x) | x == IN = ("withTString", id)
allocPtrFunc OUT = ("alloca'", const " ")
-- outstr, inoutstr not impl
allocPtrFunc OPTPTR = ("withMaybe", id)
allocPtrFunc (STR OPT) = ("withTStringMaybe", id)
allocPtrFunc BUFFER = ("withByteStringPtr", id)

rawParamsCode = concatMap $ (" "++) . rawParams
rawParams (BSIZE, Field _ name) = concat ["(fromIntegral . sizeOf $ ", name, ")"] -- todo: withTStringLen
rawParams (OPT, Field _ name) = concat ["(fromMaybe 0 ", name, ")"]
rawParams (is_ht_ptr -> True, Field _ name) = name ++ "_ptr"
rawParams (_, Field _ name) = name
collectOutput = map (\(field_name . snd -> name) -> concat ["  ", name, "_ret_ <- peek ", name, "_ptr"]) . filter (is_ht_out.fst)
returnArgs args = case filter (is_ht_out.fst) args of
  [] -> "ret_"
  [a] -> concat ["(ret_, ", ret a, ")"]
  list -> concat ["(ret_, (", intercalate ", " . fmap ret $ list, "))"]
  where ret = (++"_ret_") . field_name . snd

raw_sign ret_type args = do
  concatMap ((++ " -> ") . to_h . Type . field_type . snd) args
  if ' ' `elem` h then
    "IO (" ++ h ++ ")"
  else "IO " ++ h
  where (>>) = (++)
        h = to_h ret_type

is_wstring = isSuffixOf "WSTR"
to_htypes :: (?decls :: [Decl]) => [Arg] -> [(HType, Field)]
to_htypes = go []
  where go _ [] = []
        go sizes ((attrs, Field (Type t) n):xs) = (: go nsizes xs) . is_string . maybe (IN, nfield) snd . find fst $ [
            (is_size_ref, (BSIZE, Field (Type t2) size_ref)),
            (is_opt && is_ptr t, (OPTPTR, nfield)),
            (is_opt, (OPT, nfield)),
            (is_inout, (INOUT, nfield)),
            (is_out, (OUT, nfield)),
            (is_buffer, (BUFFER, nfield)),
            (is_ptr t, (PTR, nfield))]
          where size_arg = find ((== "_In_reads_bytes_").macro_name) attrs
                nsizes = case size_arg of 
                  Just (Macro _ [name]) -> (name, n) : sizes
                  Nothing -> sizes
                (is_size_ref, size_ref) = (isJust &&& fromJust) $ snd <$> find ((== n) . fst) sizes
                is_opt = any ((=="_In_opt_").macro_name) attrs
                is_out = any ((`elem` words "_Out_opt_ _Out_ _Outptr_") . macro_name) attrs
                is_inout = any ((=="_Inout_").macro_name) attrs
                is_buffer = isJust size_arg && t2 == "ByteString"
                is_string = bool id (first STR) $ t2 == "String"
                t2 = wrap_type t
                nfield = Field (Type t2) n
        wrap_type x | is_wstring x = "String"
        wrap_type x | is_void x = "ByteString"
        wrap_type "size_t" = "ULONG64"
        wrap_type x | is_ptr x = wrap_type (un_ptr x)
        wrap_type x = upperIdent x

is_ht_ptr = (`elem` [PTR, BUFFER, OPTPTR, OUT, INOUT, STR IN, STR OUT, STR OPT, STR INOUT])
is_ht_out = (`elem` [OUT, INOUT, STR OUT, STR INOUT])
is_ptr t = last t == '*'
un_ptr = dropWhileEnd isSpace . init
is_void t = t == "VOID" || t == "void"
is_func_ptr :: String -> [Decl] -> Bool
is_func_ptr name = foldr (\x acc -> (|| acc) . maybe False ((==name) . func_type) . rej $ x) False 
ptr_name name d | is_func_ptr name d = "FunPtr" | otherwise = "Ptr"
instance GenH Type where
  to_h (Type t) = case t of
    "size_t" -> "ULONG64"
    t | is_void t -> "()"
    p | is_ptr p -> (ptr_name t ?decls ++) . (" " ++) . to_h . Type $ t where t = un_ptr p
    t -> upperIdent t
upperIdent, lowerIdent :: String -> String
upperIdent (x:xs) = toUpper x : xs
lowerIdent (x:xs) = toLower x : xs

(>>) a b = a ++ "\n" ++ b

reserve_field_name f@(Field t name) 
  | name `elem` reserve_words = Field t $ name ++ "0"
  | otherwise = f
reserve_words = words "pattern type"

c_header_to_haskell :: String -> String
c_header_to_haskell = unlines . (add:) . to_h' . prewalk reserve_field_name . c_parse file_p
  where add = do
          "-- Warning: this file is automatically generated by BindGen.\n"
          "{-# LANGUAGE DuplicateRecordFields, DataKinds, CPP, RecordWildCards, DeriveGeneric #-}"
          "{- HLINT ignore -}"
          "module System.Win32.ProjFS ("
          " module System.Win32.ProjFS.Types, "
          " module System.Win32.ProjFS.Helper, "
          " module System.Win32.ProjFS.GenDir, "
          " module System.Win32.ProjFS) where"
          "import System.Win32.Types"
          "import Data.Maybe"
          "import Data.Default"
          "import GHC.Generics"
          "import Control.Monad"
          "import Control.Arrow"
          "import Foreign.Ptr"
          "import Foreign.Storable"
          "import Foreign.Marshal.StaticArray"
          "import Foreign.Marshal.Alloc"
          "import Foreign.C.Types"
          "import Data.Array.Unboxed"
          "import System.Win32.ProjFS.Types"
          "import System.Win32.ProjFS.Helper"
          "import System.Win32.ProjFS.GenDir"
          "import qualified Data.ByteString as BS"
          ""
        to_h' d = let ?decls = d in fmap to_h d
c_header_to_haskell_file :: FilePath -> FilePath -> IO ()
c_header_to_haskell_file src dst = c_header_to_haskell <$> readFile src >>= writeFile dst

ifThenElse b x y = bool y x b

-- c_header_to_haskell_file "projectedfslib.h" "src/ProjFS.hs"
