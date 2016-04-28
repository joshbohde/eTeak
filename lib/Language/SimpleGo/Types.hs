-- |

module Language.SimpleGo.Types (
       Type(..),
       NumType(..),
       CmplxType(..),
       UnTyped(..),
       primitives,
       assignableTo, convertibleTo,
       canTypeAs, defaultType,
       translate,
       builtinByte
       ) where

import           Prelude               hiding (lookup)

import           Data.Vector           (Vector)

import qualified Language.SimpleGo.AST as AST

data Type = Named String Type
          | Bool
          | Numeric NumType
          | Complex CmplxType
          | String
          | Array Integer Type
          | Slice Type
          | Ptr Type
          | UnsafePtr
          | Chan AST.ChanKind Type
          | Map Type Type
          | Struct [(AST.Id, Type)]
          | Func Signature
          deriving (Show, Eq)

data Signature = Signature (Vector Type) (Vector Type)
               deriving (Show, Eq)


data NumType = Int8 | Int16 | Int32 | Int64
             | Uint8 | Uint16 | Uint32 | Uint64
             | GoInt | GoUint | Uintptr
             | Float32 | Float64
  deriving (Show, Eq)

data CmplxType = Complex64 | Complex128
  deriving (Show, Eq)

builtinByte :: Type
builtinByte = Named "byte" (Numeric Uint8)

builtinRune :: Type
builtinRune = Named "rune" (Numeric Int32)

primitives :: [(String, Type)]
primitives = [
  ("int8", Numeric Int8),
  ("int16", Numeric Int16),
  ("int32", Numeric Int32),
  ("int64", Numeric Int64),
  ("uint8", Numeric Uint8),
  ("uint16", Numeric Uint16),
  ("uint32", Numeric Uint32),
  ("uint64", Numeric Uint64),
  ("int", Numeric GoInt),
  ("uint", Numeric GoUint),
  ("uintptr", Numeric Uintptr),
  ("float32", Numeric Float32),
  ("float64", Numeric Float64),
  ("bool", Bool),
  ("string", String),
  ("byte", builtinByte),
  ("rune", builtinRune)
  ]


data UnTyped = DefaultBool
             | DefaultRune
             | DefaultInt
             | DefaultFloat64
             | DefaultComplex128
             | DefaultString
             deriving (Eq, Show)

-- This isn't strictly right, but will work for now
-- See https://golang.org/ref/spec#Constants for a full implementation
-- We will need to check for overflows, etc here
canTypeAs :: UnTyped -> Type -> Bool
canTypeAs u (Named _ t) = canTypeAs u t
canTypeAs DefaultBool t = convertibleTo Bool t
canTypeAs DefaultRune t = convertibleTo builtinRune t
canTypeAs DefaultInt t = convertibleTo (Numeric GoInt) t
canTypeAs DefaultFloat64 t = convertibleTo (Numeric Float64) t
canTypeAs DefaultComplex128 t = convertibleTo (Complex Complex128) t
canTypeAs DefaultString t = convertibleTo String t

defaultType :: UnTyped -> Type
defaultType DefaultBool = Bool
defaultType DefaultRune = builtinRune
defaultType DefaultInt = Numeric GoInt
defaultType DefaultFloat64 = Numeric Float64
defaultType DefaultComplex128 = Complex Complex128
defaultType DefaultString = String

assignableTo :: Type -> Type -> Bool
assignableTo (Named name1 t1) (Named name2 t2) = name1 == name2 && t1 == t2
assignableTo (Named _ t1@(Chan _ _))
             t2@(Chan _ _) = assignableTo t1 t2
assignableTo t1@(Chan _ _)
             (Named _ t2@(Chan _ _)) = assignableTo t1 t2
assignableTo (Named _ t) u = t == u
assignableTo t (Named _ u) = t == u
assignableTo (Chan AST.Bidirectional t1) (Chan _ t2) = t1 == t2
assignableTo t u = t == u

convertibleTo :: Type -> Type -> Bool
convertibleTo (Named _ t) (Named _ u) = case (t, u) of
  (Numeric _, Numeric _) -> True
  (Complex _, Complex _) -> True
  (t', u') -> t' == u'
convertibleTo (Ptr t) (Ptr u) = underlying t == underlying u
convertibleTo (Numeric _) (Numeric _) = True
convertibleTo (Complex _) (Complex _) = True
convertibleTo t u = assignableTo t u

underlying :: Type -> Type
underlying (Named _ t) = t
underlying t = t

translate :: (Monad m) => (String -> m Type) -> (AST.Id -> m (Either Type AST.Type)) -> AST.Type -> m Type
translate err lookup = go
  where
    goSig (AST.Signature ins outs) = Signature <$> traverse paramType ins <*> traverse paramType outs
    paramType (AST.Param _ t) = go t
    go (AST.TypeName i) = do
      found <- lookup i
      case found of
        Left t -> return t
        Right a -> go a
    -- only support static sizes at this point
    go (AST.ArrayType (AST.Prim (AST.LitInt i)) t) = Array i <$> go t
    go (AST.Channel k t) = Chan k <$> go t
    go (AST.FunctionType sig) = Func <$> goSig sig
    go (AST.MapType t t') = Map <$> go t <*> go t'
    go (AST.PointerType t) = Ptr <$> go t
    go (AST.SliceType t) = Slice <$> go t
    go (AST.Struct fields) = Struct <$> traverse (traverse go) fields
    go t = err $ "can't translate type: " ++ show t
