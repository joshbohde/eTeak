-- | Operations for both Go & Balsa Types

module Language.SimpleGo.Balsa.Types (
  TypeDeclaration(..),
  TypeError(..),
  TypeNamespace(..),
  -- * Declaration
  -- $declaration
  declareBuiltins,
  declareNamedType,
  canonicalType,
  -- * Type Checking
  -- $typechecking
  (=?)
  ) where

import           Prelude                          hiding (lookup)
import qualified Prelude

import           Control.Monad                    (forM_, unless)

import qualified Language.SimpleGo.AST            as AST
import           Language.SimpleGo.AST.Name       (Name, name)
import qualified Language.SimpleGo.Balsa.Builtins as BalsaBuiltins
import qualified Language.SimpleGo.Types          as GoTypes
import qualified ParseTree                        as PT
import qualified Report                           as R

data TypeDeclaration = TypeDeclaration GoTypes.Type PT.TypeBody
                       deriving (Eq, Show)


data TypeError = Unfound Name
               | AlreadyDeclared Name
               | Incompatible GoTypes.Type GoTypes.Type
               | IncompatibleUntyped GoTypes.Type GoTypes.UnTyped
               | UnsupportedType AST.Type
               deriving (Show, Eq)

{-|

'TypeNamespace' captures the operations neccessary to do type
declaration & checking independent of some base 'Monad'

-}

class (Monad m) => TypeNamespace m where
  {-|
Lookup declarations. This is expected to throw an `Unfound` if the
declaration is not found.
   -}
  lookup :: Name -> m TypeDeclaration


  {-| Make a declaration. This is expected to throw an `AlreadyDeclared` if the
    declaration already exists.
  -}
  declare :: Name -> TypeDeclaration -> m ()

  {-| Throw a 'TypeError'
  -}
  typeError :: TypeError -> m a


{- $declaration -}

{-|
    An opaque operation to define all builtin types in Go, assuming they
    have a mapping to Balsa.
-}
declareBuiltins :: (TypeNamespace m) => m ()
declareBuiltins = forM_ GoTypes.primitives $ \(n, t) ->
    case Prelude.lookup n BalsaBuiltins.types of
      (Just bt) -> declare (name n) $ TypeDeclaration t $ PT.AliasType R.NoPos bt
      Nothing -> return ()

{-|
    Declare the common pattern of  `type a int`.
-}
declareNamedType :: (TypeNamespace m) => Name -> AST.Type -> PT.TypeBody -> m ()
declareNamedType n t balsaBody = do
    goType <- canonicalType t
    declare n $ TypeDeclaration (GoTypes.Named n goType) balsaBody


canonicalType :: (TypeNamespace m) => AST.Type -> m GoTypes.Type
canonicalType = go
  where
    goSig (AST.Signature ins var outs) = GoTypes.Signature <$> traverse paramType ins <*> traverse paramType var <*> traverse paramType outs
    paramType (AST.Param _ t) = go t
    go (AST.TypeName (AST.Id i)) = do
      (TypeDeclaration t _) <- lookup (name i)
      return t
    -- only support static sizes at this point
    go (AST.ArrayType (AST.Prim (AST.LitInt i)) t) = GoTypes.Array i <$> go t
    go (AST.Channel k t) = GoTypes.Chan k <$> go t
    go (AST.FunctionType sig) = GoTypes.Func <$> goSig sig
    go (AST.MapType t t') = GoTypes.Map <$> go t <*> go t'
    go (AST.PointerType t) = GoTypes.Ptr <$> go t
    go (AST.SliceType t) = GoTypes.Slice <$> go t
    go (AST.Struct fields) = GoTypes.Struct <$> traverse (traverse go) fields

    go t = typeError $ UnsupportedType t


{- $typechecking -}

{-|

Check if the lhs can be assigned the type of the rhs, throwing an error if they cannot.

Example usage:

@
('AST.TypeName' "byte") =? ('Right' ('AST.TypeName' "string"))
@

-}

(=?) :: (TypeNamespace m) => GoTypes.Type -> Either GoTypes.UnTyped GoTypes.Type -> m ()
lhs =? rhs = case rhs of
    Left untype -> unless (GoTypes.canTypeAs untype lhs) $
      typeError $ IncompatibleUntyped lhs untype
    Right rht -> unless (GoTypes.assignableTo rht lhs) $
        typeError $ Incompatible lhs rht


isIntegral :: Either GoTypes.UnTyped GoTypes.Type -> Bool
isIntegral (Left u) = GoTypes.canTypeAs u (GoTypes.Numeric GoTypes.GoInt)
isIntegral (Right t) =  GoTypes.convertibleTo (GoTypes.Numeric GoTypes.GoInt) t
