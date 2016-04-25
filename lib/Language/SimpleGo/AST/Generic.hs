{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
-- |

module Language.SimpleGo.AST.Generic (
  Program(..),
  Id(..),
  Declaration(..),
  Rec(..),
  Signature(..),
  Param(..),
  Type(..),
  ChanKind(..),
  Expr(..),
  Prim(..),
  Value(..),
  Block(..),
  Statement(..),
  Simp(..),
  Chan(..),
  Cond(..),
  Case(..),
  ) where

import           Data.Functor.Identity           (Identity)
import qualified Data.Text                       as T
import qualified Data.Vector                     as U
import qualified Language.SimpleGo.AST.Operators as Operators

newtype PackageName = PackageName { unPackage :: T.Text } deriving (Eq, Show)

data Program a = Program {
      declarations :: U.Vector (Declaration a)
      } deriving (Eq, Show, Read, Functor, Foldable, Traversable)

newtype Id = Id { idText :: T.Text } deriving (Eq, Show, Read)


data Declaration a = Const Id (Type a) a
                 | Var Id (Type a) a
                 | Type Id (Type a)
                 | Func Id (Signature a) (Block a)
                 deriving (Eq, Read, Show, Functor, Foldable, Traversable)

data Rec a = Rec Bool (Maybe Id) (Type a)
         deriving (Eq, Read, Show, Functor, Foldable, Traversable)

data Signature a = Signature {
  input  :: U.Vector (Param a),
  output :: U.Vector (Param a)
  } deriving (Eq, Read, Show, Functor, Foldable, Traversable)

data Param a = Param (Maybe Id) (Type a)
           deriving (Eq, Read, Show, Functor, Foldable, Traversable)

data Type a = TypeName Id
          | ArrayType a (Type a)
          | Channel ChanKind (Type a)
          | FunctionType (Signature a)
          | MapType (Type a) (Type a)
          | PointerType (Type a)
          | SliceType (Type a)
          | Struct [(Id, Type a)]
          | EllipsisType (Type a) -- only in Literals
          | VariadicType (Type a) -- only in Funcs
          deriving (Eq, Read, Show, Functor, Foldable, Traversable)


data ChanKind = Input         -- <-chan
              | Output        -- chan<-
              | Bidirectional -- chan
              deriving (Eq, Read, Show)

data Expr a = Zero
          | Prim (Prim a)
          | UnOp Operators.Unary a
          | BinOp Operators.Binary a a
          deriving (Eq, Read, Show, Functor, Foldable, Traversable)

data Prim a = LitInt  !Integer
          | LitReal !Float
          | LitImag !Float
          | LitChar !Char
          | LitStr  !T.Text
          | LitFunc (Signature a) (Block a)
          | LitComp (Type a) [a]
          | Qual (Maybe Id) Id                              -- 'PrimaryExpr/Operand/QualifiedIdent'
          | Method (Rec a) Id                        -- 'PrimaryExpr/Operand/MethodExpr'
          | Paren a                           -- 'PrimaryExpr/Operand/MethodExpr'
          | Cast (Type a) a                       -- 'PrimaryExpr/Conversion'
          | New  (Type a)                            -- 'PrimaryExpr/BuiltinCall/new'
          | Make (Type a) [a]                     -- 'PrimaryExpr/BuiltinCall/make'
                                                 --          | BI Id Type [Expr]  -- 'PrimaryExpr/BuiltinCall'
          | Select (Prim a) Id                       -- 'PrimaryExpr/Selector'
          | Index (Prim a) a                      -- 'PrimaryExpr/Index'
          | Slice (Prim a) (Maybe a) (Maybe a) -- 'PrimaryExpr/Slice'
          | TA    (Prim a) (Type a)                      -- 'PrimaryExpr/TypeAssertion'
          | Call  (Prim a) [a] (Maybe a) -- The final expression is a variadic call
          deriving (Eq, Read, Show, Functor, Foldable, Traversable)

data Comp a = Comp [Element a]
          deriving (Eq, Read, Show, Functor, Foldable, Traversable)

data Element a = Element (Key a) (Value a)
             deriving (Eq, Read, Show, Functor, Foldable, Traversable)

data Key a = KeyNone
         | KeyField Id
         | KeyIndex a
         deriving (Eq, Read, Show, Functor, Foldable, Traversable)

data Value a = ValueExpr a -- 'Value/Expression'
           | ValueComp (Comp a) -- 'Value/LiteralValue'
           deriving (Eq, Read, Show, Functor, Foldable, Traversable)

newtype Block a = Block (U.Vector (Statement a))
              deriving (Eq, Read, Show, Functor, Foldable, Traversable)

data Statement a = StmtDecl (Declaration a) -- 'Statement/Declaration'
               | Simple (Simp a)
               | Go a
               | Return [a]
               | Break    (Maybe Id)
               | Continue (Maybe Id)
               | Fallthrough
               | StmtBlock (Block a)
               | If (Cond a) (Block a) (Maybe (Statement a))
                 -- TODO rewrite this to a more static case structure, with default
               | StmtSelect  [Case Chan a]
               | Switch (Cond a) [Case Identity a]
               | ForWhile (Maybe a) (Block a)
               | ForThree (Simp a) (Maybe a) (Simp a) (Block a)
                 -- True if AssignDecl
               | ForRange [a] a Bool (Block a)
               deriving (Eq, Read, Show, Functor, Foldable, Traversable)

data Simp a = Empty
          | Send a a
          | SimpleExpr a
          | Inc  a
          | Dec  a
          | SimpVar Id a
          | Assign a a
          deriving (Eq, Read, Show, Functor, Foldable, Traversable)

data Chan a = ChanRecv (Maybe (a, Maybe a, Operators.Unary)) a
          | ChanSend a a
          deriving (Eq, Read, Show, Functor, Foldable, Traversable)

data Cond a = Cond (Maybe (Simp a)) (Maybe a)
          deriving (Eq, Read, Show, Functor, Foldable, Traversable)


data Case f e = Case [f e] [Statement e]
              | Default  [Statement e]
            deriving (Eq, Read, Show, Functor, Foldable, Traversable)
