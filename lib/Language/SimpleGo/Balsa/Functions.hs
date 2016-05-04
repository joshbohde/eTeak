{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
-- |

module Language.SimpleGo.Balsa.Functions where

{-

Operations to rewrite functions so that they can be translated to Balsa.

Balsa's memory model is more restrictive than Go's in that it does not
allow variables to cross procedure boundaries. The only way to send a
variable is through input and output.

Balsa has two notions of callables:

  functions, which take input and return a single expression and return a single value

  procedures, which take static parameters, input and output channels, and return void.

Balsa functions are essentially macros.

We can emulate this though with channels. Given the following example code:

func inc(a int) int {
    return a + 1
}

We can rewrite it to be procedure like:

func(a <-chan int, output chan<- int){
    tmp := <-a
    output <- tmp + 1
}

We will need to rewrite the caller like from:

m := inc(n)

to:

inc_in := make(chan int)
inc_out := make(chan int)
go inc(inc_in, inc_out)
inc_in <- n
m := <- inc_out

There are more complications with regards to multiple return values.

func multi(a int, b byte) (byte, int) {
    return (b, int)
}

c, d := multi(0, 2)

Translates to:

type multi_in {tmp1 int, tmp2 byte}
type multi_out {tmp3 byte, tmp4 int}

func multi(tmp5 <-chan multi_in, tmp6 chan<- multi_out){
     tmp7 := <- tmp5
     a := tmp7.tmp1
     b := tmp7.tmp2
     tmp6 <- multi_out{b, a}
}

-}

import           Data.Maybe                 (mapMaybe)
import           Data.Monoid                ((<>))
import           Language.SimpleGo.AST      (ChanKind (Input, Output), Id (..),
                                             Param (Param),
                                             Signature (Signature))
import           Language.SimpleGo.AST.Name (name)
import qualified Language.SimpleGo.Types    as Types

data In t = Passthrough t
          | Bundle t
          deriving (Eq, Show, Functor, Foldable, Traversable)

data Out t = Out t
           deriving (Eq, Show, Functor, Foldable, Traversable)

data Args t = Args {
  -- The bundled record of non channels args
  inputs  :: [In t],
  -- The bundled record of non channels outputs
  outputs :: [Out t]
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance Monoid (Args t) where
  mempty = Args [] []
  (Args i o) `mappend` (Args i' o') = Args (i <> i') (o <> o')

signatureArgs :: Signature Types.Type -> Args (Param Types.Type)
signatureArgs (Signature i _ o) = input i <> output o
  where
    mkArg (Param n (Types.Chan Input t)) = Args [Passthrough (Param n t)] []
    mkArg (Param n (Types.Chan Output t)) = Args [Passthrough (Param n t)] []
    mkArg p = Args [Bundle p] []

    input = foldMap mkArg

    output = foldMap g
      where
        g t = Args [] [Out t]

-- A data type representing how the args to a function should be converted for a procedure
data ArgConvention =
  -- Nothing to be done, the procedure can take these args as is
  Empty
  -- A single argument to the procedure needs to be passed in as a chan
  | Single Int (Param Types.Type)
  -- A named group of arguments needs to be bundled, passed in a chan, and then unbundled
  | Bundled Id [(Id, Int, Param Types.Type)]
  deriving (Eq, Show)

conventions :: (Monad m) => m Id -> Args (Param Types.Type) -> m (ArgConvention, ArgConvention)
conventions fresh (Args ins outs) = do
  ind <- procDecl (mapMaybe bundled (indexed ins))
  outd <- procDecl $ indexed $ map unOut outs
  return (ind, outd)
  where
    indexed = zip [0..]
    unOut (Out p) = p

    bundled (i, Bundle p) = Just (i, p)
    bundled _ = Nothing

    procDecl [] = return Empty
    procDecl [(i, p)] = return $ Single i p
    procDecl ps = Bundled <$> fresh <*> traverse field ps
      where
       field (i, p) = do
         name' <- fresh
         return (name', i, p)

-- The instantiated type of the args
argType :: ArgConvention -> Maybe Types.Type
argType Empty = Nothing
argType (Single _ (Param _ t))  = Just t
argType (Bundled (Id id') fs) = Just $ Types.Named (name id') struct
  where
    struct = Types.Struct $ map (\(i, _, Param _ t) -> (i, t))  fs

-- If we would need to make a new type, return enough info here to declare it
newType :: ArgConvention -> Maybe (Id, Types.Type)
newType Empty = Nothing
newType (Single _ _)  = Nothing
newType (Bundled id' fs) = Just (id', struct)
  where
    struct = Types.Struct $ map (\(i, _, Param _ t) -> (i, t))  fs
