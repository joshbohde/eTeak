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

import           Data.Maybe              (mapMaybe)
import           Data.Monoid             ((<>))
import           Language.SimpleGo.AST   (ChanKind (Input, Output), Id,
                                          Param (Param), Signature (Signature))
import qualified Language.SimpleGo.Types as Types

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


data ProcedureDecl = Empty -- Nothing to be done
                   -- Just declare a chan of a type
                   | SimpleChan Types.Type
                   -- Declare the data type, then the chan
                   | BundledChan Types.Type

argsDecls :: (Monad m) => m Id -> Args (Param Types.Type) -> m (ProcedureDecl, ProcedureDecl)
argsDecls fresh (Args ins outs) = do
  ind <- procDecl (mapMaybe bundled ins)
  outd <- procDecl (map (unParam . unOut) outs)
  return (ind, outd)
  where
    unParam (Param _ t) = t
    unOut (Out p) = p

    bundled (Bundle p) = Just (unParam p)
    bundled _ = Nothing

    procDecl [] = return Empty
    procDecl [t] = return $ SimpleChan t
    procDecl ts = BundledChan . Types.Struct <$> traverse field ts
      where
       field t = do
         i <- fresh
         return (i,t)
