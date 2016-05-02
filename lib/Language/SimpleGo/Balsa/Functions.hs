{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import qualified Data.Text as T
