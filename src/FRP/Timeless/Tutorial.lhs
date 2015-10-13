-- |
-- Module:     FRP.Timeless.Tutorial
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

> {-# LAWNGUAGE Arrows #-}
> module FRP.Timeless.Tutorial where

\section{Introduction}

This module is a tutorial of `timeless` written in Literate Haskell. To run this tutorial, run ghci, import this module, and run 'runTurorial'.

This tutorial steps through the core of `timeless`, and builds a simple console interactive program which makes use of many aspects of 'timeless'


\section{Basics of Timeless}

First, to use anything from `timeless`, import the following module(s):

> import FRP.Timeless

If you don't want to look at basic details, but rather like to go straight for the application, skip this section.

`Signal` is the core of the `timeless` library. It has type:

< Signal s m a b

Four type parameters! However, for the most of the time, what only matters are the `a` and `b`, as you will see.

All `Signal`s are `Arrows`, which can be thought as generalized functions (not exactly, because they do a lot more). If you look at any arrow related functions from `Control.Arrow`, they typically look like this:

< arr :: Arrow a => (b -> c) -> a b c

In the case of `Signal`, the arrow is `Signal s m`. The `a` is the input, and `b` is the output. From this aspect, it looks just like a function. However, `Signal` does much more than a function: it can keep state, chain monadic actions nicely with pure functions, and can be activated or inhibited.

I mentioned "monadic actions". So that is the `m` in the type parameter. In fact, in most functions, `Signal` has type

< (Monad m) => Signal s m a b

Usually, `m` will be `IO`. However, I recommend to only put `IO` explicitly in the outermost signals or those actually does `IO` actions, to make the constructed signals more reusable.

`s` is just a "session" type that holds delta time information in running the signals. This information can be used in constructing signals, but is invisible in composition.


\section{The Example Application}

The goal of this application is to demonstrate a simple console interactive program. The program reads user's name from standard input. As the user type, the name is updated inside a greeting sentence, for example:

    Hello, Rongcui Dong!

Then, when the user presses return, another greeting replaces the previous one:

    Greetings, Rongcui Dong!

If the user types "quit" or "q", case non-sensitive, and then presses return, the program quits. The process of implementation is broken into several parts.


\subsection{Echo Input}

First, we create a program that simply echos user input, to make sure that both input and output works. To see the final result of this section, run `runEcho` in ghci.

As will be demonstrated below, usually we write a pure or monadic function, and then lift the function into a `Signal` using various factory functions in `FRP.Timeless.Prefab` (automatically imported). 

To make sure no garbage is displayed on console, and to give opportunity for immediate response, we turn off buffering and echo:

> initConsole :: IO ()
> initConsole = do
>   hSetEcho stdin False
>   hSetBuffering stdin NoBuffering
>   hSetBuffering stdout NoBuffering

Of course, this is the first thing we will do in `runEcho`. Then, look at this function in `FRP.Timeless.Run`:

< runBox :: (Monad m) => Session m s -> Signal s m () () -> m ()

This function basically runs a "black box" signal network, and stops when the output inhibits. /Black box/, of course, means that the `Signal` runned has no observable input and output. Since everything is sealed inside the box, any `IO` actions should be nicely isolated from other code.

Now, `runEcho` is defined as following:

> runEcho :: IO ()
> runEcho = do
>   initConsole
>   runBox clockSession_ sEchoBox

Here, `clockSession_` is something that can pass real delta time (in seconds) information into the signal network (implicitly, if you read the previous section). If it is replaced by `countSession_ n`, then the delta time will be the fixed integer `n`. In this tutorial, all `Signal`s have name starting with an /s/, such as the `sEchoBox` here. Any signal with /Box/ as suffix means that the runner do not know anything about the internal of the signal.

First, we prepare the input function. To make things simpler for now, let's consider a /blocking input/, which blocks the program until input is detected:

> inputBlocking :: IO Char
> inputBlocking = getChar

Very simple. However, this is an `IO` action, not a `Signal`. Therefore, we make it a `Signal`:

> sInputBlocking :: Signal s IO a Char
> sInputBlocking = mkActM inputBlocking

`mkActM` lifts a monadic action into a `Signal`. Although GHC's type inference should perform well in determining the type of the signal, but it is much easier to explicitly write it out so that when you make a mistake, the debug message gets much more readable. Now I will explain the type of this signal.

< Signal s IO a Char

`s` is the implicit `Session` type, which can be thought of as "time" (and it is "timeless"!). `IO` is explicitly written out because `inputBlocking` is an `IO` action. `a` here is any type, and `Char` is the output type. To start out, imagine a function like this:

< f :: a -> IO Char

The only thing this function do is to get a `Char` from somewhere else (`RealWorld`), because there is no way we can get a `Char` from an `a` of unknown type. Therefore, `a` as an input is completely ignored. (Actually, `mkActM` is a synonym of `mkConstM`, implying that is behaves somewhat like a constant)

Next, we need something for output:

> output :: Char -> IO ()
> output = putChar

Similarly, we lift it into a signal:


