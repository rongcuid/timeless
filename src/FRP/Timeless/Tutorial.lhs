-- |
-- Module:     FRP.Timeless.Tutorial
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

> {-# LANGUAGE Arrows #-}
> module FRP.Timeless.Tutorial where
>
> import System.IO

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

> sInput :: Signal s IO () Char
> sInput = mkActM inputBlocking

`mkActM` lifts a monadic action into a `Signal`. Although GHC's type inference should perform well in determining the type of the signal, but it is much easier to explicitly write it out so that when you make a mistake, the debug message gets much more readable. Now I will explain the type of this signal.

< Signal s IO a Char

`s` is the implicit `Session` type, which can be thought of as "time" (and it is "timeless"!). `IO` is explicitly written out because `inputBlocking` is an `IO` action. `()` means that this signal should not take any inputs, and `Char` is the output type. To start out, imagine a function like this:

Next, we need something for output:

> output :: Char -> IO ()
> output = putChar

Similarly, we lift it into a signal:

> sOutput :: Signal s IO Char ()
> sOutput = mkKleisli_ output

The `output` function has type `Char -> IO ()`, or `a -> m b`, which is a Kleisli function. `mkKleisli_` lifts a Kleisli function into a `Signal`. Now `sOutput` will magically print characters on screen when input is supplied!

Now, we have the input and output signal, and we are ready to create the black box!

> sEchoBox :: Signal s IO () ()
> sEchoBox = sInput >>> sOutput

Since it is just an "echo box", we simply direct the input to output. `(>>>)` is a function in `Control.Arrow`:

< (>>>) :: Category cat => cat a b -> cat b c -> cat a c

It may look scary because of the `Category` thing, but `Category` is basically function. If you look carefully, you will notice:

< (.) :: Category cat => cat b c -> cat a b -> cat a c

`(>>>)` is just a flipped `(.)`! So instead of writing `sOutput . sInput`, the syntax looks more like chaining operations. Later we will also introduce the arrow syntax of Haskell, which makes complex logic much easier to compose.

If everything is typed correctly, try `runEcho`! Everything you type should be echoed. Of course, backspace will not work.


\subsection{Non Blocking Input}

Although the program above works, it will be nice if we don't block on input. Here we are using single thread (honestly, I haven't done any concurrency in Haskell yet, so I do not know whether `timeless` will work in concurrent programs), waits for input for 17ms, giving 60 samples per second. (I use the word /sample/ because `timeless` works in continuous time semantics).

To see result of this section, run `runEcho2`:

> runEcho2 :: IO ()
> runEcho2 = do
>   initConsole
>   runBox clockSession_ sEchoBox2


First, we write a monadic action as in normal Haskell program:

> inputNonBlocking :: IO (Maybe Char)
> inputNonBlocking = do
>   b <- hWaitForInput stdin 17
>   case b of
>     True -> fmap Just getChar
>     False -> return Nothing

The function is self explanatory. If input is found, get it. Otherwise, return `Nothing`. Since we have `IO (Maybe Char)` this time, we need another output function:

> outputMay :: Maybe Char -> IO ()
> outputMay = mapM_ putChar

This function is also self explanatory. If you can't read it, try to read some more basic Haskell tutorials or books first. Then, we construct the signals and the box as before:

> sInput' :: Signal s IO () (Maybe Char)
> sInput' = mkActM inputNonBlocking
>
> sOutput' :: Signal s IO (Maybe Char) ()
> sOutput' = mkKleisli_ outputMay
>
> sEchoBox2 :: Signal s IO () ()
> sEchoBox2 = sInput' >>> sOutput'

Now, run `runEcho2` and see what happens. It should look just like before. However, when other parts are included, this version enables other computations to be made when there is no input. The following sections will be built up on the non-blocking version of I/O.


\subsection{Remember the Name}

In this section, we will deal with stateful signals. With these additions, the backspace character will work.
