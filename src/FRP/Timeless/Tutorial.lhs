> -- |
> -- Module:     FRP.Timeless.Tutorial
> -- Copyright:  (c) 2015 Rongcui Dong
> -- License:    BSD3
> -- Maintainer: Rongcui Dong <karl_1702@188.com>

> {-# LANGUAGE Arrows #-}
> module FRP.Timeless.Tutorial where
>
> import System.IO
> import Data.Char (toLower)

\section{Introduction}

This module is a tutorial of `timeless` written in Literate
Haskell. To run this tutorial, run ghci, import this module, and run
'runTurorial'.

In very short terms, `timeless` is an Arrow based Functional Reactive
Programming library which operates in continuous time semantics. You
will get this as tutorial goes.

This tutorial steps through the core of `timeless`, and builds a
simple console interactive program which makes use of many aspects of
'timeless'


\section{Basics of Timeless}

First, to use anything from `timeless`, import the following
module(s):

> import FRP.Timeless
> import Prelude hiding ((.), id) -- I don't see a way to get rid of this yet

If you don't want to look at basic details, but rather like to go
straight for the application, skip this section.

`Signal` is the core of the `timeless` library. It has type:

< Signal s m a b

Four type parameters! However, for the most of the time, what only
matters are the `a` and `b`, as you will see.

All `Signal`s are `Arrows`, which can be thought as generalized
functions (not exactly, because they do a lot more). If you look at
any arrow related functions from `Control.Arrow`, they typically look
like this:

< arr :: Arrow a => (b -> c) -> a b c

In the case of `Signal`, the arrow is `Signal s m`. The `a` is the
input, and `b` is the output. From this aspect, it looks just like a
function. However, `Signal` does much more than a function: it can
keep state, chain monadic actions nicely with pure functions, and can
be activated or inhibited.

I mentioned "monadic actions". So that is the `m` in the type
parameter. In fact, in most functions, `Signal` has type

< (Monad m) => Signal s m a b

Usually, `m` will be `IO`. However, I recommend to only put `IO`
explicitly in the outermost signals or those actually does `IO`
actions, to make the constructed signals more reusable.

`s` is just a "session" type that holds delta time information in
running the signals. This information can be used in constructing
signals, but is invisible in composition.


\section{The Example Application}

The goal of this application is to demonstrate a simple console
interactive program. The program reads user's name from standard
input. As the user type, the name is updated inside a greeting
sentence, for example:

    Hello, Rongcui Dong! Greetings!

Then, when the user presses return, the second greeting also gets
updated

    Hello, Rongcui Dong! Greetings, Rongcui Dong!

If the user types "quit" or "q", case non-sensitive, and then presses
return, the program quits. The process of implementation is broken
into several parts.

To test final results now, run `runTutorial` in GHCi

\subsection{Echo Input}

First, we create a program that simply echos user input, to make sure
that both input and output works. To see the final result of this
section, run `runEcho` in ghci.

As will be demonstrated below, usually we write a pure or monadic
function, and then lift the function into a `Signal` using various
factory functions in `FRP.Timeless.Prefab` (automatically imported).

To make sure no garbage is displayed on console, and to give
opportunity for immediate response, we turn off buffering and echo:

> -- * Read the Source for This Tutorial!
> initConsole :: IO ()
> initConsole = do
>   hSetEcho stdin False
>   hSetBuffering stdin NoBuffering
>   hSetBuffering stdout NoBuffering

Of course, this is the first thing we will do in `runEcho`. Then, look
at this function in `FRP.Timeless.Run`:

< runBox :: (Monad m) => Session m s -> Signal s m () () -> m ()

This function basically runs a "black box" signal network, and stops
when the output inhibits. /Black box/, of course, means that the
`Signal` runned has no observable input and output. Since everything
is sealed inside the box, any `IO` actions should be nicely isolated
from other code.

Now, `runEcho` is defined as following:

> runEcho :: IO ()
> runEcho = do
>   initConsole
>   runBox clockSession_ sEchoBox

Here, `clockSession_` is something that can pass real delta time (in
seconds) information into the signal network (implicitly, if you read
the previous section). If it is replaced by `countSession_ n`, then
the delta time will be the fixed integer `n`. In this tutorial, all
`Signal`s have name starting with an /s/, such as the `sEchoBox`
here. Any signal with /Box/ as suffix means that the runner do not
know anything about the internal of the signal.

First, we prepare the input function. To make things simpler for now,
let's consider a /blocking input/, which blocks the program until
input is detected:

> inputBlocking :: IO Char
> inputBlocking = getChar

Very simple. However, this is an `IO` action, not a
`Signal`. Therefore, we make it a `Signal`:

> sInput :: Signal s IO () Char
> sInput = mkActM inputBlocking

`mkActM` lifts a monadic action into a `Signal`. Although GHC's type
inference should perform well in determining the type of the signal,
but it is much easier to explicitly write it out so that when you make
a mistake, the debug message gets much more readable. Now I will
explain the type of this signal.

< Signal s IO a Char

`s` is the implicit `Session` type, which can be thought of as "time"
(and it is "timeless"!). `IO` is explicitly written out because
`inputBlocking` is an `IO` action. `()` means that this signal should
not take any inputs, and `Char` is the output type. To start out,
imagine a function like this:

Next, we need something for output:

> output :: Char -> IO ()
> output = putChar

Similarly, we lift it into a signal:

> sOutput :: Signal s IO Char ()
> sOutput = mkKleisli_ output

The `output` function has type `Char -> IO ()`, or `a -> m b`, which
is a Kleisli function. `mkKleisli_` lifts a Kleisli function into a
`Signal`. Now `sOutput` will magically print characters on screen when
input is supplied!

Now, we have the input and output signal, and we are ready to create
the black box!

> sEchoBox :: Signal s IO () ()
> sEchoBox = sInput >>> sOutput

Since it is just an "echo box", we simply direct the input to
output. `(>>>)` is a function in `Control.Arrow`:

< (>>>) :: Category cat => cat a b -> cat b c -> cat a c

It may look scary because of the `Category` thing, but `Category` is basically function. If you look carefully, you will notice:

< (.) :: Category cat => cat b c -> cat a b -> cat a c

`(>>>)` is just a flipped `(.)`! So instead of writing `sOutput
. sInput`, the syntax looks more like chaining operations. Later we
will also introduce the arrow syntax of Haskell, which makes complex
logic much easier to compose.

If everything is typed correctly, try `runEcho`! Everything you type
should be echoed. Of course, backspace will not work.


\subsection{Non Blocking Input}

Although the program above works, it will be nice if we don't block on
input. Here we are using single thread (honestly, I haven't done any
concurrency in Haskell yet, so I do not know whether `timeless` will
work in concurrent programs), waits for input for 17ms, giving 60
samples per second. (I use the word /sample/ because `timeless` works
in continuous time semantics).

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

The function is self explanatory. If input is found, get
it. Otherwise, return `Nothing`. Since we have `IO (Maybe Char)` this
time, we need another output function:

> outputMay :: Maybe Char -> IO ()
> outputMay = mapM_ putChar

This function is also self explanatory. If you can't read it, try to
read some more basic Haskell tutorials or books first. Then, we
construct the signals and the box as before:

> sInput' :: Signal s IO () (Maybe Char)
> sInput' = mkActM inputNonBlocking
>
> sOutput' :: Signal s IO (Maybe Char) ()
> sOutput' = mkKleisli_ outputMay
>
> sEchoBox2 :: Signal s IO () ()
> sEchoBox2 = sInput' >>> sOutput'

Now, run `runEcho2` and see what happens. It should look just like
before. However, when other parts are included, this version enables
other computations to be made when there is no input. The following
sections will be built up on the non-blocking version of I/O.


\subsection{Remember the Name}

In this section, we will deal with stateful signals. This time, the
backspace character is correctly handled.

To try the results, run `runGetName`.

> runGetName = initConsole >> runBox clockSession_ sNameBox

First, let's look at what a /name/ is made of: a `String`, or
`[Char]`. To append a string to an existing name, we use `(++)`:

< (++) :: [a] -> [a] -> [a]

Generalizing a bit, it has type `b -> a -> b`, where `b` is the state
of a function. Multiple of these functions can be chained together to
perform a sequence of stateful computation. Also, a /name/ has the
initial value of an empty string, giving the starting point of the
computations. Therefore, we make a function to update the name. Note
that it is a bit different from `(++)` to incorporate with our process
better:

> updateName :: String -> Maybe Char -> String
> updateName s Nothing = s
> updateName s (Just c) = f s c
>     where
>       isDel c = (c == '\b') || (c == '\DEL')
>       f s '\n' = s
>       f "" c'
>           | isDel c' = ""
>           | otherwise = c':""
>       f s@(c:cs) c'
>           | isDel c' = cs
>           | otherwise = c':s


Note that the name is actually stored in reverse so that the code
looks cleaner. Whenever a backspace character ('\b') is detected, the
last character is deleted. Then, we make it a stateful /wire/:

> sName :: (Monad m) => Signal s m (Maybe Char) String
> sName = mkSW_ "" updateName

This signal is called a /wire/ because it never inhibits by
itself. However, it can inhibited by its input signal. In other word,
a /wire/ is passive. `mkSW_` (read it as "make stateful wire") is a
factory function to create a stateful signal from a stateful
computation of type `b -> a -> b`. Notice that the `IO` monad is no
longer specified in its type since its underlyling computation does
not involve `IO`.

Then, to display the name properly, we need to reverse it:

> sReverse :: (Monad m) => Signal s m String String
> sReverse = mkPW_ reverse

`mkPW_` ("make pure wire") creates a pure, stateless, wire, from a
function. Note that `arr` also works for this purpose.

Now, make an output signal that prints string, always on the same line:

> sLineOut :: Signal s IO String ()
> sLineOut = (mkSK_ 0 $ f) >>> mkConstM (return ())
>     where
>       f n s = do
>         putStr $ '\r':(replicate n ' ')
>         putStr $ '\r':s
>         return $ length s

To actually make backspace work, the entire line is overwritten by
white space. Since we need to keep track of the length of previous
string to be covered by space, we need a stateful monadic function of
type `b -> a -> m b`. In this case, using string length as state, we
get type `Int -> String -> IO Int`. Just like `mkSW_`, we use `mkSK_`
to construct such a stateful signal(wire). We also want the output to
be `()`, so we chain to a constant monadic signal. Remember that
`mkConstM` and `mkActM` are just synonyms. The different names just
make it easier to read.

Finally, construct the box. This time we will use the arrow syntax:

> sNameBox :: Signal s IO () ()
> sNameBox = proc _ -> do
>   c <- sInput' -< ()
>   name <- sReverse <<< sName -< c
>   sLineOut -< name
>   returnA -< ()

`proc` keyword is like lambda for arrows. It takes and only takes one
input, in this case, `()`. Next, in the arrow `do` notation, the input
values are to the right of `-<`, while the arrows are on the
left. `<-` extracts the output from an arrow. Notice that only arrows
can go between `<-` and `-<` (which looks like an arrow), and the only
way to feed value into arrows is to use the `-<` operator. Finally, we
return `()` by feeding it into the special `returnA` arrow.

Try to run it by executing `runGetName`.


\subsection{Hello!}

This time, we are going to put your name inside the greeting! Test the
result by running `runHello`

> runHello = initConsole >> runBox clockSession_ sHelloBox

Previously, everything seems to involve a lot of work. But not this
time! The power of FRP now starts to shine!

We need a function to enclose the name in a greeting:

> hello :: String -> String
> hello = ("Hello, "++) . (++"!")
>
> sHello :: (Monad m) => Signal s m String String
> sHello = arr hello

Again, since `hello` is pure, we don't specify the `IO` monad
here. Now, connect the box like this:

> sHelloBox :: Signal s IO () ()
> sHelloBox = proc _ -> do
>   c <- sInput' -< ()
>   name <- sReverse <<< sName -< c
>   helloName <- sHello -< name
>   sLineOut -< helloName
>   returnA -< ()

Try it!


\subsection{Greetings!}

In this section, we are going to make the program more fancy. When the
user types anything, the screen prints "Hello, <name>! Greetings,
<name2>!". However, <name2> is updated only when the user hits
return. Test the results by running `runGrettings`

> runGreetings = initConsole >> runBox clockSession_ sGreetingsBox

First, we need a signal to detect a return:

> sIsReturn :: (Monad m) => Signal s m (Maybe Char) Bool
> sIsReturn = (arr $ f)
>     where
>       f (Just c) = c == '\n'
>       f Nothing = False

We will use the `rising` filter to detect a transition from `False` to
`True`, with the initial value as `False`. This filter will create one
single impulse of `True` when it detects a rising edge.

What the event "hitting enter" does here is to take a sample of the
current name, and store it somewhere. Therefore, we need a `sample`
signal:

> sName2 :: (Monad m) => Signal s m (Bool, String) String
> sName2 = sample

Then, we will need a signal to render the second greeting:

> sGreeting :: (Monad m) => Signal s m String String
> sGreeting = arr f
>     where
>       f "" = "Greetings!"
>       f s = "Greetings, " ++ s ++ "!"

Now, simply make a greetings box:

> sGreetingsBox :: Signal s IO () ()
> sGreetingsBox = proc _ -> do
>   mc <- sInput' -< ()
>   name <- sReverse <<< sName -< mc
>   helloName <- sHello -< name
>   ret <- rising False <<< sIsReturn -< mc
>   greetingName <- sGreeting <<< sName2 -< (ret, name)
>   sLineOut -< helloName ++ " " ++ greetingName
>   returnA -< ()


\subsection{Quitting}

Finally, we need to give the program ability to quit. Since this is
the last part, run `runTutorial`.

> runTutorial = initConsole >> runBox clockSession_ sTutorialBox

This time, we need something that inhibits. Any factory functions that are provided that takes a function that returns `Maybe` creates signals that can inhibit themselves. For example:

< mkPure_ :: (a -> (Maybe b)) -> Signal s m a b

If the function returns `Nothing`, the signal inhibits. Therefore, the
quit signal is simple:

> sQuit :: (Monad m) => Signal s m String ()
> sQuit = mkPure_ f
>     where
>       f s | (toLower <$> s) == "q" || (toLower <$> s) == "quit" = Nothing
>           | otherwise = Just ()

Get this in box:

> sTutorialBox :: Signal s IO () ()
> sTutorialBox = proc _ -> do
>   mc <- sInput' -< ()
>   name <- sReverse <<< sName -< mc
>   helloName <- sHello -< name
>   ret <- rising False <<< sIsReturn -< mc
>   snapName <- sName2 -< (ret, name)
>   greetingName <- sGreeting -< snapName
>   sLineOut -< helloName ++ " " ++ greetingName
>   returnA <<< sQuit -< snapName


DONE!!!!!!! You have followed this super long tutorial and made your
first complete interactive program using `timeless`!!!

Timeless will continue to be developed timelessly. Now I am working on
THE project: `timeless-RPG`, which is more complicated in orders of
magnitude. However, even with such complex libraries, I will use the
very same factories and combinators. If you actually type out the
code, you may notice that you almost never need to debug the program
unless you typed something wrong or have incorrect understanding of
the data flow. THAT is the power of Functional Reactive Programming! I
hope this is a good introduction to `timeless`, and in general, FRP!
