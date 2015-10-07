This file will be literate Haskell in Markdown format, and should compile directly using a recent version of GHC.

# Introduction

## Timeless and Netwire

`Timeless` is a rewrite on the [`netwire-5.0.1`](http://hub.darcs.net/ertes/netwire) library, intending to create a simpler interface for easy FRP(Functional Reactive Programming) construction. The core module, `FRP.Timeless.Signal`, `FRP.Timeless.Session`, and `FRP.Timeless.Run` are mostly taken from Netwire, except that the `Wire s e m a b` is simplified to `Signal s m a b`, and several instances (such as `Profuctor`) are stripped away (I have to admit that I don't understand them, so I don't add them until they are REALLY needed). Everything else will be rewritten from scratch, based on my other project, namely `timeless-RPG`, which tries to create a complete RPG game engine framework (For real, this time! Hopefully it will not be abandoned like... well, the python one, the SFML/C++ one, the Ruby one, the second SFML/C++ one, the python one again... you name it) based on SDL2.

The motivation to rewrite `netwire` as `timeless` is because `netwire` lacks proper documentation, and its `5.*` version is quite incomplete comparing to `4.*`. At the same time, it doesn't seem to be actively developed anymore, so I decide to write most if not all necessary things from scratch to gain a better understanding on, well, everything.

Please do not expect this library to finish before t → ∞. I can only hope that I will not abandon this project like the other ones that I did. I would really like to keep the repo private until I make it rock solid (so that I am less likely to lose motivation due to false satisfaction), but I fear that my hard drive may crash some time in the future (which it just did for my other computer), so I push the repo up now.

## What is Timeless? (do a `s/is/will be/g` in your head, for now)

`Timeless` is an Arrow based Functional Reactive Programming framework which supports continuous-time semantics. Discrete time events are simulated by "impulse functions". It supports dynamic switching and inhibition. Signals include pure, stateful, and Kleisli functions, which should give a wide range of applications.


# Tutorial

Since `timeless` has only the core for now, it is hard to make a tutorial since there are almost no prefabricated signal networks and generators. However, I would still like to give some notes as a hint.

For small examples, the `README` in `netwire` repository should be applicable here. 

For core documentaton, read the comments in the code. I try to make them as clear as possible. Note that a "signal" inhibits, but a "wire" never does, and the `lstrict` things are there to prevent space leak. The things with no comments are basically directly modified from `netwire`. They are nested so deeply that I cannot easily explain them in comments. But if they work, just use them. They are pure code, so they should work anyway.

Inspired by my own answer regarding `netwire` on StackOverflow: [1][1], [2][2], [3][3]. `timeless` (also `netwire`, of course) can be totally treated as a black box whose only input is time (hidden in the `Signal` arrow), and only output is `∅` (which can be inhibited). In this way, a huge program can be packed into a black box of type `Signal s IO () ()`.

This may not be the most elegent way to form a large program, but at least for now, it works very, very well on single-threaded small test programs I have written. As shown in the StackOverflow posts, the main logic of programs are quite well separated, and the arrow syntax makes things really nice. Of course, almost no `IO` is exposed, and most of the program is purely functional.

To convert the examples in the posts into `timeless`, simply import `FRP.Timeless` instead of anything from `Control.Wire`, and change all `Wire s e m a b` to `Signal s m a b` (Of course, the actual types are different), all `mkSF` to `mkPW`, and `kKleisli` to `mkKleisli_` (I may miss something, but I will give real examples).


[1]:http://stackoverflow.com/questions/30905930/what-can-be-a-minimal-example-of-game-written-in-haskell
[2]:http://stackoverflow.com/questions/30992299/console-interactivity-in-netwire
[3]:http://stackoverflow.com/questions/32745934/kleisli-arrow-in-netwire-5
