# Warning
Don't use timeless before I fix the memory leak! Also, I found that Arrows seem to work a bit differently than I thought, and there are some mysterious delays causing my state to be non transparent. Before fixing those, Timeless is not usable!


# Introduction

## Timeless and Netwire

After version 1, the only common code between `timeless` and `netwire` is the typeclass part. I greatly simplified and purefied the entire Signal class, and hopefully make it much easier to reason about, while keeping all nice properties and syntax about Arrow.


## What is Timeless? (do a `s/is/will be/g` in your head, for now)

`Timeless` is an Arrow based Functional Reactive Programming framework which supports continuous-time and discrete time semantics, and allows the use of Arrow syntax.

# Tutorial

Look at the package [timeless-tutorials](https://hackage.haskell.org/package/timeless-tutorials). It is just started by me, and should expand later.

[1]:http://stackoverflow.com/questions/30905930/what-can-be-a-minimal-example-of-game-written-in-haskell
[2]:http://stackoverflow.com/questions/30992299/console-interactivity-in-netwire
[3]:http://stackoverflow.com/questions/32745934/kleisli-arrow-in-netwire-5
