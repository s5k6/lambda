Lambda
======

A primitive, normal order, WHNF reducer for expressions of a slightly
extended untyped λ-calculus.

It comes with a nice REPL, builtin help, and lots of example code.

I've mainly used this as playground for myself, and also for some
demonstrations while [teaching functional
programming](http://stefan-klinger.de/#lect_kdp).

On the inside, it might be a bit messy since I've used code that
emerged on different occasions.

WARNING: This is experimental, and may just break or do something wrong!


Getting started
===============

Note: On Arch Linux, the argument `-dynamic` must be passed to GHC.
This can be done by passing `--ghc-option=-dynamic` to each of the
`cabal` commands below, or by setting

    program-default-options
      ghc-options: -dynamic

in `~/.cabal/config`.


Compile
-------

    $ cabal build

To run it, type

    $ cabal exec lambda
    Primitive λ-evaluator — Type `:h` for help.

    λ>


one can also pass a file to load on the command line, e.g., try
[`quickstart.l`](demo/quickstart.l) for starters

    $ cabal exec lambda demo/quickstart.l
    Primitive λ-evaluator — Type `:h` for help.

    λ> head daltons
    …
    β→
       Joe


Install
-------

Note: If you install binaries into a directory that happens to contain
a symlink, then `cabal` will fail to create a proper link to the
binary.  One can avoid this by using absolute paths in `~/.cabal/config`.

To install the binary, use

    $ git describe --dirty=+ > REVISION
    $ cabal install   # --overwrite-policy=always

Enjoy!


Misc
====

Check source code:

    $ cabal install hlint
    $ hlint src/
