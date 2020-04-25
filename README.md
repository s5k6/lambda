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
---------------

Compile:

    $ cabal build

Then run it, and have a look at [`quickstart.l`](demo/quickstart.l)
for some code examples.

    $ cabal run lambda demo/quickstart.l
    Primitive λ-evaluator — Type `:h` for help.
    …
    λ> head daltons
    …
    β→
       Joe


Install binary: FIXME: not working currently.  Why?  Due to using
TemplateHaskell, need to sort this out...

    $ cabal install
    $ lambda

Check source code:

    $ cabal install hlint
    $ hlint src/


Enjoy!
