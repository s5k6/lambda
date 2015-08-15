
#>----------------------------------------------------------------------

Have a look at `demo.l`.  Using the functions declared there, solve
the following tasks:

  1. Write a function that tests whether the passed integer argument
     is even.

            even 42
         *→   4 steps (1 β-reductions + 2 δ-reductions + 1 lookups)
            True

            even 23
         *→   4 steps (1 β-reductions + 2 δ-reductions + 1 lookups)
            False

            even "Joe"
         *→   2 steps (1 β-reductions + 1 lookups)
         Cannot δ-reduce this

  2. Write a function `filter pred list` that removes all items from a
     `list` that do not qualify for the (boolean) `pred`icate.

            length $ filter even $ mkList 1 10
         *→   254 steps (152 β-reductions + 64 δ-reductions + 38 lookups)
            5

  3. Define a `value` to be the sum of the numbers 1–100 (inclusive).

         :set limit none  # it's a length calculation
         :set trace none  # optional, for faster output
         
            value
         *→   2220 steps (1312 β-reductions + 649 δ-reductions + 259 lookups)
            2550

  4. Define `shallow xs` so that *all* list constructors of the list
     `xs` are replaced by `C` for cons, and `N` for nil, but the list
     items are not evaluated.

            shallow $ map (add 1) $ mkList 0 5
         *→   145 steps (108 β-reductions + 17 δ-reductions + 20 lookups)
            C (add 1 0) (C (add 1 1) (C (add 1 2) (C (add 1 3) (C (add 1 4)
              (C (add 1 5) N)))))


Hints: To load multiple files at once, use one of

    $ lambda demo.l exercise.l

or

    λ> :l demo.l exercise.l 

Loading in two steps is not possible, because the environment (the
list of known definitions) will be cleared for the second load.

The last exercise requires strict evaluation in the right place.

----------------------------------------------------------------------<#


# Solution:

even n = eq 0 $ mod n 2;

filter p = foldr (\x ys. if (p x) (cons x ys) ys) nil;

value = sum $ filter even $ mkList 1 100;

shallow = foldr (\x !xs. C x xs) N;
deep = foldr (\!x !xs. C x xs) N;
