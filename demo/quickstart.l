#comment>

  * Comments are of the form `#` to EOL, or span multiple lines between
    `#foo>` and `<foo#`.

  * This file contains examples for the simple λ-reducer [1].  It can be
    loaded with the `:l` command from the interactive prompt

        λ> :l demo/quickstart.l

    or from the shell, using

        $ ./lambda demo/quickstart.l

    First, try the commands `:h` and `:h syntax` to get an overview.

<comment#



#------------------------------------------------------------
# Basics


id x = x;

const x y = x;

flip f x y = f y x;


#------------------------------------------------------------
# Building pairs


# construct pair of two arguments

pair x y = λs. s x y;



# get first component of pair

fst p = p $ λx y. x;



# get second component of pair

snd p = p $ λx y. y;



#>
    pair Bonnie Clyde
    fst $ pair Bonnie Clyde
    snd $ pair Bonnie Clyde
<#



#------------------------------------------------------------
# Building Lists


# the empty list

nil = λc n. n;



# list construction from head and tail

cons x xs = λc n. c x $ xs c n;



# extract the head

head xs = xs const undefined;



# the Daltons

daltons = cons Joe $ cons William $ cons Jack $ cons Averell nil;

#>
    daltons
    head daltons
<#



# extract the tail

tail xs
  = fst
    $
    xs
    (\h t1. pair (snd t1) $ cons h (snd t1))
    (pair undefined nil)
  ;

#>
    tail daltons
    head it
<#



# Test for empty list

null xs = xs (λy ys. False) True;

#>
    null daltons
    null nil
<#



# foldr is really easy

foldr f z xs = xs f z;



# length of a list

length = foldr (const $ add 1) 0;

#>
    length daltons
<#



# get last element from list

last xs = foldr (\h t o. t h) id xs undefined;

#>
    last daltons
<#



# map

map f = foldr (λx xs'. cons (f x) xs') nil;



# sum

sum = foldr add 0;

#> 
    :d five = cons 1 $ cons 2 $ cons 3 $ cons 4 $ cons 5 nil
    sum five
    map (mul 10) five
    sum it
<#



# fold left

foldl f z xs = foldr (\a s t. s $ f t a) id xs z;



# reverse a list

reverse = foldl (flip cons) nil;

#>
    reverse daltons
<#


#------------------------------------------------------------
# More fun stuff


# the Y-combinator

yc = λx. (λy. x (y y)) (λy. x (y y));



# `iterate f k` → k : f k : f (f k) : ...

iterate f = yc (λg k. (λx xs c n. c x (xs c n)) k (g (f k)));



# `mkList 2 7` generates list of integers 2 to 7

mkList
  = yc
    \m !a !b.     # yeah, this uses strict arguments...
    if
    (eq a b)
    (cons b nil)
    (cons a $ m (add a 1) b)
  ;

#>
    :set trace none   # do not print steps
    :set limit 5000   # do not stop after 1000 steps
    sum $ mkList 1 100
<#



# take and drop elements from the head of a list

take = yc \t !n !xs. if
                     (xs (\a b. (eq n 0)) True)
                     nil
                     (cons (head xs) $ t (sub n 1) (tail xs))
                     ;

drop = yc \d !n !xs. if
                     (xs (\a b. (eq n 0)) True)
                     xs
                     (d (sub n 1) (tail xs))
                     ;

#>
    sum $ take 5 $ drop 10 $ mkList 1 100
<#


#============================================================
