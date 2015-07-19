# basics -------------------------------------------------------------

id x = x;

const x y = x;

flip f x y = f y x;

# booleans -----------------------------------------------------------

true = const;
false = flip const;

if' = id;

and a b = if' a b false;
or a b = if' a true b;
not a = if' a false true;

# Translate back to builtin booleans
say a = if' a True False;


# tuples -------------------------------------------------------------

fst p = p $ λx y. x;

snd p = p $ λx y. y;

pair x y = λs. s x y;


#> Testen:

    pair Bonnie Clyde
    fst $ pair Bonnie Clyde
    snd $ pair Bonnie Clyde

<#


# lists --------------------------------------------------------------

nil = λc n. n;
cons x xs = λc n. c x $ xs c n;

head xs = xs const undefined;

#> local definitions not yet implemented
tail xs
  = ( fst $ xs c n
    ; n = pair undefined nil
    ; c h t1 = ( pair t2 $ cons h t2 ; t2 = snd t1 )
    )
  ;
<#

tail xs
  = fst $ xs (\h t1. pair (snd t1) $ cons h (snd t1)) (pair undefined nil)
  ;

tailStricter xs
  = fst $ xs (\h t1. (\!t2. pair t2 $ cons h t2) (snd t1)) (pair undefined nil)
  ;






isNil xs = xs (λy ys. false) true;

foldr f z xs = xs f z;

map f = foldr (λx xs'. cons (f x) xs') nil;


# Examples with lists:

daltons = cons Joe $ cons William $ cons Jack $ cons Averell nil;

last xs = foldr (\h t o. t h) id xs undefined;



# `iterate f k` → k : f k : f (f k) : ...

iterate f
  = yc (λg k. (λx xs c n. c x (xs c n)) k (g (f k)));

yc = λx. (λy. x (y y)) (λy. x (y y));


mkList
  = yc
    \m !a !b.
    if
    (eq a b)
    (cons b nil)
    (cons a $ m (add a 1) b)
  ;

# mkList 0 4   C N



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

#> Testen:

    map (\k. head $ drop k daltons) (mkList 0 3)   C N

    map (\k. drop k daltons C N) (mkList 0 3) C N
C
(C Joe (C William (C Jack (C Averell N))))
((λy. (λm !a !b. If (Eq a b) (cons b nil) (cons a (m (Add a 1) b))) (y y)) (λy. (λm !a !b. If (Eq a b) (cons b nil) (cons a (m (Add a 1) b))) (y y)) (Add 0 1) 3 (λx xs'. cons ((λk. drop k daltons C N) x) xs') nil C N)


<#
