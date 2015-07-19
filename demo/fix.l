# Hello emacs: -*- mode: fundamental -*-


# An assortment of various fixed-point combinators:


# The Y-Combinator

ycom = λx. (λy. x (y y))  (λy. x (y y));


# Alan Turing's Θ

theta = alan alan;
alan = λx y. y (x x y);



# Klop's K, a little obfuscated...

klop = (λl. l l l l l l l l l l l l l l l l l l l l l l l l l l)
       (λa b c d e f g h i j k l m n o p q s t u v w x y z r. r(

#      ...but trust me,
    
       t h i s   i s   a   f i x e d   p o i n t   c o m b i n a t o r

));



# Using this, we can implement the factorial function:

fix = ycom;


fact = fix $ \g n. if (eq n 0) 1  $  mul n  $  g  $  sub n 1;

# implementation with strict argument
fact' = fix $ \g !n. if (eq n 0) 1  $  mul n  $  g  $  sub n 1;

# implementation with strict accumulating paramaters
fact'' = (fix $ \f !x !n. if (eq n 0) x $ f (mul n x) (sub n 1)) 1;

# An implementation using name-recursion would be
fact3 !n = if (eq n 0) 1 $ mul n $ fact3 (sub n 1);



#> -------------------------------------------------------------------

Compare reduction steps, `fact'` is the fastest, but `fact''` produces
smaller intermediate results:

    λ> fact 20    
     →
       2432902008176640000
    990 steps (64 β-reductions + 462 δ-reductions + 464 lookups)

    λ> fact' 20
     →
       2432902008176640000
    230 steps (64 β-reductions + 82 δ-reductions + 84 lookups)

    λ> fact'' 20
     →
       2432902008176640000
    251 steps (85 β-reductions + 82 δ-reductions + 84 lookups)

------------------------------------------------------------------- <#
