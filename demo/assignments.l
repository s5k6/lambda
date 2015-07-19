# Blatt 3 Aufgabe 2

a1 = x y x;
a2 = x (λx. x y);
a3 = (λx.y) x;
a4 = (λx.(λy. mul x z) y) y;
a5 = (λx.x x)(λy. add y z);
a6 = (λx z. y (λx. x y) (λy. x y));

# Blatt 3 Aufgabe 3

b1 = (λx. x 3) (div 9);
b2 = (λa b. b a) 7 (add 1);
b3 = (λa b c.a (b c)) (add 1) (λx. x) 7;
b4 = (λf x y. f y x) div 7 49;

# Blatt 4 Aufgabe 1

c1 = (λx. λy. mul 5 x) y;
c2 = (λx. if (eq x 0) 42 x) (sub 5 4);
c3 = (λf x.f (f x))(λx. mul x 2);
c4 = (λf x. f x) (λf x. f x);
c5 = (λf x y. f y x) (λf g. (λx. f (g x)));

# Blatt 4 Aufgabe 2

true = \x y. x;
false = \x y. y;
and' = \a b. a b false;

say x = if' x Yes No;

#> Testen:

    say $ and' true true
    say $ and' true false
    say $ and' false true
    say $ and' false false

<#

if' = \x.x;

#> Testen:

    if' true 42 23

<#


# Blatt 12 Aufgabe 1

d1 = (λz x. (λx. x) z) x;
d2 = (λx y. x 3) (add 4) (add 6 7);
d3 = (λg x. (λf. x f) g) (λf. g f) ((λg h. f h g) (h f));
d4 = (λc n. c (sub 30 7) (c (mul 6 7) n)) (λy. add 1) 0;
