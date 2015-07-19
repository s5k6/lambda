id x = x;
const x y = x;
yc = \x. (\y. x $ y y) (\y. x $ y y);


pair x y = \s. s x y;
fst p = p (\x y. x);
snd p = p (\x y. y);


couple = pair Bonnie Clyde;

#> Testen:

    fst couple
    snd couple

<#



nil c n = n;
cons x xs c n = c x (xs c n);

daltons = cons "Joe" (cons "William" (cons "Jack" (cons "Averell" nil)));

head xs = xs const Undef;
last xs = xs (\h t o. t h) id Undef;
tail xs = fst (xs (\h t1. (\!t2. pair t2 (cons h t2)) (snd t1)) (pair Undef nil));

#> Testen:

    daltons C N
    head daltons
    tail daltons
    head $ tail daltons

<#



numbers = cons 1 $ cons 2 $ cons 3 $ cons 4 $ cons 5 nil;

#> Zum Rumspielen:

    head numbers
    numbers add 0

<#



from = yc  $  \yf !m. cons m $ yf (add m 1);

fromTo = yc  $  \yf !m k. cons m $ if (eq m k) nil (yf (add m 1) k);



#> Zum Rumspielen:

    fromTo 1 5 Cons Nil

    fromTo 1 5 add 0

<#
