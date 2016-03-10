# Basic

id x = x;


# Conversion

enc !x = if (eq x 0) (\f x.x) (succ $ enc $ sub x 1);

dec n = n (add 1) 0;

# Calucations.  All copied from [1].

succ n = λf x. f $ n f x;

plus n m = λf x. n f $ m f x;

power n m = n m;

pred n = λf x. n (λg h. h (g f)) (λu.x) id;

times n m = λf x. m (n f) x;

minus m n = n pred m;


# Understand this!  See <https://en.wikipedia.org/wiki/Church_encoding>

#meins> SWITCH: original meins <#

<original#
divide = (\n.((\f.(\x.x x) (\x.f (x x))) (\c.\n.\m.\f.\x.(\d.(\n.n (\x.(\a.\b.b)) (\a.\b.a)) d ((\f.\x.x) f x) (f (c d m f x))) ((\m.\n.n (\n.\f.\x.n (\g.\h.h (g f)) (\u.x) (\u.u)) m) n m))) ((\n.\f.\x. f (n f x)) n));
#end>

<meins#

fix = \f. (λx. f (x x)) (λx. f (x x));
#fix f = f $ fix f;

divide !n !m
  = ( fix
      $
      λc n m f x.
      (λ!d. d (λx a b. b) (λa b. a) x  $  f  $  c d m f x)
      ((λm n. n (λn f x. n (λg h. h (g f)) (λu. x) (λu. u)) m) n m)
    )
    (λf x. f (n f x))
    m
;

#end> <end#

#>
:l "demo/lists.l" "demo/church-numbers.l"
<#

test x y = if (eq y $ dec $ divide (enc $ mul x y) (enc x))
              (eq x $ dec $ divide (enc $ mul x y) (enc y))
              False;

#> small testing

    :set trace none
    :set limit 20000
    :l
    test 5 7

<#

primes = cons 2 $ cons 3 $ cons 5
       $ cons 7
#       $ cons 11
       $ nil;

n = enc $ primes mul 1;
ps = λc n. foldr (λx. c (enc x)) n primes;

bigtest = deep $ map (λp. dec $ divide n p) ps;

#> testing
    :set trace none
    :set limit none
    :l
    bigtest
<#



#> TESTING

:set trace none
:set limit 1000000

dec $ divide (enc 100) (enc 13) # 7

<#
