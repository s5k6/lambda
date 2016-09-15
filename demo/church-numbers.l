#>
NOTE: also load `demo/lists.l`.  So the invocation should be

    $ ./lambda demo/lists.l demo/church-numbers.l

<#


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


# Understand this!
# See <https://en.wikipedia.org/wiki/Church_encoding>

#fix = \f. (λx. f (x x)) (λx. f (x x));
fix f = f $ fix f;

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



# take two numbers, divide their product to the latter and check result

test x y = eq y $ dec $ divide (enc $ mul x y) (enc x);

#> 
    :set trace none
    :set limit 1000000
    
    test 5 7
    test 12 5
    
    ands $ map (test 3) (fromTo 2 10)

    (\l. ands $ map (\x. ands $ map (test x) l) l) (fromTo 2 10)
<#

and a b = if a b False;
ands = foldr and True;

