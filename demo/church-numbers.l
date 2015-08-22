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


# Understand this!  See [1]

divide = λn.((λf.(λx.x x) (λx.f (x x))) (λc.λn.λm.λf.λx.(λd.(λn.n (λx.(λa.λb.b)) (λa.λb.a)) d ((λf.λx.x) f x) (f (c d m f x))) ((λm.λn.n (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) m) n m))) ((λn.λf.λx. f (n f x)) n);


#> References

[1] https://en.wikipedia.org/wiki/Church_encoding

<#



#> TESTING

:set trace none
:set limit 1000000

dec $ divide (enc 100) (enc 13) # 7

<#
