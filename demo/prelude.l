
# basic combinators --------------------------------------------------

id x = x;

const c x = c;

flip f x y = f y x;

comp f g x = f $ g x;
app f x = f x;

and x y = if x t False;
or x y = if x True y;
not x = if x true False;
