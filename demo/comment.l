
id x = x;   # This is a comment.  If ends at the end of this line.

#> This is a multi-line comment.
It has to be terminated with <#      flip f x y = f y x;

#foo> If a key (here: `foo`) appears between `#` and `>`, then the
comment does not end with <#, or the end of the line.  Instead, the
same key must be used again: <foo#

#---------------------------------------------------------------------->
For obvious reasons, the key must not contain the characters `<`, `>`,
`#`, or any whitespace.  This implementation uses the rather generous
character set

    ['$'..';'] ++ "=" ++ ['?'..'}']

for keys, here given in Haskell list notation.  It contains all ASCII
letters, and digits, and most punctuation.

This technique allows to comment out arbitrary regions of source code.
It also provides the "switching comment" hack:
<----------------------------------------------------------------------#

fix = # here are three ways to define `fix`:


    #rec>            The first of these three lines which does not feature
    # Y>              a whitespace between `#` and `>`, determines the branch
    # turing>         of source code to be used in the following:

    
    Recursion using variable lookup - <rec#

        \f. f $ fix f

    #end>

    
    The Y combinator - <Y#

        \f. (\x. f (x x)) (\x. f (x x))

    #end>

    
    Turing's Θ - <turing#

        ( α α ; α = λx y. y (x x y) )

    #end> end of switching comment <end#

;
