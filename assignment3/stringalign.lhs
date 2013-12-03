Assignment 3 - String Alignment
Erik Jansson, ada09eja, erikjansson90@gmail.com

Introduction
============

This report covers an implementation of the string alignment problem for the
functional programming course on LTH.

The string alignment problem is fairly simple to understand: Given two strings,
place one above the other and insert spaces as needed in order to maximize the
score. The score is given by three parameters:

* scoreMatch    - gives the score when there's a match between the lists
* scoreMismatch - opposite of the scoreMatch
* scoreSpace    - gives the score of an inserted space

Example:

    If we have the score parameters:
        scoreMatch    = 1
        scoreMismatch = -1
        scoreSpace    = -2

    The score of:
        MU-FFIN
        MUSTARD

    Would be:
        1+1-2-1-1-1-1 = -4

In order to write a function to determine an optimal string alignment, we define
the return type of that function as (although the function will return a list of
these):

> type AlignmentType = (String,String)

I.e. our function will return a list of String pairs that are aligned optimally.

A Minor Detour
--------------

Another problem, known as the MCS problem, deals with finding the maximum common
subsequence of two sequences. A sequence is a subsequence of another sequence if
the former can be obtained by removing zero or more element from the latter.
E.g. "hll" is a subsequence of "hello" since it can be obtained be removing the
e and o from "hello". Thus, e.g. the MCS of "hello" and "gello" is obviously
"ello".

Cycling back to our original problem, one can note that the MCS problem is a
special case of the string alignment problem. Setting the score parameters to
scoreMatch=1 and scoreMismatch=scoreSpace=0 will result in the string alignment
algorithm returning the sequences (of characters) which have the maximum number
of matches between the two strings. If we then simply remove the inserted spaces
and the mismatching characters from any of the lists, we arrive at the MCS of
the two lists.

Implementation
==============

Now we've had enough text. More code!

Utility Functions
-----------------

This section defines some utility functions used by the main functions to
facilitate their tasks.

As can be seen in the definition below, similarityScore uses the utility
function max3 which we define as:

> max3 :: Int -> Int -> Int -> Int
> max3 a b = max a . max b

I.e., it takes the maximum of three arguments instead of two.

> attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
> attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

The attachHeads functions takes two items and a list of tuples with two list
members. It will then construct a new list of tuples with the two items added as
the head element of the lists in each tuple. E.g.:

    attachHeads 'H' 'P' [("askell", "ascal"), ("TML", "ython")]
        = [("Haskell","Pascal"),("HTML","Python")]

Another handy utility function that we will use later is maximaBy. It works
exactly like the library function maximumBy except that it returns a list of all
elements that are equally large (by the comparison function).

> maximaBy :: Ord b => (a -> b) -> [a] -> [a]
> maximaBy cmp xs = foldr maxima [] xs
>     where
>         maxima x    []   = [x]
>         maxima x (a:acc)
>             | cmp x >  cmp a = [x]
>             | cmp x == cmp a = x:(a:acc)
>             | otherwise      = (a:acc)

Main Functions
--------------

Now we have defined what we need for the functions that will actually find the
optimum alignments as well as the best score. We will begin by defining them
with a very naïve implementations which will not scale to larger Strings but
will illustrate the ideas nicely.

Our first task is to implement a function to find out the score of the optimal
alignment of two Strings. We call this function similarityScore.

> similarityScore :: String -> String -> Int
> similarityScore [] [] = 0
> similarityScore [] ys = scoreSpace * length ys
> similarityScore xs [] = scoreSpace * length xs
> similarityScore (x:xs) (y:ys) = max3 (sim   xs    ys   + score  x   y)
>                                      (sim   xs  (y:ys) + score  x  '-')
>                                      (sim (x:xs)  ys   + score '-'  y)
>     where
>         sim = similarityScore -- only defined for brevity above
>         score  x '-' = scoreSpace
>         score '-' y  = scoreSpace
>         score  x  y  = if x == y then scoreMatch else scoreMismatch

It simply takes both Strings as parameters and recursively finds the one with
the highest score.

The next task is to implement the function performing the actual optimal
alignment. We call this function optAlignments.

> optAlignments :: String -> String -> [AlignmentType]
> -- optAlignments s1 s2 = maximaBy scoreAlign . genAlignments s1 $ s2
> optAlignments = ((.) (maximaBy scoreAlign)) . genAlignments
> -- point free to practice it, not sure if it's more readable though
>     where
>         scoreAlign ([],[])         = 0
>         scoreAlign ((x:xs),(y:ys)) = score x y + scoreAlign (xs,ys)
>             where
>                 score  x '-' = scoreSpace
>                 score '-' y  = scoreSpace
>                 score  x  y  = if x == y then scoreMatch else scoreMismatch
>         genAlignments [] s2 = [(replicate (length s2) '-', s2)]
>         genAlignments s1 [] = [(s1, replicate (length s1) '-')]
>         genAlignments (x:xs) (y:ys) =
>             (attachHeads  x  y  $ genAlignments   xs   ys) ++
>             (attachHeads  x '-' $ genAlignments   xs (y:ys)) ++
>             (attachHeads '-' y  $ genAlignments (x:xs) ys)

This function bears some similarities to similarityScore but is a bit different.
It actually generates all possible alignments and then filters them by use of
the maximaBy function. Note that we can't use similarityScore as the first
argument to maximaBy since it would find the score of the optimum alignment of
its two parameters. We instead want a function that only calculates the score
for the given alignment.

> testOptAlignmets = optAlignments "writers" "vintner" == [("writ-ers","vintner-"), ("wri-t-ers","v-intner-"), ("wri-t-ers","-vintner-")] 

> scoreMatch = 0
> scoreMismatch = -1
> scoreSpace = -1

