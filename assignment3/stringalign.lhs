Assignment 3 - String Alignment
Erik Jansson, ada09eja, erikjansson90@gmail.com

> import Prelude hiding (concat) -- we want the concat name for ourselves!

Introduction
============

This report covers an implementation of the string alignment problem for the
functional programming course on LTH.

The string alignment problem is fairly simple to understand: Given two strings,
place one above the other and insert spaces as needed (although not directly
above another space) in order to maximize the score. The score is given by three
parameters:

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

Main Functions
--------------

Now we have defined what we need for the functions that will actually find the
optimum alignments as well as the best score. We will begin by defining them
with a very naïve implementations which will not scale to larger Strings but
will illustrate the ideas nicely.

Our first task is to implement a function to find out the score of the optimal
alignment of two Strings. We call this function similarityScore.

> similarityScore' :: String -> String -> Int
> similarityScore' [] [] = 0
> similarityScore' [] ys = scoreSpace * length ys
> similarityScore' xs [] = scoreSpace * length xs
> similarityScore' (x:xs) (y:ys) = max3 (sim   xs  (y:ys) + score  x  '-')
>                                       (sim   xs    ys   + score  x   y)
>                                       (sim (x:xs)  ys   + score '-'  y)
>     where
>         sim = similarityScore' -- only defined for brevity above

It simply takes both Strings as parameters and recursively finds the one with
the highest score.

The next task is to implement the function performing the actual optimal
alignment. We call this function optAlignments.

> optAlignments' :: String -> String -> [AlignmentType]
> -- optAlignments s1 s2 = maximaBy scoreAlign . genAlignments s1 $ s2
> optAlignments' = ((.) (maximaBy scoreAlign)) . genAlignments
> -- point free to practice it, not sure if it's more readable though
>     where
>         scoreAlign ([],[])         = 0
>         scoreAlign ((x:xs),(y:ys)) = score x y + scoreAlign (xs,ys)
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

The problem with these implementations is that they will evaluate recursive
calls many times. E.g. in similarityScore', there are three recursive calls
which means that we get an execution tree similar to this:

                                [a,b] [d,e]
                              /      |      \
                             /       |       \
                     [b] [d,e]     [b] [e]     [a,b] [e]
                     /    |    \ /    |    \ /   |    \
                    /     |     X   [] []   X    |     \
                   /      |    / \         / \   |      \
              [] [d,e]  [] [e]     [b] [e]     [b] []  [a,b] []

The lists of letters in the (extremely beautiful) figure illustrates parameters
to the similarityScore' function. Already after a recursion depth of three one
can see that several calls are performed more than once. This will only get
worse as the length of the Strings increase and will quickly become
unsustainable.

Luckily, there is a technique to deal with this called memoization: We can save
the already calculated results in a table which we later can perform lookups in.
This way, each of the nodes in the tree above will only be calculated once and
the next time the value is needed a lookup in the table is performed.

The two implementations using memoization follows:

> similarityScore :: String -> String -> Int
> similarityScore xs ys = sim (length xs) (length ys)
>     where
>         sim i j = simTable !! i !! j
>         simTable = [[ simEntry i j | j<-[0..]] | i<-[0..]]
> 
>         simEntry :: Int -> Int -> Int
>         simEntry 0 0 = 0
>         simEntry i 0 = i * scoreSpace
>         simEntry 0 j = j * scoreSpace
>         simEntry i j = max3 (sim (i-1) (j-1) + score  x   y)
>                             (sim (i-1)   j   + score  x  '-')
>                             (sim   i   (j-1) + score '-'  y)
>             where
>                 x = xs !! (i-1)
>                 y = ys !! (j-1)

similarityScore resembles the naïve implementation but instead of the call to
max3 being done when recursing, we now save its return value as an entry in the
table.

> optAlignments :: String -> String -> [AlignmentType]
> optAlignments xs ys = snd $ optAlign (length xs) (length ys)
>     where
>         optAlign i j = optTable !! i !! j
>         optTable = [[ optEntry i j | j<-[0..]] | i<-[0..]]
> 
>         optEntry :: Int -> Int -> (Int, [AlignmentType])
>         optEntry 0 0 = (0,[([],[])])
>         optEntry i 0 = (s + scoreSpace, attachTails x '-' a)
>             where (s,a) = optAlign (i-1) 0
>                   x = (xs !! (i-1))
>         optEntry 0 j = (s + scoreSpace, attachTails '-' y a)
>             where (s,a) = optAlign 0 (j-1)
>                   y = (ys !! (j-1))
>         optEntry i j
>             | x == y = entry1
>             | otherwise = concat res
>             where
>                 x = (xs !! (i-1))
>                 y = (ys !! (j-1))
>                 (s1,e1) = optAlign (i-1) (j-1)
>                 (s2,e2) = optAlign (i-1)   j
>                 (s3,e3) = optAlign   i   (j-1)
>                 entry1 = (s1 + score  x  y , attachTails  x  y  e1)
>                 entry2 = (s2 + score  x '-', attachTails  x '-' e2)
>                 entry3 = (s3 + score '-' y , attachTails '-' y  e3)
>                 res = maximaBy fst [entry1, entry2, entry3]

optAlignments is slightly more involved but the gist is the same. One difference
is that we use recursion when either parameter is 0 (instead of using replicate
as we did before when either list was empty). We also avoid generating all
possible alignments (which is kind of the point of doing this optimization) and
each recursion step only returns the ones with the best score.

We also define an output function that will print the result to the screen in a
nice manner.

> outputOptAlignments :: String -> String -> IO ()
> outputOptAlignments string1 string2 = do
>     putStrLn $ "There are " ++ show (length alignments) ++
>         " optimal alignments:"
>     mapM_ output alignments
>     where
>         alignments = optAlignments string1 string2
>         output alignment = do
>             putStrLn ""
>             mapM_ (\c -> putStr $ " " ++ [c]) $ fst alignment
>             putStrLn ""
>             mapM_ (\c -> putStr $ " " ++ [c]) $ snd alignment
>             putStrLn ""

Utility Functions
-----------------

This section defines some utility functions used by the main functions to
facilitate their tasks.

As can be seen in the definition above, similarityScore uses the utility
function max3 which we define as:

> max3 :: Int -> Int -> Int -> Int
> max3 a b = max a . max b
> -- max3 a = ((.) (max a)) . max
> -- can we get rid of the a in max a?

I.e., it takes the maximum of three arguments instead of two.

> attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
> attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

The attachHeads functions takes two items and a list of tuples with two list
members. It will then construct a new list of tuples with the two items added as
the head element of the lists in each tuple. E.g.:

    attachHeads 'H' 'P' [("askell", "ascal"), ("TML", "ython")]
        = [("Haskell","Pascal"),("HTML","Python")]

We also define the opposite of attachHeads (i.e. attachTails) which will we use
in our optimized version.

> attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
> attachTails h1 h2 li = [((xs ++ [h1]), (ys ++ [h2])) | (xs,ys) <- li]

Another handy utility function that we extensive use of is maximaBy. It works
exactly like the library function maximumBy except that it returns a list of all
elements that are equally large (by the comparison function).

> maximaBy :: Ord b => (a -> b) -> [a] -> [a]
> maximaBy cmp xs = foldr maxima [] xs
>     where
>         maxima x    []       = [x]
>         maxima x (a:acc)
>             | cmp x >  cmp a = [x]
>             | cmp x == cmp a = x:(a:acc)
>             | otherwise      = (a:acc)

We also use a custom concat function which will take a list of tuples of Int
(the score of the alignments) and [AlignmentType] and concatenates them into one
tuple. This requires that the list items have the same score but the function
does not ensure it, this has to be done by the caller.

> concat :: [(Int, [AlignmentType])] -> (Int, [AlignmentType])
> concat (l:li) = (fst l, concatMap snd (l:li))

Lastly, the score function is used by all of the main functions and simply
calculates the score of two chars.

> score :: Char -> Char -> Int
> score  x '-' = scoreSpace
> score '-' y  = scoreSpace
> score  x  y  = if x == y then scoreMatch else scoreMismatch

Test Cases
----------

Lastly here are some test cases to test in GHCi. A main is also defined to allow
compiling of this file into an executable.

> testSimScore = similarityScore "writers" "vintner" == -5

> testOptAlignments' = optAlignments' "writers" "vintner" ==
>                                                 [("writ-ers","vintner-"),
>                                                  ("wri-t-ers","v-intner-"),
>                                                  ("wri-t-ers","-vintner-")]

> testOptAlignments = optAlignments "writers" "vintner" ==
>                                                 [("writ-ers","vintner-"),
>                                                  ("wri-t-ers","-vintner-"),
>                                                  ("wri-t-ers","v-intner-")]

> main = outputOptAlignments "aferociousmonadatemyhamster" "functionalprogrammingrules"

> scoreMatch = 0
> scoreMismatch = -1
> scoreSpace = -1

