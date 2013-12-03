module Pattern where
import Utilities


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute wildcard [] pattern = []
substitute wildcard (m:ms) pattern
    | wildcard /= m = m : substitute wildcard ms pattern
    | otherwise     = pattern ++ substitute wildcard ms pattern


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match     _      []     [] = Just []
match     _      _      [] = Nothing
match     _      []     _  = Nothing
match wildcard pattern@(p:ps) str@(s:ss)
    | p /= wildcard && p == s = match wildcard ps ss
    | p /= wildcard           = Nothing
    | otherwise               = longerWildcardMatch pattern str `orElse` singleWildcardMatch pattern str
    where
        -- Helper functions to match
        singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
        singleWildcardMatch (wc:ps) (x:xs) = mmap (const [x]) $ match wc ps xs
        longerWildcardMatch pattern@(wc:ps) (x:xs) = mmap (x:) $ match wc pattern xs


-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc f transList (pattern, replacement) = mmap (substitute wc replacement) $ mmap f matched
    where
        matched = match wc pattern transList


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply wc f dict input = foldr1 orElse $ map (transformationApply wc f input) dict



--------------------------------------------------------
-- Test cases
--------------------------------------------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

substituteCheck2 = substitute 'x' "3*cos(x) + 4 - x" "5.37" == "3*cos(5.37) + 4 - 5.37"

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions

matchTest1 = match 'x' "2*x+3" "2*7+3" == Just "7"
matchTest2 = match '*' "frodo" "gandalf" == Nothing
matchTest3 = match 2 [1,3..5] [1,3..5] == Just []
matchTest4 = match '*' "* and *" "you and me" == Just "you"
matchTest5 = match 'x' "2*x+3+x" "2*7+3" == Nothing

matchTests = matchTest1 && matchTest2 && matchTest3 && matchTest4 && matchTest5

frenchPresentation = ("My name is *", "Je m'appelle *")
testTransApply = transformationApply '*' id "My name is Zacharias" frenchPresentation == Just "Je m'appelle Zacharias"

frenchPresentations = [(words "Hail * hypnotoad", words "Le * quacque - dinner"), (words "My name is *", words "Je m'appelle *")]
testTranssApply = transformationsApply "*" id frenchPresentations (words "My name is Zacharias") == Just (words "Je m'appelle Zacharias")
testTranssApply1 = transformationsApply "*" id frenchPresentations (words "Duck.") == Nothing
testTranssApply2 = transformationsApply "*" id frenchPresentations (words "Hail the hypnotoad") == Just (words "Le the quacque - dinner")


