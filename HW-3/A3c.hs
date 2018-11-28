module A3c where 

import Data.Char 
import Data.Maybe 
import Data.List

--Function Composition
----------------------
--Takes a list of strings and returns those which begin with a lower case character 
onlyLowercase :: [String] -> [String]
onlyLowercase str = 
  filter (isLower . head) str

--Takes a list of strings and returns the longest string, returning the first in the event of a tie.
longestString :: [[Char]] -> [Char]
longestString strList = 
  foldl (\arg x -> if length x > length arg then x else arg) "" strList 

--Takes a list of strings and returns the longest string, returning the last in the event of a tie.
longestString' :: [[Char]] -> [Char]
longestString' strList = 
  foldl (\arg x -> if length arg > length x then arg else x) "" strList 

--Takes a function for comparing two strings, a list of strings, and returns the longest string
longestStringHelper :: (Int -> Int -> Bool) -> [[Char]] -> [Char]
longestStringHelper func strList =
  foldl (\arg x -> if func (length arg) (length x) then arg else x) "" strList

--Finds the longest string in a given list, returning the first in the event of a tie
longestString3 :: [[Char]] -> [Char]
longestString3 strList = 
  longestStringHelper (>) (reverse strList)

--Finds the longest string in a given list, returning the last in the event of a tie
longestString4 :: [[Char]] -> [Char]
longestString4 strList = 
  longestStringHelper (>) strList

--Finds the longest string that begins with a lowercase letter given a string list, returning the first in the event of a tie
longestLowercase :: [[Char]] -> [Char]
longestLowercase strList =
  foldl (\arg x -> if (isLower . head) x && (length x > length arg) then x else arg) "" strList

--Reverses and forces the given string to lower case
revStringRev :: [Char] -> [Char]
revStringRev str = 
  let
    revStr = reverse str 
  in
    map toLower revStr

--Applies the given function to elements in the list, returning the first value that does not resolve to Nothing
firstAnswer :: ( a -> Maybe b ) -> [a] -> Maybe b
firstAnswer func [] = Nothing
firstAnswer func items = 
  let
    val = func (head items)
  in 
    if isJust val then val else firstAnswer func (tail items)

--Applies the given function to all elements in the list. If any evaluate to Nothing, returns Nothing.
allAnswers :: (a -> Maybe [b]) -> [a] -> Maybe [b]
allAnswers _ [] = Nothing
allAnswers func items = 
  let
    val = func (head items)
    checkList = map func items

    t = tail items

    convertedVal = concat (maybeToList val)
    convertedList = fromMaybe [] (allAnswers func t)
  in
    if foldr (&&) True (map isJust checkList) then Just (convertedVal ++ convertedList) else Nothing


 --Pattern Matching 
-------------------
data Pattern = WildcardPat | VariablePat (String) | UnitPat | ConstantPat (Int) | ConstructorPat (String, Pattern) | TuplePat ([Pattern]) deriving (Eq, Show)
data Value = Constant (Int) | Unit | Constructor (String, Value) | Tuple [Value] deriving (Eq, Show)

-- G takes two functions (f1,f2) and a pattern (pat). 
-- In the case of a wildcard it calls f1 on Nothing
-- In the case of a variable it calls f2 on the contents of the variable
-- In the case of a constructor it partially applies the function to the contained pattern
-- In the case of a tuple it partially applies the function to the contained pattern summing the results with foldl
g f1 f2 pat =
  let
    r = g f1 f2
  in
    case pat of
      WildcardPat -> f1 ()
      VariablePat x -> f2 x
      ConstructorPat (_, p) -> r p
      TuplePat values -> foldl (\i p -> (r p) + i) 0 values
      _ -> 0

--Counts the number of WildcardPats in a given Pattern
countWildcards :: Pattern -> Int 
countWildcards pat = 
  case pat of 
    WildcardPat            -> g (\x -> 1) (\x -> 0) pat
    ConstructorPat (s1, p) -> g (\x -> 1) (\x -> 0) pat
    TuplePat xs            -> g (\x -> 1) (\x -> 0) pat
    _ -> 0

--Counts the number of WildcardPats and the length of the variable strings in a given Pattern
countWildAndVariableLengths :: Pattern -> Int
countWildAndVariableLengths pat = 
  case pat of 
    WildcardPat              -> g (\x -> 1) (\x -> length x) pat
    VariablePat str          -> g (\x -> 0) (\x -> length x) pat
    ConstructorPat (s1, p)   -> g (\x -> 1) (\x -> length x) pat
    TuplePat xs              -> g (\x -> 1) (\x -> length x) pat
    _ -> 0

--Counts the number of times the given string occurrs in a given pattern (variable)
countAVar :: (String, Pattern) -> Int
countAVar (s, pat) = 
  case pat of 
    VariablePat str          -> g (\x -> 0) (\x -> if x == s then 1 else 0) pat
    ConstructorPat (s1, p)   -> g (\x -> 0) (\x -> if x == s then 1 else 0) pat
    TuplePat xs              -> g (\x -> 0) (\x -> if x == s then 1 else 0) pat
    _ -> 0 

--Returns true if all variables in the pattern are distinct, false otherwise
checkPat :: Pattern -> Bool
checkPat pat =
  let 
    variables = patternToList pat 
  in 
    if length (nub variables) == length variables then True else False

--Converts the variables in a given pattern to a list of strings - used as helper for checkPat
patternToList :: Pattern -> [String]
patternToList pat = 
  case pat of 
    VariablePat x -> [x]
    ConstructorPat (s1, p) -> patternToList p
    TuplePat xs -> concat (map patternToList xs)
    _ -> []

--Takes a (Value, Pattern) pair and returns the relevant bindings provided both pattern AND value match
match :: (Value, Pattern) -> Maybe [(String, Value)]
match (v, p) =
  case p of 
    VariablePat s -> Just [(s, v)]
    UnitPat -> case v of
                Unit -> Just []
                _    -> Nothing
    ConstantPat x -> case v of  
                      Constant y -> if x == y then Just [] else Nothing
                      _ -> Nothing
    ConstructorPat (s1, p) -> case v of 
                                Constructor (s2, v) -> if s1 == s2 then match (v,p) else Nothing
                                _ -> Nothing
    TuplePat ps -> case v of 
                    Tuple vs -> if (length ps) == (length vs) then zip vs (allAnswers match ps) else Nothing
                    _ -> Nothing
    _ -> Nothing

--Takes a value and a list of patterns, then returns relevant bindings if it finds the first match. 
--Nothing otherwise.
firstMatch :: Value -> [Pattern] -> Maybe[(String, Value)]
firstMatch val pats = 
  let 
    h = head pats 
  in 
    firstAnswer match [(val, h)] 

