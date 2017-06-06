module Palindrome

palindrome : String -> Bool
palindrome input = (toLower input) == (toLower (reverse input))

longerThan : Nat -> String -> Bool
longerThan maxLength input = length input > maxLength

longPalindrome : String -> Bool
longPalindrome input = (palindrome input) && longerThan 10 input

counts : String -> (Nat, Nat)
counts input = (length input, length (words input))

topTen : (Ord a) => List a -> List a
topTen input = take 10 (reverse (sort input))

overLength : Nat -> List String -> Nat
overLength limit xs = length (filter (\x => (length x) > limit) xs)