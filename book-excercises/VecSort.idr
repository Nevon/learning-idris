module VecSort

import Data.Vect

total insert : Ord elem =>
         (head : elem) -> (sortedTail : Vect len elem) -> Vect (S len) elem
insert head [] = [head]
insert head (thing :: sortedTail) = case head < thing of
                                         False => thing :: insert head sortedTail
                                         True => head :: thing :: sortedTail

total insSort : Ord elem =>
          Vect len elem -> Vect len elem
insSort [] = []
insSort (thing :: things) = let sortedThings = insSort things in
                        insert thing sortedThings

myLength : List a -> Nat
myLength [] = 0
myLength (x :: xs) = 1 + myLength xs

total insertBackwards : (head : a) -> (tail : Vect len a) -> (Vect (S len) a)
insertBackwards head [] = [head]
insertBackwards head (thing :: tail) = thing :: insertBackwards head tail

total myReverse : Vect len a -> Vect len a
myReverse [] = []
myReverse (x :: xs) = let things = myReverse xs in
                          insertBackwards x things

total myMap : (a -> b) -> Vect len a -> Vect len b
myMap f [] = []
myMap f (x :: xs) = f x :: myMap f xs