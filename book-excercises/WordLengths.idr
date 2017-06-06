module WordLengths

import Data.Vect

total allLengths : (words : Vect n String) -> Vect n Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words