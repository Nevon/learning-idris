module Matrix

import Data.Vect

total createEmpties : Vect columns (Vect 0 elem)
createEmpties {columns = Z} = []
createEmpties {columns = (S k)} = [] :: createEmpties

total transposeMat : Num elem =>
               Vect rows (Vect columns elem) -> Vect columns (Vect rows elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let transposed = transposeMat xs in
                             zipWith (\x, y => x :: y) x transposed

total addMat : Num a =>
         Vect rows (Vect cols a) -> Vect rows (Vect cols a) -> Vect rows (Vect cols a)
addMat [] [] = []
addMat (xrow :: xcolumns) (yrow :: ycolumns) = zipWith (+) xrow yrow :: addMat xcolumns ycolumns