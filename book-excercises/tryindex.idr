import Data.Vect

tryIndex : Integer -> Vect len a -> Maybe a
tryIndex {len} i xs = case integerToFin i len of
                           Nothing => Nothing
                           (Just idx) => Just (Vect.index idx xs)

