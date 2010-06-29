{-# LANGUAGE ExistentialQuantification #-}

-- Data type from Max Rabkin's "Beautiful Folding" (http://squing.blogspot.com/2008/11/beautiful-folding.html):
-- Fold over list of type |[b]| with result of type |c|
data Fold b c = forall a. F (a -> b -> a) a (a -> c)


-- Data type after existential elimination, Oleg-style:
data Fold' b c = F' (b -> Fold' b c) c

back :: Fold' b c -> Fold b c
back f' = F (\(F' x _) b -> x b) f' (\(F' _ y) -> y)

forth :: Fold b c -> Fold' b c
forth (F x a y) = F' (\b -> forth (F x (x a b) y)) (y a)


main :: IO ()
main = return ()
