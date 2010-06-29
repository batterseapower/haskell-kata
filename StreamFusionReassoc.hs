{-# LANGUAGE ExistentialQuantification, BangPatterns, TypeOperators #-}
import Prelude hiding (enumFromTo, concatMap, replicate)

data Stream a = forall s. Stream !(s -> Step a s)  -- a stepper function
                                 !s                -- an initial state

-- | A stream step.
--
--   A step either ends a stream, skips a value, or yields a value
--
data Step a s = Yield a !s
              | Skip    !s
              | Done


-- | Construct an abstract stream from a list.
stream :: [a] -> Stream a
stream xs0 = Stream next xs0
  where
    {-# INLINE next #-}
    next []     = Done
    next (x:xs) = Yield x xs
{-# INLINE [0] stream #-}

-- | Flatten a stream back into a list.
unstream :: Stream a -> [a]
unstream (Stream next s0) = unfold_unstream s0
  where
    unfold_unstream !s = case next s of
      Done       -> []
      Skip    s' -> unfold_unstream s'
      Yield x s' -> x : unfold_unstream s'
{-# INLINE [0] unstream #-}

--
-- /The/ stream fusion rule
--

{-# RULES
"STREAM stream/unstream fusion" forall s.
    stream (unstream s) = s
  #-}


{-# INLINE replicate #-}
replicate n x = unstream (replicateS n x)

{-# INLINE [0] replicateS #-}
replicateS :: Int -> a -> Stream a
replicateS n x = Stream next n
  where
    {-# INLINE next #-}
    next !i | i <= 0    = Done
            | otherwise = Yield x (i-1)

{-# INLINE enumFromTo #-}
enumFromTo x y = unstream (enumFromToS x y)

{-# INLINE [0] enumFromToS #-}
enumFromToS x y = Stream step x
  where
    {-# INLINE step #-}
    step x | x <= y    = Yield x (x + 1)
           | otherwise = Done

data a :!: b = !a :!: !b

{-# INLINE concatMap #-}
concatMap f xs = unstream (concatMapS (stream . f) (stream xs))

{-# INLINE [0] concatMapS #-}
concatMapS :: (a -> Stream b) -> Stream a -> Stream b
concatMapS f (Stream next0 s0) = Stream next (s0 :!: Nothing)
  where
    {-# INLINE next #-}
    next (s :!: Nothing) = case next0 s of
      Done       -> Done
      Skip    s' -> Skip (s' :!: Nothing)
      Yield x s' -> Skip (s' :!: Just (f x))

    next (s :!: Just (Stream g t)) = case g t of
      Done       -> Skip    (s :!: Nothing)
      Skip    t' -> Skip    (s :!: Just (Stream g t'))
      Yield x t' -> Yield x (s :!: Just (Stream g t'))

-- [1,1,2,2,3,3,4,4,5,5,2,2,3,3,4,4,5,5,3,3,4,4,5,5,4,4,5,5,5,5]
main = do
    print $ concatMap (\y -> replicate 2 y) (concatMap (\x -> enumFromTo x 5) (enumFromTo 1 (5 :: Int)))
    --print $ concatMap (\x -> concatMap (\y -> replicate 2 y) (enumFromTo x 5)) (enumFromTo 1 (5 :: Int))
