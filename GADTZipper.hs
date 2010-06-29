{-# LANGUAGE GADTs, Rank2Types #-}
import Control.Applicative


-- data ApplicativeTree a where
--     Pure :: a -> ApplicativeTree a
--     Star :: ApplicativeTree (b -> a) -> ApplicativeTree b -> ApplicativeTree a
-- 
-- evaluate :: Applicative f => ApplicativeTree a -> f a
-- evaluate (Pure x) = pure x
-- evaluate (Star t1 t2) = evaluate t1 <*> evaluate t2

data List a where
    Nil :: List a
    Cons :: a -> List a -> List a

data ZList a where
    StopList :: List a -> ZList a
    Down :: a -> ZList a -> ZList a

reverseConcatList :: List a -> List a -> List a
reverseConcatList Nil         ys = ys
reverseConcatList (Cons x xs) ys = reverseConcatList xs (Cons x ys)

startList :: List a -> (List a, ZList a)
startList xs = (xs, StopList Nil)

rebuildList :: List a -> ZList a -> List a
rebuildList xs (StopList ys) = reverseConcatList ys xs
rebuildList xs (Down x zl)   = rebuildList (Cons x xs) zl

down :: List a -> ZList a -> (List a, ZList a)
down (Cons x xs) zl = (xs, Down x zl)


data Tree a where
    Leaf :: a -> Tree a
    Branch :: Tree a -> Tree a -> Tree a

data ZTree a where
    StopTree :: ZTree a
    RightTree :: Tree a -> ZTree a -> ZTree a
    LeftTree  :: ZTree a -> Tree a -> ZTree a

startTree :: Tree a -> (Tree a, ZTree a)
startTree t = (t, StopTree)

rebuildTree :: Tree a -> ZTree a -> Tree a
rebuildTree t StopTree       = t
rebuildTree t (RightTree tl ztr) = rebuildTree (Branch tl t) ztr
rebuildTree t (LeftTree  ztl tr) = rebuildTree (Branch t tr) ztl

leftTree :: Tree a -> ZTree a -> (Tree a, ZTree a)
leftTree (Branch tl tr) zt = (tl, LeftTree zt tr)


-- Free algebra on the Applicative typeclass, plus an "Unexpanded" injection from the standard type
data ApplicativeTree f a where
    Unexpanded :: f a -> ApplicativeTree f a
    Pure :: a -> ApplicativeTree f a
    Star :: ApplicativeTree f (b -> a) -> ApplicativeTree f b -> ApplicativeTree f a

evaluate :: Applicative f => ApplicativeTree f a -> f a
evaluate (Unexpanded fx) = fx
evaluate (Pure x) = pure x
evaluate (Star t1 t2) = evaluate t1 <*> evaluate t2


-- GADT zipper. What the hell do these types mean?? I derived them by performing unification on the "rebuild" algorithm
-- with pencil and paper, so the definitions typechecked. But I have idea what the types really *mean*.
--
-- Perhaps:
--   zt :: ZApplicativeTree f a a'
-- If (zt) *consumes* an (ApplicativeTree f a) to produce an (ApplicativeTree f a')
data ZApplicativeTree f a a' where
    StopApplicativeTree  :: ZApplicativeTree f a a
    RightApplicativeTree :: ApplicativeTree f (b -> a) -> ZApplicativeTree f a a' -> ZApplicativeTree f b a'
    LeftApplicativeTree  :: ZApplicativeTree f b a' -> ApplicativeTree f a -> ZApplicativeTree f (a -> b) a'

startApplicativeTree :: ApplicativeTree f a -> (ApplicativeTree f a, ZApplicativeTree f a a)
startApplicativeTree t = (t, StopApplicativeTree)

rebuildApplicativeTree :: ApplicativeTree f a -> ZApplicativeTree f a a' -> ApplicativeTree f a'
rebuildApplicativeTree t StopApplicativeTree = t
rebuildApplicativeTree t (RightApplicativeTree tl ztr) = rebuildApplicativeTree (Star tl t) ztr
rebuildApplicativeTree t (LeftApplicativeTree  ztl tr) = rebuildApplicativeTree (Star t tr) ztl

leftApplicativeTree :: ApplicativeTree f a -> ZApplicativeTree f a a' -> (forall b. ApplicativeTree f (b -> a) -> ZApplicativeTree f (b -> a) a' -> r) -> r
leftApplicativeTree (Star tl tr) zt k = k tl (LeftApplicativeTree zt tr)

rightApplicativeTree :: ApplicativeTree f a -> ZApplicativeTree f a a' -> (forall b. ApplicativeTree f b -> ZApplicativeTree f b a' -> r) -> r
rightApplicativeTree (Star tl tr) zt k = k tr (RightApplicativeTree tl zt)


main = return ()