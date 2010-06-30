{-# LANGUAGE RankNTypes #-}
import Text.PrettyPrint.HughesPJClass

import System.IO.Unsafe

import Control.Monad.State


type UniqM = State [Int]

uniqSupply :: [Int]
uniqSupply = [0..]

runUniq :: UniqM a -> a
runUniq = flip evalState uniqSupply

unique :: UniqM Int
unique = get >>= \(x:ss) -> put ss >> return x


instance Pretty ArrowSyn where
    pPrint Id = text "id"
    pPrint (Comp a1 a2) = pPrint a1 <+> text "." <+> pPrint a2
    pPrint (Arr e) = text "arr" <+> parens (pPrint (Lam e))
    pPrint (First a) = text "first" <+> pPrint a
    pPrint (Foreign e) = text e

instance Pretty Term where
    pPrint = runUniq . pPrintTerm

pPrintTerm (Lam f) = do
    x <- fmap (\i -> "x" ++ show i) unique
    return $ parens $ text "\\" <> text x <+> text "->" <+> pPrint (f (ForeignE x))
pPrintTerm (App e1 e2) = liftM2 (\e1 e2 -> e1 <+> parens e2) (pPrintTerm e1) (pPrintTerm e2)
pPrintTerm (ForeignE e) = return $ text e


data Term = Lam (Term -> Term)
          | App Term Term
          | ForeignE String

data ArrowSyn = Id
              | Comp ArrowSyn ArrowSyn
              | Arr (Term -> Term)
              | First ArrowSyn
              | Foreign String

normalise :: ArrowSyn -> ArrowSyn
normalise m = Arr (ForeignE "fst" `App`) `Comp` go m Arr (\x -> x) `Comp` Arr (\x -> ForeignE "(\\x -> (x, ()))" `App` x)
  where
    -- r a b       -> forall e. (forall d. (d    -> (b, c)) -> p d e)    -> (forall e. (e    -> (a, c)) -> p e e)
    go :: ArrowSyn ->           (          (Term -> Term)   -> ArrowSyn) -> (          (Term -> Term)   -> ArrowSyn)
    go Id           = \k k' -> k k'
    go (Comp a1 a2) = go a2 . go a1
    go (Arr e)      = \k k' -> k (e . k')
    go (First a)    = (\k k' -> k (assoc . k')) . go a . (\k k' -> k (reassoc . k'))
      where assoc   = App (ForeignE "assoc")
            reassoc = App (ForeignE "reassoc")
    go (Foreign e)  = \k k' -> k id `Comp` First (Foreign e) `Comp` Arr k'


non_normaliseds = [
    Id,
    
    First (Arr (ForeignE "f" `App`)),
    Arr (ForeignE "f `cross` id" `App`),
    
    First (Foreign "g" `Comp` Foreign "f"),
    First (Foreign "g") `Comp` First (Foreign "f"),
    
    Arr (ForeignE "id `cross` g" `App`) `Comp` First (Foreign "f"),
    First (Foreign "f") `Comp` Arr (ForeignE "id `cross` g" `App`),
    
    Arr (ForeignE "fst" `App`) `Comp` First (Foreign "f"),
    Foreign "f" `Comp` Arr (ForeignE "fst" `App`),
    
    Arr (ForeignE "assoc" `App`) `Comp` First (First (Foreign "f")),
    First (Foreign "f") `Comp` Arr (ForeignE "assoc" `App`)
  ]

main = forM_ non_normaliseds $ \non_normalised -> do
    putStrLn "== Before"
    print $ pPrint non_normalised
    
    putStrLn "== After"
    print $ pPrint $ normalise non_normalised