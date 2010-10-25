{-# LANGUAGE TypeFamilies, EmptyDataDecls, ScopedTypeVariables #-}

import Data.Monoid


type family Force a :: *

data Forced (a :: *)
type instance Force (Forced a) = a


newtype VarF var term value = AVar { unVar :: String }

data TermF var term value
  = Var (Force var)
  | App (Force term) (Force var)
  | Value (Force value)
  | Add (Force term) (Force term)

data ValueF var term value
  = Literal Int
  | Lambda String (Force term)

data SyntaxAlgebra var term value = SyntaxAlgebra {
    varAlgebra :: VarF var term value -> Force var,
    termAlgebra :: TermF var term value -> Force term,
    valueAlgebra :: ValueF var term value -> Force value
  }


type Fix3_1 f g h = f (FixTag3_1 f g h) (FixTag3_2 f g h) (FixTag3_3 f g h)
type Fix3_2 f g h = g (FixTag3_1 f g h) (FixTag3_2 f g h) (FixTag3_3 f g h)
type Fix3_3 f g h = h (FixTag3_1 f g h) (FixTag3_2 f g h) (FixTag3_3 f g h)

data FixTag3_1 f g h
data FixTag3_2 f g h
data FixTag3_3 f g h
type instance Force (FixTag3_1 f g h) = Fix3_1 f g h
type instance Force (FixTag3_2 f g h) = Fix3_2 f g h
type instance Force (FixTag3_3 f g h) = Fix3_3 f g h


type Var   = Fix3_1 VarF TermF ValueF
type Term  = Fix3_2 VarF TermF ValueF
type Value = Fix3_3 VarF TermF ValueF


-- TODO: try doing this as a functor category?
fmap3VarF :: (Force var   -> Force var')
          -> (Force term  -> Force term')
          -> (Force value -> Force value')
           -> VarF var  term  value
         -> VarF var' term' value'
fmap3VarF _var _term _value x = case x of
    AVar x -> AVar x

fmap3TermF :: (Force var   -> Force var')
           -> (Force term  -> Force term')
           -> (Force value -> Force value')
           -> TermF var  term  value
           -> TermF var' term' value'
fmap3TermF var term value e = case e of
     Var x     -> Var (var x)
     App e x   -> App (term e) (var x)
     Value v   -> Value (value v)
     Add e1 e2 -> Add (term e1) (term e2)

fmap3ValueF :: (Force var   -> Force var')
            -> (Force term  -> Force term')
            -> (Force value -> Force value')
            -> ValueF var  term  value
            -> ValueF var' term' value'
fmap3ValueF _var term _value v = case v of
    Literal l  -> Literal l
    Lambda x e -> Lambda x (term e)

foldMap3VarF :: Monoid m
             => (Force var -> m)
             -> (Force term -> m)
             -> (Force value -> m)
             -> VarF var term value
             -> m
foldMap3VarF _var _term _value x = case x of
    AVar _ -> mempty

foldMap3TermF :: Monoid m
              => (Force var -> m)
              -> (Force term -> m)
              -> (Force value -> m)
              -> TermF var term value
              -> m
foldMap3TermF var term value e = case e of
    Var x     -> var x
    App e x   -> term e `mappend` var x
    Value v   -> value v
    Add e1 e2 -> term e1 `mappend` term e2

foldMap3ValueF :: Monoid m
               => (Force var -> m)
               -> (Force term -> m)
               -> (Force value -> m)
               -> ValueF var term value
               -> m
foldMap3ValueF _var term _value v = case v of
    Literal _  -> mempty
    Lambda _ e -> term e


example :: Value
example = Lambda "x" $ Add (Value (Literal 1)) (Var (AVar "x")) `App` AVar "x"


-- fixAlgebra :: SyntaxAlgebra var term value -> SyntaxAlgebra (Fix3_1 var term value) (Fix3_2 var term value) (Fix3_3 var term value)
-- fixAlgebra alg = undefined

applyAlgebra :: forall var term value.
                SyntaxAlgebra var term value
             -> Term -> Force term
             -- -> SyntaxAlgebra (FixTag3_1 VarF TermF ValueF) (FixTag3_2 VarF TermF ValueF) (FixTag3_3 VarF TermF ValueF)
applyAlgebra alg = {- SyntaxAlgebra var term value -- -} term
  where
    var   :: Var -> Force var
    var   = varAlgebra alg . fmap3VarF var term value
    term  :: Term -> Force term
    term  = termAlgebra alg . fmap3TermF var term value
    value :: Value -> Force value
    value = valueAlgebra alg . fmap3ValueF var term value


instance Monoid Int where
    mempty = 0
    mappend = (+)

main = print result
  where
    result = applyAlgebra alg (Value example)

    alg :: SyntaxAlgebra (Forced Int) (Forced Int) (Forced Int)
    alg = SyntaxAlgebra var term value
    
    var x   = 1 + foldMap3VarF id id id x
    term e  = 1 + foldMap3TermF id id id e
    value v = 1 + foldMap3ValueF id id id v
