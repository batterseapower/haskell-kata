-- Translated from Matt Might's article: http://matt.might.net/articles/implementation-of-kcfa-and-0cfa/k-CFA.scm
-- Extended with less ad-hoc support for halting

import Control.Applicative (liftA2, liftA3)
import qualified Control.Monad.State as State
import Control.Monad

import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace


type Var = String
type Label = Int
data Exp = Halt | Ref Var | Lam Label [Var] Call deriving (Eq, Ord, Show) -- In the Haskell tradition, I'm only allowing unary abstraction/application
data Call = Call Label Exp [Exp] deriving (Eq, Ord, Show)

-- Abstract state space
data State = State Call BEnv Store Time deriving (Eq, Ord, Show)
-- A binding environment maps variables to addresses
type BEnv = M.Map Var Addr
-- A store maps addresses to denotable values
type Store = M.Map Addr Denotable
-- | An abstact denotable value is a set of possible values
type Denotable = S.Set Value
-- For pure CPS, closures are the only kind of value
type Value = Clo
-- Closures pair a lambda-term with a binding environment that determines
-- the values of its free variables
data Clo = Closure (Label, [Var], Call) BEnv | HaltClosure deriving (Eq, Ord, Show)
-- Addresses can point to values in the store. In pure CPS, the only kind of addresses are bindings
type Addr = Bind
-- A binding is minted each time a variable gets bound to a value
data Bind = Binding Var Time deriving (Eq, Ord, Show)
-- In k-CFA, time is a bounded memory of program history.
-- In particular, it is the last k call sites through which
-- the program has traversed.
type Time = [Label]

storeInsert :: Addr -> Value -> Store -> Store
storeInsert a v s = M.insertWith S.union a (S.singleton v) s

storeJoin :: Store -> Store -> Store
storeJoin = M.unionWith S.union

-- k-CFA parameters

k :: Int
k = 1

tick :: Label -> Time -> Time
tick l t = take k (l:t)

alloc :: Time -> Var -> Addr
alloc t x = Binding x t

-- k-CFA abstract interpreter

atomEval :: BEnv -> Store -> Exp -> Denotable
atomEval benv store Halt    = S.singleton HaltClosure
atomEval benv store (Ref x) = case M.lookup x benv of
    Nothing   -> error "Var unbound in BEnv"
    Just addr -> case M.lookup addr store of
        Nothing -> error "Address unbound in Store"
        Just d  -> d
atomEval benv _     (Lam l v c) = S.singleton (Closure (l, v, c) benv)

next :: State -> S.Set State
next s@(State (Call l fun args) benv store time)
  = -- trace ("next" ++ show s) $
    S.fromList [ State call' benv'' store' time'
               | clo <- S.toList procs
               , (formals, call', benv') <- case clo of
                    Closure (_, formals, call') benv' -> [(formals, call', benv')]
                    HaltClosure                       -> []
               , let bindings = map (alloc time) formals
                     benv'' = foldr (\(formal, binding) benv' -> M.insert formal binding benv') benv' (formals `zip` bindings)
               , params <- S.toList (transpose paramss)
               , let store' = foldr (\(binding, params) store  -> storeInsert binding params store) store (bindings `zip` params)]
  where time' = tick l time
        procs  = atomEval benv store fun
        paramss = map (atomEval benv store) args

transpose :: Ord a => [S.Set a] -> S.Set [a]
transpose []         = S.singleton []
transpose (arg:args) = S.fromList [arg:args | args <- S.toList (transpose args), arg <- S.toList arg]

-- State-space exploration

explore :: S.Set State -> [State] -> S.Set State
explore seen [] = seen
explore seen (todo:todos)
  | todo `S.member` seen = explore seen todos
  | otherwise            = explore (S.insert todo seen) (S.toList (next todo) ++ todos)

-- User interface

summarize :: S.Set State -> Store
summarize states = S.fold (\(State _ _ store' _) store -> store `storeJoin` store') M.empty states

monovariantStore :: Store -> M.Map Var (S.Set Exp)
monovariantStore store = M.foldrWithKey (\(Binding x _) d res -> M.alter (\mb_exp -> Just $ maybe id S.union mb_exp (S.map monovariantValue d)) x res) M.empty store

monovariantValue :: Value -> Exp
monovariantValue (Closure (l, v, c) _) = Lam l v c
monovariantValue HaltClosure           = Halt

analyse :: Call -> M.Map Var (S.Set Exp)
analyse e = monovariantStore (summarize (explore S.empty [State e M.empty M.empty []]))

-- Helper functions for constructing syntax trees

type UniqM = State.State Int

newLabel :: UniqM Int
newLabel = State.state (\i -> (i, i + 1))

runUniqM :: UniqM a -> a
runUniqM = fst . flip State.runState 0


ref :: Var -> UniqM Exp
ref = return . Ref

lam :: [Var] -> UniqM Call -> UniqM Exp
lam xs c = liftA2 (flip Lam xs) newLabel c

call :: UniqM Exp -> [UniqM Exp] -> UniqM Call
call e es = liftA3 Call newLabel e (sequence es)

let_ :: Var -> UniqM Exp -> UniqM Call -> UniqM Call
let_ x e c = call (lam [x] c) [e]

halt :: UniqM Exp -> UniqM Call
halt e = call (return Halt) [e]

-- The Standard Example
--
-- In direct style:
--
-- let id = \x -> x
--     a = id (\z -> halt z)
--     b = id (\y -> halt y)
-- in halt b
standardExample :: UniqM Call
standardExample = 
  let_ "id" (lam ["x", "k"] (call (ref "k") [ref "x"])) $
  call (ref "id") [lam ["z"] (halt (ref "z")),
                   lam ["a"] (call (ref "id") [lam ["y"] (halt (ref "y")),
                                               lam ["b"] (halt (ref "b"))])]


main = forM_ (M.toList (analyse (runUniqM standardExample))) $ \(x, es) -> do
    putStrLn (x ++ ":")
    mapM_ (putStrLn . ("  " ++) . show) (S.toList es)