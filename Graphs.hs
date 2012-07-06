{-# LANGUAGE ScopedTypeVariables #-}
module Graphs where

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe


type LGraph node edge = [(node, [(edge, node)])]

shortcutEdges :: forall node edge.
                 Ord node
              => (node -> Bool)
              -> (edge -> node -> edge -> edge)
              -> LGraph node edge
              -> LGraph node edge
shortcutEdges should_shortcut combine g = evalState visit_graph M.empty
  where
    g_map = M.fromList g

    --visit_graph :: State (M.Map node [(edge, node)]) (LGraph node edge)
    visit_graph = sequence $ flip mapMaybe g $ \(n, ens) -> do
        guard (not (should_shortcut n))
        return $ liftM ((,) n) $ visit S.empty ens

    --visit :: [node] -> [(edge, node)] -> State (M.Map node [(edge, node)]) [(edge, node)]
    -- Given the outgoing edges for some node, returns all the outgoing edges for that node
    -- after shortcutting
    visit path ens = concatMapM (uncurry (visit' path)) ens

    --visit' :: S.Set node -> edge -> node -> State (M.Map node [(edge, node)]) [(edge, node)]
    -- Given an edge label and the node reached via that label, returns all the nodes reached
    -- after shortcutting
    visit' path e n' | n' `S.member` path       = return []        -- Doesn't contribute any extra paths: all paths will considered by a caller
                     | not (should_shortcut n') = return [(e, n')] -- Won't be shortcutted away, no need to look further
                     | otherwise                = do
                        -- Since n' is not in the path, we can try to memoise
                        mb_res <- liftM (M.lookup n') get
                        res <- case mb_res of
                          Just res -> return res
                          Nothing  -> do
                            res <- visit (S.insert n' path) (M.findWithDefault (error "shortcutEdges") n' g_map)
                            modify (M.insert n' res)
                            return res
                        return $ map (first (combine e n')) res

sccs :: Ord node
     => LGraph node edge
     -- -> LGraph (LGraph node edge) [(edge, node)]
     -> [[node]]
sccs g = case execState strongconnect_graph (0, M.empty, [], []) of (_, _, _, sccs) -> sccs
  where
    g_map = M.fromList g

    -- Observations about Tarjan's algorithm:
    --  1. strongconnect(v) is only called if v.index is undefined
    --  2. Vertex v's lowlink is only mutated by strongconnect(v)
    --  3. Once index is set it is never changed
    --
    -- We can use these facts to build an implementation that makes minimal use of the state monad

    strongconnect_graph = forM_ g $ \(n, ens) -> do
      ix_defined <- liftM (\(_, ixs, _, _) -> n `M.member` ixs) get
      unless ix_defined $ void $ strongconnect n ens

    -- (strongconnect n ens) returns index of a node n' reachable from n such that that index[n'] < index[n],
    -- if possible. Otherwise returns index[n].
    strongconnect n ens = do
      ix <- state $ \(next_ix, ixs, s, sccs) -> (next_ix, (next_ix + 1, M.insert n next_ix ixs, n:s, sccs))
      lowlink <- (\f -> foldM f ix ens) $ \lowlink (e, n') -> do
        (mb_ix', in_s') <- liftM (\(_, ixs, s, _) -> (M.lookup n' ixs, n' `elem` s)) get
        case mb_ix' of
          Nothing              -> liftM (lowlink `min`) $ strongconnect n' (M.findWithDefault (error "sccs") n' g_map) -- Successor not yet visited: recurse on it
          Just ix' | in_s'     -> return $ lowlink `min` ix'                                                           -- Successor is in the stack and hence the current SCC
                   | otherwise -> return lowlink
      -- Since lowlink is at most ix, this condition can only be true if we failed to find a node reachable
      -- from n with a lower index. We use this as our cue to form a new SCC.
      when (lowlink == ix) $ do
        modify $ \(next_ix, ixs, s, sccs) -> let (scc, _n:s') = span (/= n) s in (next_ix, ixs, s', (n:scc) : sccs)
      -- Return this nodes final lowlink for use when computing the predecessors lowlink
      return lowlink

sccs' :: forall node edge.
         (Ord node, Ord edge) -- FIXME: can relax (Ord edge) requirement really..
      => LGraph node edge
      -> LGraph (LGraph node edge) [(edge, node)]
sccs' g = case execState strongconnect_graph (0, M.empty, [], [], M.empty, M.empty) of (_, _, _, sccs, _, _) -> sccs
  where
    g_map = M.fromList g

    -- Observations about Tarjan's algorithm:
    --  1. strongconnect(v) is only called if v.index is undefined
    --  2. Vertex v's lowlink is only mutated by strongconnect(v)
    --  3. Once index is set it is never changed
    --  4. Nodes occur in the stack in decreasing order of index
    --
    -- We can use these facts to build an implementation that makes minimal use of the state monad

    strongconnect_graph = forM_ g $ \(n, ens) -> do
      ix_defined <- liftM (\(_, ixs, _, _, _, _) -> n `M.member` ixs) get
      unless ix_defined $ void $ strongconnect n ens

    strongconnect :: node -> [(edge, node)]
                  -> State (Int,
                            M.Map node Int,
                            [node],
                            LGraph (LGraph node edge) [(edge, node)],
                            M.Map node               [(edge, node)],
                            M.Map (LGraph node edge) [(edge, node)])
                           (Int,
                            Maybe (LGraph node edge))
    -- (strongconnect n ens) returns:
    --  1. Index of a node n' reachable from n such that that index[n'] < index[n],
    --     if possible. Otherwise returns index[n].
    --  2. The SCC graph node of the newly-created SCC. If no new SCC was created then n is guaranteed
    --     to still be on the stack (which occurs iff we managed to find a suitable index[n'])
    strongconnect n ens = do
      ix <- state $ \(next_ix, ixs, s, sccs, all_internal_ens, all_external_ens) -> (next_ix, (next_ix + 1, M.insert n next_ix ixs, n:s, sccs, all_internal_ens, all_external_ens))
      (lowlink, internal_ens, external_ens) <- (\f -> foldM f (ix, [], M.empty) ens) $ \(lowlink, internal_ens, external_ens) (e, n') -> do
        (mb_ix', in_s') <- liftM (\(_, ixs, s, _, _, _) -> (M.lookup n' ixs, n' `elem` s)) get
        (lowlink, mb_scc) <- case mb_ix' of
                                  -- Successor not yet visited: recurse on it
                                  -- Since the index assigned to n' > ix, it is guaranteed that n will still be on the
                                  -- stack when we return.
                                  -- TODO: include an edge depending on in_s'
          Nothing              -> liftM (first (lowlink `min`)) $ strongconnect n' (M.findWithDefault (error "sccs") n' g_map)
                                  -- Successor is in the stack and hence the current SCC
                                  -- TODO: include this edge as an internal edge in the SCC
          Just ix' | in_s'     -> return (lowlink `min` ix', Nothing)
                                  -- Successor visited but not in stack: it is already part of another SCC
                                  -- TODO: prepare to emit an edge from this node to that other SCC
                   | otherwise -> do scc <- liftM (\(_, _, _, sccs, _, _) -> head [scc | (scc, _) <- sccs, any (\(n'', _) -> n'' == n') scc]) get
                                     return (lowlink, Just scc)
        (internal_ens, external_ens) <- return $ case mb_scc of
                                          Nothing  -> ((e, n'):internal_ens, external_ens)
                                          Just scc -> (internal_ens, M.insertWith (++) scc [(e, n')] external_ens)
        return (lowlink, internal_ens, external_ens)
      -- Record discovered internal/external edges
      modify $ \(next_ix, ixs, s, sccs, all_internal_ens, all_external_ens) -> (next_ix, ixs, s, sccs, M.insert n internal_ens all_internal_ens, M.unionWith (++) external_ens all_external_ens)
      -- Since lowlink is at most ix, this condition can only be true if we failed to find a node reachable
      -- from n with a lower index. We use this as our cue to form a new SCC.
      mb_scc <- if (lowlink == ix)
                 -- NB: because nodes on the stack are in decreasing order of index, this operation never pops a node with index < ix
                 then do scc <- state $ \(next_ix, ixs, s, sccs, all_internal_ens, all_external_ens) -> let (s_scc, _n:s') = span (/= n) s
                                                                                                            scc = [(n, M.findWithDefault (error "sccs") n all_internal_ens) | n <- n:s_scc]
                                                                                                            all_external_ens' = [(ens, scc) | (scc, ens) <- M.toList all_external_ens]
                                                                                                        in (scc, (next_ix, ixs, s', (scc, all_external_ens') : sccs, all_internal_ens, M.empty))
                         return (Just scc)
                 else return Nothing
      -- Return this nodes final lowlink for use when computing the predecessors lowlink
      return (lowlink, mb_scc)

-- Given a graph, returns:
--  1. An acyclic graph of the strongly connected components of the input graph.
--     Each SCC is identified by a unique Int.
--  2. A mapping from Ints to the "sub-graph" corresponding to each SCC. Each sub-graph
--     contains all the nodes in the SCC as well as any edges between those nodes.
--     Note that in particular the sub-graph for an acyclic SCC will contain exactly one node and no edges.
--
-- Uses an adaptation of Tarjan's algorithm <http://en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm>
sccs'' :: forall node edge.
          Ord node
       => LGraph node edge
       -> (LGraph Int [(edge, node)],
           IM.IntMap (LGraph node edge))
sccs'' g = case execState strongconnect_graph (0, M.empty, [], [], IM.empty, M.empty, IM.empty) of (_, _, _, sccs, scc_datas, _, _) -> (sccs, scc_datas)
  where
    g_map = M.fromList g

    -- Observations about Tarjan's algorithm:
    --  1. strongconnect(v) is only called if v.index is undefined
    --  2. Vertex v's lowlink is only mutated by strongconnect(v)
    --  3. Once index is set it is never changed
    --  4. Nodes occur in the stack in decreasing order of index
    --
    -- We can use these facts to build an implementation that makes minimal use of the state monad

    strongconnect_graph = forM_ g $ \(n, ens) -> do
      ix_defined <- liftM (\(_, ixs, _, _, _, _, _) -> n `M.member` ixs) get
      unless ix_defined $ void $ strongconnect n ens

    -- (strongconnect n ens) returns:
    --  1. Index of a node n' reachable from n such that that index[n'] < index[n],
    --     if possible. Otherwise returns index[n].
    --  2. Whether we didn't just create a new SCC containing n. If no new SCC was created then n is guaranteed
    --     to still be on the stack (which occurs iff we managed to find a suitable index[n'])
    --
    -- Precondition: there is no assigned index for n
    strongconnect :: node -> [(edge, node)]
                  -> State (-- Next node index to assign
                            Int,
                            -- Mapping from nodes to their assigned index (if any)
                            -- NB: after the node has been removed from the stack, we update the Int in the mapping
                            -- to instead be the lowlink of the SCC it was assigned to. This is OK because we don't
                            -- need the raw index of the node after that point: we only need record the fact that
                            -- it had some index at a point in the past
                            M.Map node Int,
                            -- Stack containing expanded nodes that are not presently in a SCC
                            [node],
                            -- Work-in-progress graph of SCC
                            LGraph Int [(edge, node)],
                            -- Work-in-progress SCC sub-graph mapping
                            IM.IntMap (LGraph node edge),
                            -- Records all discovered "internal" edges from expanded nodes to somewhere *within* their SCC
                            M.Map node [(edge, node)],
                            -- Records all discovered "external" edges from the current SCC-in-progress to some other (already existant) SCC
                            -- It might seem more obvious to use a [([(edge, node)], Int)] here, but that makes it awkward to common up multiple
                            -- edges from this SCC going to the same external SCC
                            IM.IntMap  [(edge, node)])
                           (Int, Bool)
    strongconnect n ens = do
      ix <- state $ \(next_ix, ixs, s, sccs, scc_datas, all_internal_ens, all_external_ens) -> (next_ix, (next_ix + 1, M.insert n next_ix ixs, n:s, sccs, scc_datas, all_internal_ens, all_external_ens))
      (lowlink, internal_ens, external_ens) <- (\f -> foldM f (ix, [], IM.empty) ens) $ \(lowlink, internal_ens, external_ens) (e, n') -> do
        (mb_ix', in_s') <- liftM (\(_, ixs, s, _, _, _, _) -> (M.lookup n' ixs, n' `elem` s)) get
        (lowlink, mb_scc) <- case mb_ix' of
                                  -- Successor not yet visited: recurse on it
                                  -- Whether we add an internal or external edge depends on whether the recursive call created an SCC or not.
                                  -- If it did create an SCC, that SCC will be identified by lowlink'
          Nothing              -> do (lowlink', in_s') <- strongconnect n' (M.findWithDefault (error "sccs") n' g_map)
                                     return (lowlink `min` lowlink', if in_s' then Nothing else Just lowlink')
                                  -- Successor is in the stack and hence the current SCC, so record an internal edge
          Just ix' | in_s'     -> return (lowlink `min` ix', Nothing)
                                  -- Successor visited but not in stack: it is already part of another SCC, so record an external edge
                                  -- NB: this makes use of my hack whereby ix' will actually be a SCC lowlink for such successors
                   | otherwise -> return (lowlink, Just ix')
        (internal_ens, external_ens) <- return $ case mb_scc of
                                          Nothing  -> ((e, n'):internal_ens, external_ens)
                                          Just scc -> (internal_ens, IM.insertWith (++) scc [(e, n')] external_ens)
        return (lowlink, internal_ens, external_ens)
      -- Record accumulated internal/external edges. We don't need to record them as we go along because they can only possibly be used by one of our callers, not our callees
      modify $ \(next_ix, ixs, s, sccs, scc_datas, all_internal_ens, all_external_ens) -> (next_ix, ixs, s, sccs, scc_datas, M.insert n internal_ens all_internal_ens, IM.unionWith (++) external_ens all_external_ens)
      -- Since lowlink is at most ix, this condition can only be true if we failed to find a node reachable
      -- from n with a lower index. We use this as our cue to form a new SCC.
      in_s <- if (lowlink == ix)
               -- NB: because nodes on the stack are in decreasing order of index, this operation never pops a node with index < ix
              then do modify $ \(next_ix, ixs, s, sccs, scc_datas, all_internal_ens, all_external_ens) -> let (s_scc, _n:s') = span (/= n) s
                                                                                                              scc = [(n, M.findWithDefault (error "sccs") n all_internal_ens) | n <- n:s_scc]
                                                                                                              all_external_ens' = [(ens, scc) | (scc, ens) <- IM.toList all_external_ens]
                                                                                                              -- Replace node indexes with the lowlink of the SCC they were assigned to (a small hack to save one map lookup):
                                                                                                              ixs' = foldr (\n -> M.insert n lowlink) ixs (n:s_scc)
                                                                                                          in (next_ix, ixs', s', (lowlink, all_external_ens') : sccs, IM.insert lowlink scc scc_datas, all_internal_ens, IM.empty)
                      return False
              else return True
      -- Return this nodes final lowlink for use when computing the predecessors lowlink
      return (lowlink, in_s)

obscure :: Ord node => LGraph node edge -> LGraph Int edge
obscure g = [(to_key n, [(e, to_key n') | (e, n') <- ens]) | (n, ens) <- g]
  where key_map = M.fromList [(n, i) | ((n, _), i) <- g `zip` [0..]]
        to_key n = M.findWithDefault (error "obscure") n key_map


{-
sccs g = search_graph -- FIXME
  where
    g_map = M.fromList g

    allocatePreorderNumber n = state $ \(next_pon, pons, s, p, assigned, sccs) -> case M.lookup n pons of
      Just pon -> ((False, pon),      (next_pon,     pons,                     s, p, assigned, sccs))
      Nothing  -> ((True,  next_pon), (next_pon + 1, M.insert n next_pon pons, s, p, assigned, sccs))

    search_graph = case execState (mapM_ (uncurry search) g) (0, M.empty, [], [], S.empty, []) of (_, _, _, _, _, sccs) -> sccs

    search n ens = do
      (fresh, pon) <- allocatePreorderNumber n
      if not fresh
       then return (Just pon)
       else do
        -- Push onto both stacks
        modify $ \(next_pon, pons, s, p, assigned, sccs) -> (next_pon, pons, n:s, (n, pon):p, assigned, sccs)
        -- Consider paths
        forM ens $ \(e, n') -> do
          mb_n'_pon <- search n' (M.findWithDefault (error "sccs") n' g_map)
          case mb_n'_pon of
            -- PON was not yet assigned, the recursive search was enough
            Nothing -> return ()
            -- PON was already assigned, need to mess with p
            Just n'_pon -> modify $ \(next_pon, pons, s, p, assigned, sccs) -> if n' `S.member` assigned
                                                                               then (next_pon, pons, s, p, assigned, sccs)
                                                                               else (next_pon, pons, s, dropWhile (\(_, p_pon) -> p_pon > n'_pon) p, assigned, sccs)
        modify $ \(next_pon, pons, s, p, assigned, sccs) -> case p of
          (p_head, _):p_tail | p_head == n -> (next_pon, pons, s, p_tail, assigned `S.union` S.fromList scc, scc : sccs)
            where (s1, _n:s2) = span (/= n) s
                  scc = n:s1
          _ -> (next_pon, pons, s, p, assigned, sccs)
        return Nothing
-}


g0 = [("Root", [("a", "Residual Loop")
               ,("b", "Fully Shortcutted Loop")
               ,("c", "Shortcutted Loop")
               ,("d", "Indirect Non-Loop 1")
               ,("e", "Indirect Non-Loop 2")])
     ,("Residual Loop", [("f", "Residual Loop")])
     ,("Fully Shortcutted Loop", [("g", "Fully Shortcutted Loop")])
     ,("Shortcutted Loop", [("h", "Shortcutted Loop 1")])
     ,("Shortcutted Loop 1", [("i", "Shortcutted Loop")])
     ,("Indirect Non-Loop 1", [("j", "Indirect Non-Loop 2")])
     ,("Indirect Non-Loop 2", [])]

test1 = shortcutEdges (`elem` ["Fully Shortcutted Loop", "Shortcutted Loop 1"]) (\e1 n e2 -> e1 ++ "(" ++ n ++ ")" ++ e2) g0

test2 = sccs g0

test3 = sccs test1

test4 = sccs'' g0

test5 = sccs'' test1


-- Code below this line stolen from GHC to save time
newtype State s a = State { runState' :: s -> ( a, s ) }

instance Functor (State s) where
    fmap f m  = State $ \s -> case runState' m s of
                              ( r, s' ) -> ( f r, s' )

instance Applicative (State s) where
   pure x   = State $ \s -> ( x, s )
   m <*> n  = State $ \s -> case runState' m s of
                            ( f, s' ) -> case runState' n s' of
                                           ( x, s'' ) -> ( f x, s'' )

instance Monad (State s) where
    return x = State $ \s -> ( x, s )
    m >>= n  = State $ \s -> case runState' m s of
                             ( r, s' ) -> runState' (n r) s'

get :: State s s
get = State $ \s -> ( s, s )

gets :: (s -> a) -> State s a
gets f = State $ \s -> ( f s, s )

put :: s -> State s ()
put s' = State $ \_ -> ( (), s' )

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ( (), f s )

state :: (s -> (a, s)) -> State s a
state f = State $ \s -> case f s of (x, s') -> ( x, s' )


evalState :: State s a -> s -> a
evalState s i = case runState' s i of
                ( a, _ ) -> a


execState :: State s a -> s -> s
execState s i = case runState' s i of
                ( _, s' ) -> s'


runState :: State s a -> s -> (a, s)
runState s i = case runState' s i of
               ( a, s' ) -> (a, s')


-- | Monadic version of concatMap
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)