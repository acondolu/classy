#!/usr/bin/env stack runghc
module Classy (main) where

import Data.List (partition)
import qualified Data.IORef as IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Foldable (foldrM)

main :: IO ()
main = do
  -- (vars, res) <- trans $ Lambda 0 $ App (V 0) (V 0)
  -- (vars, res) <- trans $ Lambda 0 $ Lambda 1 $ V 0
  -- (vars, res) <- trans $ Lambda 0 $ Lambda 1 $ App (App (V 0) (V 0)) (V 1)
  -- (vars, res) <- trans $ Lambda 0 $ Lambda 1 $ App (App (V 0) (V 1)) (App (V 0) (V 1))
  tp $ Lambda 0 $ App (V 0) (V 0)
  tp $ Lambda 0 $ V 0
  tp $ Lambda 0 $ Lambda 1 $ App (V 0) (App (V 1) (V 1))

-- | Translate a lambda term and print the result.
tp :: LTerm -> IO ()
tp t = do
  (env, res) <- trans t
  case env of
    [] -> pure ()
    _ -> error $ "tp: non-empty env: " <> show env
  print res

-- proj2 :: IO Term
-- proj2 = snd <$> trans (Lambda 0 $ Lambda 1 $ V 1)

-- | Nouns (variable names). Opaque, using Ints for simplicity.
type Noun = Int

-- | Classy terms: A and B (two occurrences of channel variable),
-- Pair (angle bracket), Dest (square bracket), Contr (contraction),
-- Hole (weakening).
data Term = A Noun | B Noun | Pair Term Term | Dest Term Term | Contr [Term] | Hole

nouns :: [String]
nouns = ["a", "b", "c", "d", "e", "f", "g"]

instance Show Term where
  show (A n) = nouns !! n
  show (B n) = nouns !! n
  show (Pair t u) = "<" <> show t <> ", " <> show u <> ">"
  show (Dest t u) = "[" <> show t <> ", " <> show u <> "]"
  show (Contr []) = "{}"
  show (Contr [t]) = "{" <> show t <> "}"
  show (Contr (t:ts)) = "{" <> show t <> shows ts <> "}"
    where
      shows [] = ""
      shows (t:ts) = "," <> show t <> shows ts
  show Hole = "@"

-- | A "program" is a list of cuts.
type Program = [Cut]

data Cut = Cut Term Term
  deriving (Show)

-- An attempt to write down a reduction rule (ignore pls).
-- t1 :: Program -> Cut -> Program
-- t1 p (Cut (Pair t1 t2) (Dest u1 u2)) = Cut t1 u1 : Cut t2 u2 : p
-- t1 p (Cut (Dest t1 t2) (Pair u1 u2)) = Cut t1 u1 : Cut t2 u2 : p
-- t1 p c = c : p

-- | Lambda terms.
data LTerm = V Noun | Lambda Noun LTerm | App LTerm LTerm
  deriving (Show)

{-# NOINLINE freshNoun #-}
freshNoun :: IO Noun
freshNoun = unsafePerformIO $ do
  ref <- IORef.newIORef 0
  pure $ do
    n <- IORef.readIORef ref
    IORef.writeIORef ref $! n+1
    pure n

-- | Translation function relies on intermediate mappings
-- from lambda variable names to classy terms, to track the
-- usage of variables after entering a lambda abstraction.
-- Let's call it a "translation environment".
type Env = [(Noun, Term)]

trans :: LTerm -> IO (Env, Term)
trans = go []
  where
    go env (Lambda n t) = do
      (env', t') <- go env t
      let (env'', ns) = gatherVariableOccurrences env' n
      let c = case ns of
                [] -> Hole -- unused variable
                [x] -> x -- used once
                _ -> Contr ns -- used multiple times, contract
      pure (env'', Dest c t')
    -- treat variables and applications the same way
    -- (variables are applied to 0 arguments)
    go env a = do
      ret <- freshNoun -- generate new return variable
      let (head, args) = splitApp a
      let f t (e, res) = do
            (e', t') <- go e t
            pure (e', Pair t' res)
      (env', pair) <- foldrM f (env, B ret) args
      pure ((head, pair) : env', A ret)

-- | Split application into (head, arguments).
splitApp :: LTerm -> (Noun, [LTerm])
splitApp t = go [] t
  where
    go acc (V i) = (i, acc)
    go acc (App t u) = go (u:acc) t
    go _ t = error $ "splitApp: not an application: " <> show t

-- | Gather the occurrences of a variable in a traslation environment,
-- remove them from the environment and return them.
gatherVariableOccurrences :: Env -> Noun -> (Env, [Term])
gatherVariableOccurrences env noun = do
  let (ns, others) = partition (\e -> fst e == noun) env
  (others, map snd ns)
