module Main where

import           Prelude                         hiding (lookup)
import           Data.HashMap.Strict             hiding (map, fromList)
import           System.Random.MWC               as MWC
import           System.Random.MWC.Distributions as MD
import qualified Data.Vector                     as V
import           Control.Monad
import           Data.Maybe

type Name = String
type Env  = HashMap String Val

data Val
    = I Integer
    | L [Val]
    | F (Val -> Val)
    | Error

instance Eq Val where
  I x == I y         = x == y
  L x == L y         = x == y
  _ == _             = False

instance Show Val where
  show (I x) = "I " ++ show x
  show (L x) = "L " ++ show x
  show (F x) = "function val"
  show Error = "error val"

data Term
    = Var  Name
    | Lam  Name Term
    | App  Term Term
    | Lit  Integer
    | Nil
    | Cons Term Term
  deriving (Eq, Show)

app :: Val -> Val -> Val
app (F f') x' = f' x'
app _      _  = Error

cons :: Val -> Val -> Val
cons Error _      = Error -- Does cons care about its arguments?
cons x     (L xs) = L (x:xs)
cons _     _      = Error

eval :: Term -> Env -> Val
eval (Var  x)      env = maybe Error id (lookup x env)
eval (Lam  x body) env = F (\ x' -> eval body (insert x x' env))
eval (App  f x)    env = app (eval f env) (eval x env)
eval (Lit  a)      _   = I a
eval Nil           _   = L []
eval (Cons x xs)   env = cons (eval x env) (eval xs env)

poor_bias = V.fromList [1,1,1,1,1,1]
good_bias = V.fromList [3,1,1,2,2,4]


generateTerm :: MWC.GenIO -> IO Term
generateTerm g = do
    categorical good_bias g >>= go
  -- where MCTS logic goes
  where go 0 = return (Var "x")
        go 1 = Lam "x" <$> generateTerm g
        go 2 = App     <$> generateTerm g
                       <*> generateTerm g
        go 3 = return (Lit 3)
        go 4 = return Nil
        go 5 = Cons    <$> generateTerm g
                       <*> generateTerm g
              
objective :: Val -> Bool
objective (L xs) = length xs == 5
objective _      = False

getPassingTerm :: (Val -> Bool)
               -> MWC.GenIO
               -> IO Term
getPassingTerm o g = do
  x <- generateTerm g
  case o (eval x empty) of
    True  -> return x
    False -> getPassingTerm o g


main :: IO ()
main = do
    g <- createSystemRandom
    x <- replicateM 40 (getPassingTerm objective g)
    mapM_ print x
