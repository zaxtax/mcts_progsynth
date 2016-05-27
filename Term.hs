module Main where

import           Prelude                         hiding (lookup)
import           Data.HashMap.Strict             hiding (map, fromList)
import           System.Random.MWC               as MWC
import           System.Random.MWC.Distributions as MD
import qualified Data.Vector                     as V
import           Control.Monad
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Maybe
import           Data.Maybe

-- This program generates a program which when evaluated
-- returns of list of length 5.

-- Right now, this program is very brittle and involves
-- no learning. The objective function is very strict (0/1)
-- and we do not take into account context or types when
-- generating sub-expressions.

-- Note: No code yet exists to cut off programs which are
-- likely to diverge.

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

poor_bias, good_bias :: V.Vector Double

-- poor_bias and good_bias encode at which frequency
-- we should generate different terms. poor_bias
-- encodes a uniform bias, which is likely to
-- generate many useless programs. good_bias encodes
-- a bias that prefers Cons which is likely what we
-- will need to produce a list.

poor_bias = V.fromList [1,1,1,1,1,1]
good_bias = V.fromList [3,1,1,2,2,4]

type Fuel   = Integer
type Fueled = StateT Fuel (MaybeT IO)

runFueled :: Fueled a -> Fuel -> IO (Maybe (a, Fuel))
runFueled s fuel = runMaybeT (runStateT s fuel)

generateTerm :: MWC.GenIO -> Fueled Term
generateTerm g = do
    modify (subtract 1)
    fuel <- get
    case fuel > 0 of
      True  -> categorical good_bias g >>= go
      False -> mzero
  -- where MCTS logic goes
  where go :: Int -> Fueled Term
        go 0 = Var     <$> pure "x"
        go 1 = Lam "x" <$> generateTerm g
        go 2 = App     <$> generateTerm g
                       <*> generateTerm g
        go 3 = Lit     <$> pure 3
        go 4 = return Nil
        go 5 = Cons    <$> generateTerm g
                       <*> generateTerm g
              
void_bool True  = Just ()
void_bool False = Nothing

objective :: Val -> Maybe ()
objective (L xs) = void_bool $ length xs == 5
objective _      = Nothing

getPassingTerm :: (Val -> Maybe ())
               -> MWC.GenIO
               -> IO Term
getPassingTerm o g = do
  x <- runFueled (generateTerm g) 2000
  case go x of
    Just x' -> return x'
    Nothing -> getPassingTerm o g
 where go x = do
         (term, _) <- x
         o (eval term empty)
         return term

main :: IO ()
main = do
    g <- createSystemRandom
    x <- replicateM 10 (getPassingTerm objective g)
    mapM_ print x
