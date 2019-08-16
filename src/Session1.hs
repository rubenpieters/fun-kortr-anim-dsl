{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Session1 where

import Prelude hiding (seq)

import Control.Monad (liftM, ap)

import Lens.Micro

-------------------------------
-- define operation data type
-------------------------------

newtype Duration = For Float
  deriving (Eq, Show)

newtype To = To Float
  deriving (Eq, Show)

data Ops obj a where
  Basic :: Duration
        -> Traversal' obj Float
        -> To
        -> Ops obj ()

instance Show (Ops obj a) where
  show (Basic duration traversal target) = "Basic (" ++ show duration ++ ") (..) (" ++ show target ++ ")"

execOp :: obj -> Float -> Ops obj a -> (obj, Either (Ops obj a) a)
execOp obj t (Basic (For duration) traversal (To target)) = let
  newObj = obj & traversal %~ updateValue t duration target
  newDuration = duration - t
  result = if newDuration > 0
    then Left (Basic (For newDuration) traversal (To target))
    else Right ()
  in (newObj, result)

updateValue ::
  Float -> -- time delta
  Float -> -- duration
  Float -> -- target value
  Float -> -- current value
  Float -- new value
updateValue t duration target current = let
  speed = (target - current) * t / duration
  newValue = current + speed
  in if target > current
    then min target newValue
    else max target newValue

-------------------------
-- define dsl data type
-------------------------

data Dsl f a where
  Return :: a -> Dsl f a
  Bind :: f a -> (a -> Dsl f b) -> Dsl f b
  Par :: [Dsl f a] -> ([a] -> Dsl f b)-> Dsl f b

instance Functor (Dsl f) where
  fmap = liftM

instance Applicative (Dsl f) where
  pure = return
  (<*>) = ap

instance Monad (Dsl f) where
  return = Return
  (Return x) >>= k2 = k2 x
  (Bind fa k) >>= k2 = Bind fa (\x -> k x >>= k2)
  (Par fs k) >>= k2 = Par fs (\x -> k x >>= k2)

seq :: [Dsl (Ops obj) ()] -> Dsl (Ops obj) ()
seq [] = Return ()
seq (anim:r) = anim >> seq r

par :: [Dsl (Ops obj) ()] -> Dsl (Ops obj) ()
par fs = Par fs (\_ -> Return ())

basic :: Duration -> Traversal' obj Float -> To -> Dsl (Ops obj) ()
basic duration traversal target = Bind (Basic duration traversal target) (\_ -> Return ())

type XY = (Float, Float)

r :: Dsl (Ops XY) ()
r = basic (For 0.5) (_1) (To 50)

u :: Dsl (Ops XY) ()
u = basic (For 0.5) (_2) (To 50)

initial :: XY
initial = (0, 0)

upThenRight :: Dsl (Ops XY) ()
upThenRight = seq [u, r]

diagonal :: Dsl (Ops XY) ()
diagonal = par [u, r]

execDsl :: obj -> Float -> Dsl (Ops obj) a -> (obj, Dsl (Ops obj) a)
execDsl obj t (Return a) = (obj, Return a)
execDsl obj t (Bind fa k) = let
  (newObj, eResult) = execOp obj t fa
  in case eResult of
       Right a -> (newObj, k a)
       Left remainingOp -> (newObj, Bind remainingOp k)
execDsl obj t (Par fs k) = let
  (newObj, remaining) = applyOps obj t fs
  in case returnValues remaining of
       Right as -> (newObj, k as)
       Left () -> (newObj, Par remaining k)

applyOps :: obj -> Float -> [Dsl (Ops obj) a] -> (obj, [Dsl (Ops obj) a])
applyOps obj t [] = (obj, [])
applyOps obj t (op:r) = let
  -- apply first operation
  (obj', op') = execDsl obj t op
  -- apply the rest of the operations
  (obj'', ops) = applyOps obj' t r
  in (obj'', op' : ops)

returnValues :: [Dsl f a] -> Either () [a]
returnValues [] = Right []
returnValues ((Return a):r) = do
  l <- returnValues r
  return (a : l)
returnValues _ = Left ()




------------------------------
-- define running operations
------------------------------

------------------------------
-- define running animations
------------------------------


undefs :: [a]
undefs = undefined : undefs

instance (forall a. Show (f a)) => Show (Dsl f a) where
  show (Bind fa k) = "Bind (" ++ show fa ++ ") (" ++ (show (k undefined)) ++ ")"
  show (Par fs k) = "Par (" ++ show fs ++ ") (" ++ (show (k undefs)) ++ ")"
  show (Return a) = "Return"

