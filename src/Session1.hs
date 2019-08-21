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
  deriving (Show, Eq)
newtype To = To Float
  deriving (Show, Eq)

data Operation obj a where
  Basic :: Duration -> Traversal' obj Float -> To -> Operation obj ()
  Get :: Lens' obj x -> Operation obj x
  Set :: Traversal' obj x -> x -> Operation obj ()
  Create :: (obj -> (obj, Int)) -> Operation obj Int
  Delete :: (Int -> obj -> obj) -> Int -> Operation obj ()
  Delay :: Duration -> Operation obj ()

instance Show (Operation obj a) where
  show (Basic duration lens target) = "Basic (" ++ show duration ++ ") (..) (" ++ show target ++ ")"

-- define u / r

type XY = (Float, Float)

u1 :: Operation XY ()
u1 = error "not implemented"

r1 :: Operation XY ()
r1 = error "not implemented"

-------------------------------
-- define animation data type
-------------------------------

data Animation f a where
  Bind :: f a -> (a -> Animation f b) -> Animation f b
  Return :: a -> Animation f a
  Par :: [Animation f a] -> ([a] -> Animation f b) -> Animation f b

-- cheating code to look inside the animation data type
-- don't try this at home

instance (forall a. Show (f a)) => Show (Animation f a) where
  show (Bind fa k) = "Bind (" ++ show fa ++ ") (" ++ (show (k undefined)) ++ ")"
  show (Par fs k) = let
    undefs = undefined : undefs
    in "Par (" ++ show fs ++ ") (" ++ (show (k undefs)) ++ ")"
  show (Return a) = "Return"

-- create the examples upThenRight / diagonal

upThenRight1 :: Animation (Operation XY) ()
upThenRight1 = error "not implemented"

diagonal1 :: Animation (Operation XY) ()
diagonal1 = error "not implemented"

-- monad instance

-- import Control.Monad (liftM, ap)

instance Functor (Animation f) where
  fmap = liftM

instance Applicative (Animation f) where
  pure = return
  (<*>) = ap

instance Monad (Animation f) where
  return = Return
  (Bind fa k) >>= k2 = Bind fa (\x -> k x >>= k2)
  (Par fs k) >>= k2 = Par fs (\l -> k l >>= k2)
  (Return a) >>= k2 = k2 a

-- define helper functions

seq :: [Animation (Operation obj) ()] -> Animation (Operation obj) ()
seq = error "not implemented"

par :: [Animation (Operation obj) ()] -> Animation (Operation obj) ()
par = error "not implemented"

basic :: Duration -> Traversal' obj Float -> To -> Animation (Operation obj) ()
basic duration traversal to = error "not implemented"

get :: Lens' obj x -> Animation (Operation obj) x
get lens = Bind (Get lens) (\x -> Return x)

set :: Traversal' obj x -> x -> Animation (Operation obj) ()
set traversal x = Bind (Set traversal x) (\_ -> Return ())

create :: (obj -> (obj, Int)) -> Animation (Operation obj) Int
create f = Bind (Create f) (\i -> Return i)

delete :: (Int -> obj -> obj) -> Int -> Animation (Operation obj) ()
delete f i = Bind (Delete f i) (\_ -> Return ())

delay :: Duration -> Animation (Operation obj) ()
delay duration = Bind (Delay duration) (\_ -> Return ())

-- redo examples

u2 :: Animation (Operation XY) ()
u2 = error "not implemented"

r2 :: Animation (Operation XY) ()
r2 = error "not implemented"

upThenRight2 :: Animation (Operation XY) ()
upThenRight2 = error "not implemented"

diagonal2 :: Animation (Operation XY) ()
diagonal2 = error "not implemented"

------------------------------
-- define running operations
------------------------------

data OperationResult obj a
  = Remainder (Operation obj a)
  | Finished a

-- the execOp function applies an operation to the given world, given a time delta
-- it returns the new obj, and the remainder of the operation (if not finished) or the resulting value of the operation (if finished)

execOp :: obj -> Float -> Operation obj a -> (obj, OperationResult obj a)
execOp = error "not implemented"

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

-- examples execOp

------------------------------
-- define running animations
------------------------------

-- the execAnimation function applies an animation to the given world, given a time delta
-- it returns the new obj, and the remainder of the animation

execAnimation ::
  obj ->
  Float ->
  Animation (Operation obj) a ->
  (obj, Animation (Operation obj) a)
execAnimation = error "not implemented"

execOps :: obj -> Float -> [Animation (Operation obj) a] -> (obj, [Animation (Operation obj) a])
execOps obj t [] = (obj, [])
execOps obj t (op:r) = let
  -- exec first operation
  (obj', op') = execAnimation obj t op
  -- exec the rest of the operations
  (obj'', ops) = execOps obj' t r
  in (obj'', op' : ops)

data ReturnValues a
  = NotFinishedYet
  | ResultsAvailable [a]

returnValues :: [Animation f a] -> ReturnValues a
returnValues [] = ResultsAvailable []
returnValues ((Return a):r) = case returnValues r of
  NotFinishedYet -> NotFinishedYet
  ResultsAvailable l -> ResultsAvailable (a : l)
returnValues _ = NotFinishedYet

-- examples execAnimation



-- helper function

cleanAnims :: [Animation (Operation obj) a] -> [Animation (Operation obj) a]
cleanAnims l = let
  f (Return _) = False
  f _ = True
  in filter f l

