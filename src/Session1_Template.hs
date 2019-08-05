{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Session1_Template where

import Prelude hiding (seq)

import Control.Monad (liftM, ap)

import Lens.Micro

-- zoom spacemacs: SPC z x (shift)+
-- zoom shell: CTRL (shift)+

-------------------------------
-- define operation data type
-------------------------------

-- introduce extensions:
-- * GADTs: type parameter 'a' appears in the definition, but is narrowed to '()' in the 'Basic' constructor
-- * RankNTypes: Traversal' is actually 'forall f. (Applicative f) => (a -> f a) -> (s -> f s)'

-- lenses examples
-- _1 is a predefined lens provided by the library

{-

> import Lens.Micro
> (120, 180) ^. _1
120
> (120, 180) ^. _2
180
> (120, 180) & _1 %~ \x -> x + 50
(170,180)
> ((120, 130), 180) & _1 . _1 %~ \x -> x + 50
((170,130),180)
> [1,2,3,4,5] & traverse %~ \x -> x + 1
[2,3,4,5,6]

-}

newtype Duration = For Float
  deriving (Show, Eq)
newtype To = To Float
  deriving (Show, Eq)

data Ops obj a where
  Basic :: Duration -> Traversal' obj Float -> To -> Ops obj ()

instance Show (Ops obj a) where
  show (Basic duration lens target) = "Basic (" ++ show duration ++ ") (..) (" ++ show target ++ ")"

-- define u / r

type XY = (Float, Float)

u1 :: Ops XY ()
u1 = Basic (For 0.5) (_2) (To 50)

r1 :: Ops XY ()
r1 = Basic (For 0.5) (_1) (To 50)

-------------------------
-- define dsl data type
-------------------------

data Dsl f a where
  Bind :: f a -> (a -> Dsl f b) -> Dsl f b
  Return :: a -> Dsl f a
  Par :: [Dsl f a] -> ([a] -> Dsl f b) -> Dsl f b

-- create the examples upThenRight / diagonal

upThenRight1 :: Dsl (Ops XY) ()
upThenRight1 = Bind u1 (\_ -> Bind r1 (\_ -> Return ()))

diagonal1 :: Dsl (Ops XY) ()
diagonal1 = Par [Bind u1 (\_ -> Return ()), Bind r1 (\_ -> Return ())] (\_ -> Return ())

-- introduce helper functions

-- import Control.Monad (liftM, ap)

instance Functor (Dsl f) where
  fmap = liftM

instance Applicative (Dsl f) where
  pure = return
  (<*>) = ap

instance Monad (Dsl f) where
  return = Return
  (Bind fa k) >>= k2 = Bind fa (\x -> k x >>= k2)
  (Par fs k) >>= k2 = Par fs (\l -> k l >>= k2)
  (Return a) >>= k2 = k2 a

seq :: [Dsl (Ops obj) ()] -> Dsl (Ops obj) ()
seq [] = Return ()
seq (anim:r) = anim >>= (\_ -> seq r)

par :: [Dsl (Ops obj) ()] -> Dsl (Ops obj) ()
par l = Par l (\_ -> Return ())

basic :: Duration -> Traversal' obj Float -> To -> Dsl (Ops obj) ()
basic duration traversal to = Bind (Basic duration traversal to) (\_ -> Return ())

-- redo examples

u2 :: Dsl (Ops XY) ()
u2 = basic (For 0.5) (_2) (To 50)

r2 :: Dsl (Ops XY) ()
r2 = basic (For 0.5) (_1) (To 50)

upThenRight2 :: Dsl (Ops XY) ()
upThenRight2 = seq [u2, r2]

diagonal2 :: Dsl (Ops XY) ()
diagonal2 = par [u2, r2]

------------------------------
-- define running operations
------------------------------

-- the applyOp function applies an operation to the given world, given a time delta
-- it returns the new obj, and the remainder of the operation (if not finished) or the resulting value of the operation (if finished)

applyOp :: obj -> Float -> Ops obj a -> (obj, Either (Ops obj a) a)
applyOp obj t (Basic (For duration) traversal (To target)) = let
  -- update object
  newObj = obj & traversal %~ updateValue t duration target
  -- reduce duration
  newDuration = duration - t
  -- create operation remainder / operation result
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

-- examples applyOp

{-

> applyOp (50, 150) 0.25 u1
((50.0,100.0),Left Basic (For 0.25) (..) (To 50.0))
> applyOp (50, 100) 0.25 (Basic (For 0.25) (_2) (To 50.0))
((50,50.0),Right ())

-}

------------------------------
-- define running animations
------------------------------

-- the applyDsl function applies an animation to the given world, given a time delta
-- it returns the new obj, and the remainder of the animation

applyDsl :: obj -> Float -> Dsl (Ops obj) a -> (obj, Dsl (Ops obj) a)
applyDsl obj t (Bind fa k) = let
  (newObj, eResult) = applyOp obj t fa
  in case eResult of
    Right result -> (newObj, k result)
    Left newOp -> (newObj, Bind newOp k)
applyDsl obj t (Par fs k) = let
  (newObj, newOps) = applyOps obj t fs
  in case returnValues newOps of
    Right l -> (newObj, k l)
    Left () -> (newObj, Par newOps k)
applyDsl obj t (Return a) = (obj, Return a)

applyOps :: obj -> Float -> [Dsl (Ops obj) a] -> (obj, [Dsl (Ops obj) a])
applyOps obj t [] = (obj, [])
applyOps obj t (op:r) = let
  -- apply first operation
  (obj', op') = applyDsl obj t op
  -- apply the rest of the operations
  (obj'', ops) = applyOps obj' t r
  in (obj'', op' : ops)

returnValues :: [Dsl f a] -> Either () [a]
returnValues [] = Right []
returnValues ((Return a):r) = do
  l <- returnValues r
  return (a : l)
returnValues _ = Left ()

-- examples applyDsl

-- introduce extension:
-- * QuantifiedConstraints

-- temporary cheats

undefs :: [a]
undefs = undefined : undefs

instance (forall a. Show (f a)) => Show (Dsl f a) where
  show (Bind fa k) = "Bind (" ++ show fa ++ ") (" ++ (show (k undefined)) ++ ")"
  show (Par fs k) = "Par (" ++ show fs ++ ") (" ++ (show (k undefs)) ++ ")"
  show (Return a) = "Return"

{-

> dslExample1 = applyDsl (150, 150) 0.25 diagonal2
> dslExample1
((100.0,100.0),Par ([Bind (Basic (For 0.25) (..) (To 50.0)) (Return),Bind (Basic (For 0.25) (..) (To 50.0)) (Return)]) (Return))
> applyDsl (fst dslExample1) 0.25 (snd dslExample1)
((50.0,50.0),Return)
> applyDsl (fst dslExample1) 0.25 (snd dslExample1)
((50.0,50.0),Return)

-}

