{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Session1_Template where

import Prelude hiding (seq)

import Control.Monad (liftM, ap)

import Lens.Micro

-- spacemacs:
--   zoom: SPC z x (shift)+
--   font size: SPC f e d
--     then change dotspacemacs-default-font :size
--     13 --> 24 and restart
-- shell:
--   zoom: CTRL-(shift)+

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
u1 = Basic (For 0.5) (_2) (To 50)

r1 :: Operation XY ()
r1 = Basic (For 0.5) (_1) (To 50)

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
upThenRight1 = Bind u1 (\_ -> Bind r1 (\_ -> Return ()))

diagonal1 :: Animation (Operation XY) ()
diagonal1 = Par [Bind u1 (\_ -> Return ()), Bind r1 (\_ -> Return ())] (\_ -> Return ())

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
seq [] = Return ()
seq (anim:r) = anim >>= (\_ -> seq r)

par :: [Animation (Operation obj) ()] -> Animation (Operation obj) ()
par l = Par l (\_ -> Return ())

basic :: Duration -> Traversal' obj Float -> To -> Animation (Operation obj) ()
basic duration traversal to = Bind (Basic duration traversal to) (\_ -> Return ())

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
u2 = basic (For 0.5) (_2) (To 50)

r2 :: Animation (Operation XY) ()
r2 = basic (For 0.5) (_1) (To 50)

upThenRight2 :: Animation (Operation XY) ()
upThenRight2 = seq [u2, r2]

diagonal2 :: Animation (Operation XY) ()
diagonal2 = par [u2, r2]

------------------------------
-- define running operations
------------------------------

-- the execOp function applies an operation to the given world, given a time delta
-- it returns the new obj, and the remainder of the operation (if not finished) or the resulting value of the operation (if finished)

execOp :: obj -> Float -> Operation obj a -> (obj, Either (Operation obj a) a)
execOp obj t (Basic (For duration) traversal (To target)) = let
  -- update object
  newObj = obj & traversal %~ updateValue t duration target
  -- reduce duration
  newDuration = duration - t
  -- create operation remainder / operation result
  result = if newDuration > 0
    then Left (Basic (For newDuration) traversal (To target))
    else Right ()
  in (newObj, result)
execOp obj t (Get lens) = let
  value = obj ^. lens
  in (obj, Right value)
execOp obj t (Set traversal value) = let
  newObj = obj & traversal .~ value
  in (newObj, Right ())
execOp obj t (Create create) = let
  (newObj, newIndex) = create obj
  in (newObj, Right newIndex)
execOp obj t (Delete delete index) = let
  newObj = delete index obj
  in (newObj, Right ())
execOp obj t (Delay (For duration)) = let
  newDuration = duration - t
  result = if newDuration > 0
    then Left (Delay (For newDuration))
    else Right ()
  in (obj, result)

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

{-

> execOp (50, 150) 0.25 u1
((50.0,100.0),Left Basic (For 0.25) (..) (To 50.0))
> execOp (50, 100) 0.25 (Basic (For 0.25) (_2) (To 50.0))
((50,50.0),Right ())

-}

------------------------------
-- define running animations
------------------------------

-- the execAnimation function applies an animation to the given world, given a time delta
-- it returns the new obj, and the remainder of the animation

execAnimation :: obj -> Float -> Animation (Operation obj) a -> (obj, Animation (Operation obj) a)
execAnimation obj t (Bind fa k) = let
  (newObj, eResult) = execOp obj t fa
  in case eResult of
    Right result -> (newObj, k result)
    Left newOp -> (newObj, Bind newOp k)
execAnimation obj t (Par fs k) = let
  (newObj, newOperation) = execOps obj t fs
  in case returnValues newOperation of
    Right l -> (newObj, k l)
    Left () -> (newObj, Par newOperation k)
execAnimation obj t (Return a) = (obj, Return a)

execOps :: obj -> Float -> [Animation (Operation obj) a] -> (obj, [Animation (Operation obj) a])
execOps obj t [] = (obj, [])
execOps obj t (op:r) = let
  -- exec first operation
  (obj', op') = execAnimation obj t op
  -- exec the rest of the operations
  (obj'', ops) = execOps obj' t r
  in (obj'', op' : ops)

returnValues :: [Animation f a] -> Either () [a]
returnValues [] = Right []
returnValues ((Return a):r) = do
  l <- returnValues r
  return (a : l)
returnValues _ = Left ()

-- examples execAnimation


{-

> dslExample1 = execAnimation (150, 150) 0.25 diagonal2
> dslExample1
((100.0,100.0),Par ([Bind (Basic (For 0.25) (..) (To 50.0)) (Return),Bind (Basic (For 0.25) (..) (To 50.0)) (Return)]) (Return))
> execAnimation (fst dslExample1) 0.25 (snd dslExample1)
((50.0,50.0),Return)
> execAnimation (fst dslExample1) 0.25 (snd dslExample1)
((50.0,50.0),Return)

-}

cleanAnims :: [Animation (Operation obj) a] -> [Animation (Operation obj) a]
cleanAnims l = let
  f (Return _) = False
  f _ = True
  in filter f l

