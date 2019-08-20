{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Session1 where

import Prelude hiding (seq)

import Control.Monad (liftM, ap)

import Lens.Micro

