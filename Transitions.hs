{-# LANGUAGE FlexibleInstances #-}

module Transitions where

import Data.Matrix

import Quipper
import Quipper.Circuit
import Quipper.Monad

import EntangleMonad
import QTuple

data StateName = StateName {
    snId :: Integer,  -- ^ state id
    snBs :: [Bool]    -- ^ boolean values in the state
} deriving Eq

instance Show StateName where
    show (StateName i bs) = show i ++ if null bs then "" else "_" ++ map (\b -> if b then 'T' else 'F') (reverse bs)

data Transitions v = Transitions {
    trFromState :: StateName,
    trDestinations :: [Transition v]
}

data Transition v = Transition {
    trMatrix :: Maybe (Matrix v),
    trToState :: StateName
}

--circToTree :: QTuple a => (a -> Circ b) -> CircTree b
circToTree :: Show b => QTuple a => (a -> Circ b) -> CircTree b
circToTree mcirc = tree where
    arg = tupleFromList $ map qubit_of_wire [1..]
    circ = extract_general arity_empty (mcirc arg)
    argsLength = tupleSize arg
    tree = buildTree circ argsLength
