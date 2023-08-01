{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Lib (runState, calculate, State (Start)) where

import Control.Parallel (par, pseq)

data History
  = Some {boughtPrice :: !Int}
  | None {soldPrice :: Maybe Int}

data State
  = Start Int
  | Trading {history :: !History, balance :: !Int, trades :: !Int}
  | End {balance :: !Int}

data DecisionTree = Tip | Branch {state :: !State, act :: DecisionTree, wait :: DecisionTree}

runState :: State -> [Int] -> DecisionTree
runState (Start k) ps = let state = Trading {history = None {soldPrice = Nothing}, balance = 0, trades = k} in runState state ps
runState (state@End {..}) _ = Branch {state, act = Tip, wait = Tip}
runState (state@Trading {trades = 0, ..}) _ = Branch {state = End {balance}, act = Tip, wait = Tip}
runState (state@Trading {..}) [] = Branch {state = End {balance}, act = Tip, wait = Tip}
runState (state@Trading {..}) (p' : ps) =
  case history of
    Some {boughtPrice = p} ->
      if p' > p
        then
          let state' = Trading {history = None {soldPrice = Just p'}, balance = balance + p', trades = trades - 1}
           in Branch {state, act = runState state' ps, wait = runState state ps}
        else Branch {state, act = Tip, wait = runState state ps}
    None {soldPrice = Nothing} ->
      let state' = Trading {history = Some {boughtPrice = p'}, balance = balance - p', trades}
       in Branch {state, act = runState state' ps, wait = runState state ps}
    None {soldPrice = Just p} ->
      if p <= p'
        then Tip
        else
          let state' = Trading {history = Some {boughtPrice = p'}, balance = balance - p', trades}
           in Branch {state, act = runState state' ps, wait = runState state ps}

calculate :: DecisionTree -> Int
calculate Tip = 0
calculate Branch {..} = case state of
  End {..} -> balance
  _ ->
    let ~acted = calculate act
        ~waited = calculate wait
     in max acted (acted `par` waited)