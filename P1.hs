{-|
Module: P1 
Description: Project 1: A Spreadsheet Application with DeerLang
Copyright: (c) University of Toronto Mississagua, 2020
               CSC324 Principles of Programming Languages, Fall 2020
-}
-- This lists what this module exports. Don't change this!
module P1
  (
    evalDeer,
    computeSpreadsheet
  )
where

-- You *may not* add imports from Data.Map 
import P1Types(Spreadsheet(..), Definition(..), Column(..),
               Expr(..), Value(..),
               Env, exampleSpreadsheet)
import Prelude hiding (lookup)
import qualified Data.Map (lookup, insert, empty)

-------------------------------------------------------------------------------
-- Main Functions: 
-- | These are the functions that we will be directly testing.
-- | Do not change the type signatures of these functions.
-------------------------------------------------------------------------------

evalDeer :: Expr -> Env -> Value
evalDeer (Id id) env = case (Data.Map.lookup id env) of
                           Just value -> value
                           Nothing    -> Error

evalDeer (Literal v) env = v

evalDeer (Builtin "+" params) env = VNum (getNum(evalDeer (params !! 0) env) + getNum(evalDeer (params !! 1) env))

-- evalDeer (Builtin "-" params) env = evalDeer params!!0 - evalDeer params!!1

-- evalDeer (Builtin "*" params) env = evalDeer params!!0 * evalDeer params!!1

-- evalDeer (Builtin "/" params) env = evalDeer params!!0 / evalDeer params!!1

-- evalDeer (Builtin ">" params) env = evalDeer params!!0 > evalDeer params!!1

-- evalDeer (Builtin "=" params) env = evalDeer params!!0 == evalDeer params!!1

-- evalDeer (Builtin ">=" params) env = evalDeer params!!0 >= evalDeer params!!1

-- evalDeer (Builtin "++" params) env = evalDeer params!!0 ++ evalDeer params!!1

-- evalDeer (Builtin "!" params) env = not evalDeer params!!0

-- evalDeer (Lambda var params) env = VClosure var params env

getNum (VNum num) = num

computeSpreadsheet :: Spreadsheet -> [Column]
computeSpreadsheet (Spreadsheet defs columns) = []
--  let defEnv    = ...build an environment with the definitions...
--      valueCols = ...just the value columns...
--      dataEnvs  = ...list of environment, one for each spreadsheet row...
--      ...
--  in ...


-------------------------------------------------------------------------------
-- Helper Functions
-- | You may add, remove, or modify any helper functions here.
-------------------------------------------------------------------------------

-- Return an environment with the appropriate identifier-to-value bindings.
getEnvironment:: Definition -> Env
getEnvironment def = undefined

-- Return a list of environments, one corresponding to each row in the data.
-- Each environment consists of bindings from the value columns, along with
-- the environment.

buildDataEnvs :: [Column] -> Env -> [Env]
buildDataEnvs columns env = undefined


-------------------------------------------------------------------------------
-- The example from the handout
-------------------------------------------------------------------------------

result = computeSpreadsheet exampleSpreadsheet
