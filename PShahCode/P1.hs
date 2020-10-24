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
import qualified Data.Map (lookup, insert, empty, union)

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

evalDeer (Builtin "+" params) env =  
  if checkNum (evalDeer (params !! 0) env) && checkNum (evalDeer (params !! 1) env) && length params == 2
    then
      VNum (getNum(evalDeer (params !! 0) env) + getNum(evalDeer (params !! 1) env))
    else
      Error

evalDeer (Builtin "-" params) env =  
  if checkNum (evalDeer (params !! 0) env) && checkNum (evalDeer (params !! 1) env) && length params == 2
    then
      VNum (getNum(evalDeer (params !! 0) env) - getNum(evalDeer (params !! 1) env))
    else
      Error

evalDeer (Builtin "*" params) env = 
  if checkNum (evalDeer (params !! 0) env) && checkNum (evalDeer (params !! 1) env) && length params == 2
    then
      VNum (getNum(evalDeer (params !! 0) env) * getNum(evalDeer (params !! 1) env))
    else 
      Error

evalDeer (Builtin "/" params) env =  
  if getNum(evalDeer (params !! 1) env) == 0 || not (length params == 2)
    then 
      Error
    else
      VNum (getNum(evalDeer (params !! 0) env) / getNum(evalDeer (params !! 1) env))

evalDeer (Builtin ">" params) env = 
  if checkNum (evalDeer (params !! 0) env) && checkNum (evalDeer (params !! 1) env) && length params == 2
    then
      VBool (getNum(evalDeer (params !! 0) env) > getNum(evalDeer (params !! 1) env))
    else
      Error

evalDeer (Builtin "=" params) env = 
  if checkNum (evalDeer (params !! 0) env) && checkNum (evalDeer (params !! 1) env) && length params == 2
    then
      VBool (getNum(evalDeer (params !! 0) env) == getNum(evalDeer (params !! 1) env))
    else
      Error

evalDeer (Builtin ">=" params) env = 
  if checkNum (evalDeer (params !! 0) env) && checkNum (evalDeer (params !! 1) env) && length params == 2
    then
      VBool (getNum(evalDeer (params !! 0) env) >= getNum(evalDeer (params !! 1) env))
    else
      Error

evalDeer (Builtin "++" params) env =  
  if checkStr (evalDeer (params !! 0) env) && checkStr (evalDeer (params !! 1) env) && length params == 2
    then
      VStr (getString(evalDeer (params !! 0) env) ++ getString(evalDeer (params !! 1) env))
    else
      Error

evalDeer (Lambda function expr) env = VClosure function expr env

evalDeer (Apply function expr) env = 
  let func = evalDeer function env
      evaluatedArgs = foldl (\x y -> x ++ [evalDeer y env]) [] expr
      new_env = parseParameters func evaluatedArgs
  in
      getFunctionValue func new_env

checkNum (VNum num) = True
checkNum _ = False

checkStr (VStr c) = True
checkStr _ = False

-- computeSpreadsheet :: Spreadsheet -> [Column]
<<<<<<< HEAD
computeSpreadsheet (Spreadsheet defs columns) =
  let new_map = Data.Map.empty
      defEnv = foldl(\x y -> Data.Map.insert (parseDefString(y)) (evalDeer (parseDefExpr(y)) x) x) new_map defs
      valueCol = filter (parseColumns) columns
      colEnv = buildDataEnvs valueCol Data.Map.empty
      globalEnv = combineEnv defEnv colEnv
      output = []
  in
    foldl (\x y -> x ++ [doColumn y globalEnv] )output columns 
=======
-- computeSpreadsheet (Spreadsheet defs columns) =
--   let new_map = Data.Map.empty
--       defEnv = foldl(\x y -> Data.Map.insert (parseDefString(y)) (evalDeer (parseDefExpr(y)) x) x) new_map defs
--       valueCol = filter (parseColumns) columns
--       colEnv = buildDataEnvs valueCol Data.Map.empty
--       globalEnv = combineEnv defEnv colEnv
--   in
--     foldl (\x y -> x ++ doColumn y defEnv colEnv ) output columns
--     output 
>>>>>>> 9e05b4e6670ea4da9f1a83a4f16108e605b4042b
    

-------------------------------------------------------------------------------
-- Helper Functions
-- | You may add, remove, or modify any helper functions here.
-------------------------------------------------------------------------------
getNum(VNum num) = num

getString(VStr str) = str

getExpr(VClosure func expr env) = expr

getEnv(VClosure func expr env) = env

parseParameters (VClosure params expr env ) args =
    let tuples = zip params args 
    in
      foldl (\x y -> Data.Map.insert (fst y) (snd y) x) env tuples

getFunctionValue(VClosure params expr env) new_env=
  evalDeer expr new_env 
  

parseDefString(Def str exp) = str

parseDefExpr(Def str exp) = exp

isError (Error) = True

parseColumns(ValCol id expr)  = True
parseColumns(ComputedCol id expr) = False

combineEnv sheetEnv columnEnv =
  let big_sheetEnv = take (length columnEnv) (repeat sheetEnv)
      combined = zip big_sheetEnv columnEnv
  in
    foldl (\x y -> x ++ [Data.Map.union (fst y) (snd y)]) [] combined

doColumn(ValCol id expr) env = ValCol id expr
doColumn(ComputedCol id expr) env =
  let 
      lists = foldl (\x y -> x ++  [evalDeer expr y]) [] env
      in

        ValCol id lists

-- getFunctionId(Apply func_id expr) = func_id

-- getParamId(Apply func_id expr) = expr


-- Return an environment with the appropriate identifier-to-value bindings.
-- getEnvironment:: Definition -> Env
-- getEnvironment def = Error

-- Return a list of environments, one corresponding to each row in the data.
-- Each environment consists of bindings from the value columns, along with
-- the environment.

-- buildDataEnvs :: [Column] -> Env -> 

buildDataEnvs columns env = 
  let env_list = buildRowEnviroment (columns !! 0) 
      final_list = []
  in 
    buildColumnHelper  columns env_list 1


buildColumnHelper columns starting_list count  =
  if count == length columns -1
    then
      let
          new_concat_list = buildRowHelper (columns !! count) starting_list 0
      in
        new_concat_list
    else 
      let new_concat_list = buildRowHelper (columns !! count) starting_list 0 
      in
          buildColumnHelper  columns new_concat_list (count + 1)
  
buildRowEnviroment(ValCol id expr)  =  
  let new = []
  in
    Prelude.foldl (\x y -> x ++ [Data.Map.insert id y Data.Map.empty]) new expr 


buildRowHelper (ValCol id expr) env_list count =

  if (length expr == 1)
    then 
      [Data.Map.insert id (expr !! 0) (env_list !! (count))]
    else 
      [Data.Map.insert id (expr !! 0) (env_list !! count)] ++ buildRowHelper(ValCol id (tail expr)) env_list (count + 1)
     

-------------------------------------------------------------------------------
-- The example from the handout
-------------------------------------------------------------------------------

result = computeSpreadsheet exampleSpreadsheet
