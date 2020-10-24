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

-------------------------------------------------------------------------------
-- checkNum 
-- | Check to see if the value parsed in an actual VNum
-- | Return True if so, else False
-------------------------------------------------------------------------------

checkNum :: Value -> Bool
checkNum (VNum num) = True
checkNum _ = False

-------------------------------------------------------------------------------
-- checkStr
-- | Check to see if the value parsed in an actual VStr. 
-- | Return True if so, else False
-------------------------------------------------------------------------------
checkStr :: Value -> Bool
checkStr (VStr c) = True
checkStr _ = False
-------------------------------------------------------------------------------
-- computeSpreadsheet
-- | Create tne final output spreadsheet, initialize globalEnv and call helpers
-- | Parse columns and apply designated functions
-------------------------------------------------------------------------------
computeSpreadsheet :: Spreadsheet -> [Column]
computeSpreadsheet (Spreadsheet defs columns) =
  let new_map = Data.Map.empty
      defEnv = foldl(\x y -> Data.Map.insert (parseDefString(y)) (evalDeer (parseDefExpr(y)) x) x) new_map defs
      valueCol = filter (parseColumns) columns
      colEnv = buildDataEnvs valueCol Data.Map.empty
      globalEnv = combineEnv defEnv colEnv
      output = []
  in
    foldl (\x y -> x ++ [doColumn y globalEnv] )output columns 
    
    

-------------------------------------------------------------------------------
-- Helper Functions
-- | You may add, remove, or modify any helper functions here.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- getNum 
-- | Return the integer/float value of a VNum
-------------------------------------------------------------------------------
getNum :: Value -> Float
getNum(VNum num) = num

-------------------------------------------------------------------------------
-- getString 
-- | Return the string representation of a VStr
-------------------------------------------------------------------------------
getString :: Value -> String
getString(VStr str) = str

-------------------------------------------------------------------------------
-- getExpr 
-- | Return the Expression within a Closure
-------------------------------------------------------------------------------
getExpr :: Value -> Expr
getExpr(VClosure func expr env) = expr

-------------------------------------------------------------------------------
-- getEnv
-- | Return the enviroment within a Closure
-------------------------------------------------------------------------------
getEnv :: Value -> Env
getEnv(VClosure func expr env) = env

-------------------------------------------------------------------------------
-- parseParameters
-- | Parse the paramaters within a closure insert them within the enviroment
-------------------------------------------------------------------------------
parseParameters :: Value -> [Value] -> Env
parseParameters (VClosure params expr env ) args =
    let tuples = zip params args 
    in
      foldl (\x y -> Data.Map.insert (fst y) (snd y) x) env tuples

-------------------------------------------------------------------------------
-- getFunctionValue
-- | Given a closure with all parameters within the enviroment, compute the value 
-- | the function would return
-------------------------------------------------------------------------------
getFunctionValue :: Value -> Env -> Value
getFunctionValue(VClosure params expr env) new_env=
  evalDeer expr new_env 

-------------------------------------------------------------------------------
-- parseDefString
-- | Return the string representation of the Definition id
-------------------------------------------------------------------------------
parseDefString :: Definition -> String
parseDefString(Def str exp) = str

-------------------------------------------------------------------------------
-- parseDefExpr
-- | Return the Expression within a Definition call
-------------------------------------------------------------------------------
parseDefExpr :: Definition -> Expr
parseDefExpr(Def str exp) = exp


-------------------------------------------------------------------------------
-- isError
-- | Check to see if an Error is passed as a parameter
-------------------------------------------------------------------------------
isError :: Value -> Bool
isError (Error) = True


-------------------------------------------------------------------------------
-- parseColumns
-- | Determine whether each Column is a ValCol or a ComputedCol
-------------------------------------------------------------------------------
parseColumns :: Column -> Bool
parseColumns(ValCol id expr)  = True
parseColumns(ComputedCol id expr) = False

-------------------------------------------------------------------------------
-- combineEnv
-- | Combine the enviroments created by computeSpreadsheet input and dataBuildCol
-- | env, create a larger env
-------------------------------------------------------------------------------
combineEnv :: Env -> [Env] -> [Env]
combineEnv sheetEnv columnEnv =
  let big_sheetEnv = take (length columnEnv) (repeat sheetEnv)
      combined = zip big_sheetEnv columnEnv
  in
    foldl (\x y -> x ++ [Data.Map.union (fst y) (snd y)]) [] combined

-------------------------------------------------------------------------------
-- doColumn
-- | If a ValCol is recieved return it as is
-- | If a ComputedCol, parse to find Closure/Builtin and evaluate the values
-------------------------------------------------------------------------------
doColumn :: Column -> [Env] -> Column
doColumn(ValCol id expr) env = ValCol id expr
doColumn(ComputedCol id expr) env =
  let 
      lists = foldl (\x y -> x ++  [evalDeer expr y]) [] env
      in

        ValCol id lists

-- Not needed 

-- Return an environment with the appropriate identifier-to-value bindings.
-- getEnvironment:: Definition -> Env
-- getEnvironment def = Error




-- Return a list of environments, one corresponding to each row in the data.
-- Each environment consists of bindings from the value columns, along with
-- the environment.

buildDataEnvs :: [Column] -> Env -> [Env]
buildDataEnvs columns env = 
  let env_list = buildRowEnviroment (columns !! 0) 
      final_list = []
  in 
    if length columns > 1
      then
        buildColumnHelper  columns env_list 1
      else 
        env_list

-------------------------------------------------------------------------------
-- buildColumnHelper
-- | Go through the list of columns one by one and create a list of size n consisting
-- | of enviroments to represent all n rows
-------------------------------------------------------------------------------
buildColumnHelper :: [Column] -> [Env] -> Int -> [Env]
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

-------------------------------------------------------------------------------
-- buildRowEnviroment
-- | Only called on first column to create n sized list of env
-- | Each element in list is an enviroment corresponding to a specific row
-------------------------------------------------------------------------------
buildRowEnviroment :: Column -> [Env]
buildRowEnviroment(ValCol id expr)  =  
  let new = []
  in
    Prelude.foldl (\x y -> x ++ [Data.Map.insert id y Data.Map.empty]) new expr 

  
-------------------------------------------------------------------------------
-- buildRowHelper
-- | Majority of work is done here, recursively go through each value within a column
-- | and add to corresponding row of the enviroment we are attempting to build
-------------------------------------------------------------------------------
buildRowHelper :: Column -> [Env] -> Int -> [Env] 
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
