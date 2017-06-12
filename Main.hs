{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Environment
import Data.Maybe
import Data.List
import Data.Char
import Control.Monad.RWS
import qualified Data.Vector as V
import Control.Lens

type Program = String
type Interpreter = RWST Program () InterpreterState IO ()

data InterpreterState 
  = InterpreterState { _dataPointer        :: Int
                     , _instructionPointer :: Int
                     , _cells              :: V.Vector Int
                     }

makeClassy ''InterpreterState

interpreter :: Interpreter 
interpreter = do
  program <- filter (`elem` "><.,+-[]") <$> ask
  ip <- use instructionPointer
  
  -- liftIO.putStrLn $ program
  -- liftIO.putStrLn $ (replicate ip ' ') ++ "^"
  
  when (ip < length program) $ do
    dp <- use dataPointer
    Just target <- gets (^? cells.ix dp)
    case program !! ip of 
      '>' -> dataPointer += 1
      '<' -> dataPointer -= 1
      '+' -> cells.ix dp += 1
      '-' -> cells.ix dp -= 1
      '.' -> liftIO.putChar . chr $ target
      ',' -> do
        val <- ord <$> (liftIO getChar)
        cells.ix dp .= val
      '[' ->
        when (target == 0) $ do
          instructionPointer .= (head . filter (>ip) . elemIndices ']' $ program)
      ']' -> 
        when (target /= 0) $ do
          instructionPointer .= (last . filter (<ip) . elemIndices '[' $ program)

    instructionPointer += 1
    interpreter

runInterpreter program = 
  let 
    initState = InterpreterState 0 0 (V.replicate 30000 0)
  in 
    runRWST interpreter program initState

main :: IO ()
main = do 
  args <- getArgs
  let filename = fromMaybe "Addition.bf" . listToMaybe $ args 
  program <- readFile filename
  runInterpreter program
  return ()
