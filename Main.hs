{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Environment
import Data.Maybe
import Data.List
import Data.Char
import Control.Monad.RWS
import qualified Data.Vector as V
import Control.Lens.Setter
import Control.Lens.Getter
import Control.Lens.TH
import Control.Lens.At

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
  
  if ip < (length program) then
    do
      dp <- use dataPointer
      cls <- use cells
      case program !! ip of 
        '>' -> dataPointer += 1
        '<' -> dataPointer -= 1
        '+' -> cells.ix dp += 1
        '-' -> cells.ix dp -= 1
        '.' -> liftIO.putChar . chr $ cls V.! dp
        ',' -> do
          val <- ord <$> (liftIO getChar)
          cells.ix dp .= val
        '[' -> do
          if cls V.! dp == 0 then
            instructionPointer .= (head . filter (>ip) . elemIndices ']' $ program)
          else
            return ()
        ']' -> do
          if cls V.! dp /= 0 then
             instructionPointer .= (last . filter (<ip) . elemIndices '[' $ program)
          else
            return ()
        _ -> do
          return ()

      instructionPointer += 1
      interpreter
  else
    return ()

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
