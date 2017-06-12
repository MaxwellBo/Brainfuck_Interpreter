{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Environment
import Data.Maybe
import Data.List
import Data.Char
import Control.Monad.RWS
import qualified Data.Vector as V
import qualified Data.Bimap as B
import Data.Bimap ((!), (!>))
import Control.Lens
import Debug.Trace

type Program = String
type Interpreter = RWST Program () InterpreterState IO ()

data InterpreterState 
  = InterpreterState { _dataPointer        :: Int
                     , _instructionPointer :: Int
                     , _cells              :: V.Vector Int
                     }

makeClassy ''InterpreterState

type BracketTable = B.Bimap Int Int

makeBracketTable :: Program -> BracketTable
makeBracketTable program = B.fromList $ go 0 program []
  where
    go :: Int -> Program -> [Int] -> [(Int, Int)]
    go _ []       _         =          [] -- There's no program left to parse
    go i ('[':xs) stack     =          go (i + 1) xs (i:stack) -- We saw a [, put its position on the stack
    go i (']':xs) (s:tack)  = (s, i) : go (i + 1) xs tack -- Close the brace pair
    go i (']':xs) []        = error "Bracket Mismatch"
    go i (_:xs)   stack     =          go (i + 1) xs stack

interpreter :: Interpreter 
interpreter = do
  program <- ask 
  ip <- use instructionPointer
  
  -- liftIO.putStrLn $ program
  -- liftIO.putStrLn $ (replicate ip ' ') ++ "^"
  
  when (ip < length program) $ do
    dp <- use dataPointer
    bracketTable <- asks makeBracketTable
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
          instructionPointer .= bracketTable ! ip
      ']' -> 
        when (target /= 0) $ do
          instructionPointer .= bracketTable !> ip

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
  let filename = fromMaybe "Hello_World.bf" . listToMaybe $ args 
  program <- filter (`elem` "><.,+-[]") <$> readFile filename
  runInterpreter program
  return ()
