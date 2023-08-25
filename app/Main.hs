module Main where

import Dice (Dice, mkDice, rollDice, Roll (result))
import Control.Monad (replicateM, forM_)

main :: IO ()
main = do
  putStrLn "Please enter the sidedness of the dice:"
  numSidesStr <- getLine
  putStrLn "Please enter the number of rolls:"
  numRollsStr <- getLine
  let numSides = (read numSidesStr :: Int)
      numRolls = (read numRollsStr :: Int)
      dice = mkDice numSides
  case dice of 
    Just d -> do 
      rolls <- replicateM numRolls (rollDice d)
      forM_ rolls print
      let total = sum $ fmap result rolls
      putStrLn $ "Total: " <> show total
    Nothing -> putStrLn "There aren't any matching dice!"
  return ()
  