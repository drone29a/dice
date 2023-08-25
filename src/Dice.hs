module Dice (
    Dice,
    mkDice,
    rollDice,
    Roll (..),
) where

import System.Random (randomRIO)

-- newtype Dice = Dice Int
--     deriving (Eq, Show)

-- mkDice :: Int -> Maybe Dice
-- mkDice numSides =
--     case numSides of
--         20 -> Just $ Dice 20
--         12 -> Just $ Dice 12
--         10 -> Just $ Dice 10
--         8 -> Just $ Dice 8
--         6 -> Just $ Dice 6
--         _ -> Nothing

data Dice
    = D20
    | D12
    | D10
    | D8
    | D6
    | D4
    deriving (Eq, Show)

mkDice :: Int -> Maybe Dice
mkDice numSides =
    case numSides of
        20 -> Just D20
        12 -> Just D12
        10 -> Just D10
        8 -> Just D8
        6 -> Just D6
        4 -> Just D4
        _ -> Nothing

numSides :: Dice -> Int
numSides D20 = 20
numSides D12 = 12
numSides D10 = 10
numSides D8 = 8
numSides D6 = 6
numSides D4 = 4

data Roll = Roll
    { dice :: Dice
    , result :: Int
    } deriving (Eq, Show)

rollDice :: Dice -> IO Roll
rollDice dice = do
    let maxRoll = numSides dice
    randVal <- randomRIO (1, maxRoll)
    return $
        Roll
            { dice = dice
            , result = randVal
            }