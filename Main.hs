module Main where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import Data.Monoid
import Data.List.Split (splitOn)

import Types

main :: IO ()
main = do
  putStr "List the ingredients you would like to combine: "
  c <- getLine
  case cook $ splitOn ", " c of
   Just s -> do
     putStrLn "1... 2... 3... Poof! You admire your latest creation."
     putStrLn . show $ s
   Nothing -> putStrLn "Nothing happened..."

cook :: [Name] -> Maybe Item
cook = mconcat . fmap (flip M.lookup items)

items :: Map Name Item
items = M.fromList $
  [ (
       "Bat Wing",
       Item
       [ Speed 10 ]
       []
       [ Quick ]
       Ingredient
    ),
    (
       "Dagger",
       Item
       [ Attack 15 ]
       []
       []
       Blade
    ),
    (
       "Iron Ore",
       Item
       [ Attack 10 ]
       []
       []
       Ingredient
    ),
    (
       "Leather",
       Item
       [ Defense 10 ]
       []
       []
       Armor
    ),
    (
       "Daedric Heart",
       Item
       [ Magic 20, Luck (-5) ]
       [Cursed]
       [Hell]
       Ingredient
    ),
    (
       "Barbed Tail",
       Item
       []
       [Poisoned]
       [Poison]
       Ingredient
    )
  ]
