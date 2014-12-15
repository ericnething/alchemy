module Alchemy where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import Data.Monoid
import Data.List.Split (splitOn)

import Types

main :: IO ()
main = do
  putStr "List the ingredients you would like to combine: "
  c <- getLine
  case cook . splitOn ", " $ c of
   Just s -> do
     putStrLn "1... 2... 3... Poof! You admire your latest creation."
     putStrLn . show $ s
   Nothing -> putStrLn "Nothing happened..."

cook :: [Name] -> Maybe Item
cook = mconcat . fmap (flip M.lookup items)

items :: Map Name Item
items = M.fromList $
        fmap (\(name, ItemInfo attr b c d) ->
               (name, Item (mconcat . map fromAttribute $ attr) b c d))
  [ (
      "Bat Wing",
      ItemInfo
      [ Speed 10 ]
      []
      [ Quick ]
      Ingredient
    ),
    (
      "Dagger",
      ItemInfo
      [ Attack 15 ]
      []
      []
      Blade
    ),
    (
      "Iron Ore",
      ItemInfo
      [ Attack 10 ]
      []
      [ Hardened ]
      Ingredient
    ),
    (
      "Leather",
      ItemInfo
      [ Defense 10 ]
      []
      [ Leather ]
      Armor
    ),
    (
      "Daedric Heart",
      ItemInfo
      [ Magic 20, Luck (-5) ]
      [ Cursed ]
      [ Daedric ]
      Ingredient
    ),
    (
      "Barbed Tail",
      ItemInfo
      []
      [ Poisoned ]
      [ Poison ]
      Ingredient
    ),
    (
      "Herb",
      ItemInfo
      [ Health 20 ]
      []
      [ Life ]
      Ingredient
    )
  ]
