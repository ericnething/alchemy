module Types where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import Data.Monoid
import Data.List (intercalate, sort)

type Name = String

data Attribute = Health Int
               | Attack Int
               | Defense Int
               | Speed Int
               | Magic Int
               | Intelligence Int
               | Luck Int
               deriving (Eq, Ord, Show, Read)

data Status = Cursed
            | Poisoned
            | Stoic
            | Sleeping
            deriving (Eq, Show)

data Augmentation = Quick
                  | Rusty
                  | Poison
                  | Steel
                  | Flame
                  | Ice
                  | Hell
                  deriving (Eq, Ord, Show)

data ItemClass = Blade
               | Armor
               | Potion
               | Ingredient
               deriving (Eq, Ord, Show)

data Item = Item
            { itemAttributes    :: [Attribute]
            , itemStatusEffects :: [Status]
            , itemAugmentation  :: [Augmentation]
            , itemClass         :: ItemClass
            } deriving (Eq)

instance Show Item where
  show (Item attr status augment itemClass) =
    "Name: " <> convert augment <> " " <> show itemClass <> "\n" <>
    "Attributes: " <> show attr <> "\n" <>
    "Status Effects: " <> show status
    where convert = intercalate " " . fmap show . sort

instance Monoid Item where
  mempty = Item [] [] [] Ingredient
  mappend (Item xs s1 ma1 c1) (Item ys s2 ma2 c2)
    = Item (xs <> ys) (s1 <> s2) (ma1 <> ma2) (min c1 c2)
--  where reduce = foldl' (\acc  -> M.lookup k) M.empty
  
