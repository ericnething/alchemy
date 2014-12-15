module Types where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import Data.Monoid
import Data.List (intercalate, sort)
import Data.Maybe (mapMaybe)

type Name = String

data Attribute
  = Health       Int
  | Attack       Int
  | Defense      Int
  | Speed        Int
  | Magic        Int
  | Intelligence Int
  | Luck         Int
  deriving (Eq, Ord, Show, Read)

data Attributes
  = Attributes
    { attrHealth       :: Int
    , attrAttack       :: Int
    , attrDefense      :: Int
    , attrSpeed        :: Int
    , attrMagic        :: Int
    , attrIntelligence :: Int
    , attrLuck         :: Int
    } deriving (Eq)

instance Monoid Attributes where
  mempty = Attributes 0 0 0 0 0 0 0
  mappend
    (Attributes a0 a1 a2 a3 a4 a5 a6)
    (Attributes b0 b1 b2 b3 b4 b5 b6)
    = Attributes
      (a0 + b0)
      (a1 + b1)
      (a2 + b2)
      (a3 + b3)
      (a4 + b4)
      (a5 + b5)
      (a6 + b6)

fromAttribute :: Attribute -> Attributes
fromAttribute (Health       x) = mempty {attrHealth       = x}
fromAttribute (Attack       x) = mempty {attrAttack       = x}
fromAttribute (Defense      x) = mempty {attrDefense      = x}
fromAttribute (Speed        x) = mempty {attrSpeed        = x}
fromAttribute (Magic        x) = mempty {attrMagic        = x}
fromAttribute (Intelligence x) = mempty {attrIntelligence = x}
fromAttribute (Luck         x) = mempty {attrLuck         = x}

instance Show Attributes where
  show attr
    = intercalate ", " . mapMaybe nonzero $
      [ ("Health ",       attrHealth       attr)
      , ("Attack ",       attrAttack       attr)
      , ("Defense ",      attrDefense      attr)
      , ("Speed ",        attrSpeed        attr)
      , ("Magic ",        attrMagic        attr)
      , ("Intelligence ", attrIntelligence attr)
      , ("Luck ",         attrLuck         attr)
      ]
    where nonzero (name, val) =
            case val of
             0 -> Nothing
             _ -> Just (name <> show val)
            

data Status
  = Cursed
  | Poisoned
  | Stoic
  | Sleeping
  deriving (Eq, Show)

data Augmentation
  = Quick
  | Rusty
  | Poison
  | Steel
  | Leather
  | Flame
  | Ice
  | Daedric
  | Life
  deriving (Eq, Ord, Show)

data ItemClass
  = Blade
  | Armor
  | Potion
  | Ingredient
  deriving (Eq, Ord, Show)

data ItemInfo
  = ItemInfo
    { itemInfoAttributes    :: [Attribute]
    , itemInfoStatusEffects :: [Status]
    , itemInfoAugmentation  :: [Augmentation]
    , itemInfoClass         :: ItemClass
    } deriving (Eq)

data Item
  = Item
    { itemAttributes    :: Attributes
    , itemStatusEffects :: [Status]
    , itemAugmentation  :: [Augmentation]
    , itemClass         :: ItemClass
    } deriving (Eq)

instance Show Item where
  show (Item attr status augment itemClass) =
    "Name: " <> convert augment <> " " <> choose itemClass <> "\n" <>
    "Attributes: " <> show attr <> "\n" <>
    "Status Effects: " <> (intercalate ", " . map show) status
    where convert = intercalate " " . fmap show . sort
          choose x = case x of
            Ingredient -> show Potion
            _          -> show x

instance Monoid Item where
  mempty = Item mempty [] [] Ingredient
  mappend (Item xs s1 ma1 c1) (Item ys s2 ma2 c2)
    = Item (xs <> ys) (s1 <> s2) (ma1 <> ma2) (min c1 c2)
