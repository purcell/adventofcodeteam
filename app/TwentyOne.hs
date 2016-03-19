module TwentyOne where

import           Data.List  (find, maximumBy, sortBy, tails)
import           Data.Maybe (catMaybes, fromJust)
import           Data.Ord   (comparing)

------------------------------------------------------------------------------
-- Stuff you can have
------------------------------------------------------------------------------

data Item = Item { itemName   :: String
                 , itemCost   :: Int
                 , itemDamage :: Int
                 , itemArmor  :: Int }
            deriving (Eq, Show)

data Inventory = Inventory { weapon :: Item
                           , armor  :: Maybe Item
                           , rings  :: [Item] }
                 deriving Show

inventoryTotal :: (Item -> Int) -> Inventory -> Int
inventoryTotal f = sum . map f . allItems
  where
    allItems p = [weapon p] ++ catMaybes [armor p] ++ rings p

inventoryCost :: Inventory -> Int
inventoryCost = inventoryTotal itemCost

availableWeapons :: [Item]
availableWeapons = [ Item "Dagger"      8 4 0
                   , Item "Shortsword" 10 5 0
                   , Item "Warhammer"  25 6 0
                   , Item "Longsword"  40 7 0
                   , Item "Greataxe"   74 8 0
                   ]

availableArmor :: [Item]
availableArmor = [ Item "Leather"     13 0 1
                 , Item "Chainmail"   31 0 2
                 , Item "Splintmail"  53 0 3
                 , Item "Bandedmail"  75 0 4
                 , Item "Platemail"  102 0 5
                 ]

availableRings :: [Item]
availableRings = [ Item "Damage +1"   25 1 0
                 , Item "Damage +2"   50 2 0
                 , Item "Damage +3"  100 3 0
                 , Item "Defense +1"  20 0 1
                 , Item "Defense +2"  40 0 2
                 , Item "Defense +3"  80 0 3
                 ]

possibleInventories :: [Inventory]
possibleInventories = Inventory <$> availableWeapons <*> possibleArmor <*> possibleRingSelections
  where
    possibleArmor = Nothing : map Just availableArmor
    possibleRingSelections = [[]] ++ map (:[]) availableRings ++ pairs availableRings

pairs :: [a] -> [[a]]
pairs xs = concatMap (\(c, rest) -> map (\d -> [c, d]) rest) $ zip xs (takeWhile (not . null) . drop 1 $ tails xs) -- ugh


------------------------------------------------------------------------------
-- Players and fighting
------------------------------------------------------------------------------

data PlayerRole = You | Boss
                deriving (Eq, Show)

data Player = Player { role         :: PlayerRole
                     , hitPoints    :: Int
                     , playerDamage :: Int
                     , playerArmor  :: Int }
              deriving (Eq, Show)

damage :: Player -> Player -> Int
damage attacker defender = max 1 (playerDamage attacker - playerArmor defender)

attack :: Player -> Player -> Player
attack attacker defender =
  defender { hitPoints = max 0 (hitPoints defender - damage attacker defender) }

winner :: Player -> Player -> Player
winner a b = mostHitpoints . fromJust . find gameOver $ rounds
  where
    rounds = iterate attackAndSwap (a, b)
    attackAndSwap (a', b') = (attack a' b', a')
    gameOver (a', b') = hitPoints a' == 0 || hitPoints b' == 0
    mostHitpoints (a', b') = maximumBy (comparing hitPoints) [a', b']

------------------------------------------------------------------------------
-- The showdown
------------------------------------------------------------------------------

youWithInventory :: Inventory -> Player
youWithInventory i = Player You 100 (inventoryTotal itemDamage i) (inventoryTotal itemArmor i)

boss :: Player
boss = Player Boss 100 8 2

youWinWith :: Inventory -> Bool
youWinWith inv = You == role victor
  where
    you = youWithInventory inv
    victor = winner you boss

cheapestWin :: Maybe Inventory
cheapestWin = find youWinWith cheapestInventories

cheapestInventories :: [Inventory]
cheapestInventories = sortBy (comparing inventoryCost) possibleInventories

priciestLoss :: Maybe Inventory
priciestLoss = find (not . youWinWith) priciestInventories

priciestInventories :: [Inventory]
priciestInventories = sortBy (flip $ comparing inventoryCost) possibleInventories

twentyOne :: (Int, Int)
twentyOne = ( inventoryCost $ fromJust cheapestWin
            , inventoryCost $ fromJust priciestLoss
            )
