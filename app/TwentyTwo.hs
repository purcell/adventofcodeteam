{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TwentyTwo where

import           Control.Applicative      (empty)
import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Data.Function            (on)
import           Data.List                (intersectBy, partition)
import           Prelude                  hiding (round)

data Spell = MagicMissile | Drain | Shield | Poison | Recharge
  deriving Show

allSpells :: [Spell]
allSpells = [MagicMissile, Drain, Shield, Poison, Recharge]

spellCost :: Spell -> Int
spellCost MagicMissile = 53
spellCost Drain        = 73
spellCost Shield       = 113
spellCost Poison       = 173
spellCost Recharge     = 229

data Power = Heal | Damage | Armor | Mana
           deriving (Eq, Show)

spellPowers :: Spell -> [(Power, Int)]
spellPowers MagicMissile = [(Damage, 4)]
spellPowers Drain        = [(Damage, 2), (Heal, 2)]
spellPowers _            = []

data Effect = Effect { effectTimer :: Int, effectPower :: Power, effectQuantity :: Int }
  deriving Show

spellEffects :: Spell -> [Effect]
spellEffects Shield   = [Effect 6 Armor 7]
spellEffects Poison   = [Effect 6 Damage 3]
spellEffects Recharge = [Effect 5 Mana 101]
spellEffects _        = []


data GameState = GameState { gsPlayerHitPoints :: Int
                           , gsPlayerMana      :: Int
                           , gsPlayerArmor     :: Int
                           , gsBossHitPoints   :: Int
                           , gsBossDamage      :: Int
                           , gsManaSpent       :: Int
                           , gsSpellsCast      :: [Spell]
                           , gsActiveEffects   :: [Effect] }
                 deriving Show


data Result = Lost | Won
  deriving (Show, Eq)

newtype Game a = Game { unGame :: ExceptT Result (State GameState) a }
  deriving (Functor, Applicative, Monad, MonadState GameState, MonadError Result)

runGame :: Game a -> GameState -> (Either Result a, GameState)
runGame g gs = runState (runExceptT $ unGame g) gs

turn :: Game a -> Game ()
turn step = do
  effects <- gets gsActiveEffects
  modify (\gs -> gs { gsActiveEffects = ageEffects effects })
  mapM_ applyEffect effects
  step *> checkResult
  mapM_ endEffect effects

ageEffects :: [Effect] -> [Effect]
ageEffects = filter ((> 0) . effectTimer) . map (\e -> e { effectTimer = effectTimer e - 1 })

round :: Spell -> Game ()
round spell = do
  turn (attack spell)
  turn defend

attack :: Spell -> Game ()
attack s = do
  allowed <- gets (spellAllowed s)
  if allowed then do
    modify (\gs -> gs { gsPlayerMana    = gsPlayerMana gs - spellCost s
                      , gsActiveEffects = gsActiveEffects gs ++ spellEffects s
                      , gsSpellsCast    = gsSpellsCast gs ++ [s]
                      , gsManaSpent     = gsManaSpent gs + spellCost s })
    mapM_ (uncurry applyPower) (spellPowers s)
  else throwError Lost

spellAllowed :: Spell -> GameState -> Bool
spellAllowed s gs = affordable && notActive
  where
    affordable = spellCost s <= gsPlayerMana gs
    notActive = null $ intersectBy ((==) `on` effectPower) (gsActiveEffects gs) (spellEffects s)

checkResult :: Game ()
checkResult = do
  res <- gets result
  case res of
    Just r -> throwError r
    _      -> pure ()
  where
    result gs | gsPlayerHitPoints gs <= 0 || gsPlayerMana gs < 0 = Just Lost
    result gs | gsBossHitPoints gs <= 0                          = Just Won
    result _ = Nothing

defend :: Game ()
defend = modify (\gs -> gs { gsPlayerHitPoints = gsPlayerHitPoints gs - damage gs})
  where damage gs = maximum [1, gsBossDamage gs - gsPlayerArmor gs]

applyPower :: Power -> Int -> Game ()
applyPower p n = checkResult *>
  case p of
   Armor  -> modify (\gs -> gs { gsPlayerArmor     = gsPlayerArmor gs + n })
   Damage -> modify (\gs -> gs { gsBossHitPoints   = gsBossHitPoints gs - n })
   Heal   -> modify (\gs -> gs { gsPlayerHitPoints = gsPlayerHitPoints gs + n })
   Mana   -> modify (\gs -> gs { gsPlayerMana      = gsPlayerMana gs + n })

applyEffect :: Effect -> Game ()
applyEffect (Effect _ power n) = applyPower power n

endEffect :: Effect -> Game ()
endEffect (Effect _ Armor n) = modify (\gs -> gs { gsPlayerArmor = gsPlayerArmor gs - n })
endEffect _ = pure ()

nextStates :: GameState -> [(Bool, GameState)]
nextStates gs = do
  spell <- allSpells
  case runGame (round spell) gs of
    (Left Lost, _  ) -> empty
    (Left Won,  gs') -> pure (True,  gs')
    (_,         gs') -> pure (False, gs')


cheapestWin :: GameState -> Maybe GameState
cheapestWin gs = go [(False, gs)] Nothing
  where
    go :: [(Bool, GameState)] -> Maybe GameState -> Maybe GameState
    go [] best = best
    go ((_,     g):gss) (Just best)
      | gsManaSpent g >= gsManaSpent best = go gss (Just best)
    go ((True,  g):gss) _                 = go gss (Just g)
    go ((False, g):gss) best              = go (losses ++ gss) (go wins best)
      where (wins, losses) = partition fst (nextStates g)


twentyTwo = cheapestWin (GameState 50 500 0 58 9 0 [] [])


sample2 = runGame rounds (GameState 10 250 0 14 8 0 [] [])
  where
    rounds = do
      round Poison
      round Shield
      round Drain
      round Poison
      round MagicMissile
