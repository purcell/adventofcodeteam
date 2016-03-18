module TwentyTwo where

type Player = (Stats, Effects, Effects)
type Stats = (Int, Int, Int, Int)
type Effect = (Stats -> Stats)
type Effects = [(Char, Int, Effect)]
type Boss = Player
type Spell = (Player, Boss) -> (Player, Boss)

costMana :: Player -> Int -> Player
costMana ((hp, mana, armour, damage), effects, bonuses) cost = ((hp, mana - cost, armour, damage), effects, bonuses)

healPlayer :: Player -> Int -> Player
healPlayer ((hp, mana, armour, damage), effects, bonuses) bonus = ((hp - bonus, mana, armour, damage), effects, bonuses)

damagePlayer :: Player -> Int -> Player
damagePlayer ((hp, mana, armour, damage), effects, bonuses) wound = ((hp - (max (wound - armour) 1), mana, armour, damage), effects, bonuses)

addPlayerEffect :: Player -> Char -> Effect -> Int -> Player
addPlayerEffect (stats, effects, bonuses) key effect duration = (stats, (effects ++ [(key, duration, effect)]), bonuses)

addPlayerBonus :: Player -> Char -> Effect -> Int -> Player
addPlayerBonus (stats, effects, bonuses) key effect duration = (stats, effects, (bonuses ++ [(key, duration, effect)]))

-- Magic Missile costs 53 mana. It instantly does 4 damage.
magicMissile :: (Player, Boss) -> (Player, Boss)
magicMissile (player, boss) = (player, (damagePlayer boss 4))

-- Drain costs 73 mana. It instantly does 2 damage and heals you for 2 hit points.
drain :: (Player, Boss) -> (Player, Boss)
drain (player, boss) = ((healPlayer player 2), (damagePlayer boss 2))

-- Shield costs 113 mana. It starts an effect that lasts for 6 turns. While it is active, your armor is increased by 7.
shield :: (Player, Boss) -> (Player, Boss)
shield (player, boss) = ((addPlayerBonus player 'S' shieldEffect 6), boss)

shieldEffect :: Stats -> Stats
shieldEffect (hp, mana, armour, damage) = (hp, mana, armour + 7, damage)

-- Poison costs 173 mana. It starts an effect that lasts for 6 turns. At the start of each turn while it is active, it deals the boss 3 damage.
poison :: (Player, Boss) -> (Player, Boss)
poison (player, boss) = (player, (addPlayerEffect boss 'P' poisonEffect 6))

poisonEffect :: Stats -> Stats
poisonEffect (hp, mana, armour, damage) = (hp - 3, mana, armour, damage)

-- Recharge costs 229 mana. It starts an effect that lasts for 5 turns. At the start of each turn while it is active, it gives you 101 new mana.

recharge :: (Player, Boss) -> (Player, Boss)
recharge (player, boss) = ((addPlayerEffect player 'R' rechargeEffect 5), boss)

rechargeEffect :: Stats -> Stats
rechargeEffect (hp, mana, armour, damage) = (hp, mana + 101, armour, damage)

spellBook :: [(Char, Spell, Int)]
spellBook = [('M', magicMissile, 53), ('D', drain, 73), ('S', shield, 113), ('P', poison, 173), ('R', recharge, 229)]

startTurn :: Player -> Player
startTurn (stats, effects, bonuses) = ((foldl applyEffect stats effects), (countdown effects), (countdown bonuses))

applyEffect :: Stats -> (Char, Int, Effect) -> Stats
applyEffect stats (_, _, effect) = effect stats

countdown :: Effects -> Effects
countdown [] = []
countdown ((_, 0, _):effects) = countdown effects
countdown ((key, turns, effect):effects) = [(key, turns - 1, effect)] ++ (countdown effects)

availableSpells :: Player -> Boss -> [(Char, Spell, Int)]
availableSpells player boss = filter (spellIsAvailable player boss) spellBook

spellIsAvailable :: Player -> Boss -> (Char, Spell, Int) -> Bool
spellIsAvailable ((_, mana, _, _), playerEffects, playerBonuses) (bossStats, bossEffects, bossBonuses) (key, _, cost) = cost <= mana && (noInstanceOf key (concat [playerEffects, playerBonuses, bossEffects, bossBonuses]))

noInstanceOf :: Char -> Effects -> Bool
noInstanceOf key effects = not (any (isInstanceOf key) effects)

isInstanceOf :: Char -> (Char, Int, Effect) -> Bool
isInstanceOf key (k, _, _) = key == k

newPlayer :: Player
newPlayer = ((50, 500, 0, 0), [], [])

newBoss :: Boss
newBoss = ((71, 0, 0, 10), [], [])

