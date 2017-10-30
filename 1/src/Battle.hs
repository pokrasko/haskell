module Battle where

import           Data.List (intercalate)


class Fighter a where
    tun :: a -> String
    health :: a -> Int
    hurtFighter :: Int -> a -> a
    attack :: a -> Int
    defense :: a -> Int

showFighter :: Fighter a => a -> String
showFighter x = tun x ++ ": health - " ++ show (health x) ++
    ", attack - " ++ show (attack x) ++ ", defense - " ++ show (defense x)

data Knight = Knight Int Int Int

instance Fighter Knight where
    tun Knight{} = "Knight"
    health (Knight h _ _) = h
    hurtFighter h (Knight _ a d) = Knight h a d
    attack (Knight _ a _) = a
    defense (Knight _ _ d) = d

data Monster = Monster Int Int Int

instance Fighter Monster where
    tun Monster{} = "Monster"
    health (Monster h _ _) = h
    hurtFighter h (Monster _ a d) = Monster h a d
    attack (Monster _ a _) = a
    defense (Monster _ _ d) = d

data FightResult = Draw
                 | Win Int Int


fight :: (Fighter a, Fighter b) => a -> b -> String
fight x y = intercalate "\n" [showFighter x, showFighter y, formatFightResult $ fight' x y]
  where
    formatFightResult :: FightResult -> String
    formatFightResult Draw = "Draw: nobody can hurt each other"
    formatFightResult (Win n h) = "Fighter " ++ show n ++ " has won: his health is " ++ show h
    fight' :: (Fighter a, Fighter b) => a -> b -> FightResult
    fight' x' y' = case fightRound x' y' of
        Nothing -> Draw
        Just (0, h) -> Win 1 h
        Just (h, 0) -> Win 2 h
        Just (xh, yh) -> fight' (hurtFighter xh x') (hurtFighter yh y')
    fightRound :: (Fighter a, Fighter b) => a -> b -> Maybe (Int, Int)
    fightRound x' y'
        | attack x' < defense y' && attack y' < defense x'
            = Nothing
        | health y' < attack x' - defense y'
            = Just (health x', 0)
        | health x' < attack y' - defense x'
            = Just (0, health y')
        | otherwise
            = Just (health x' + defense x' - attack y', health y' + defense y' - attack x')
