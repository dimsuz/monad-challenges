{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Set2 where

import MCPrelude

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
  show (Just a) = "Just " ++ show a
  show Nothing = "Nothing"

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay x = headMay . map snd . filter (\v -> fst v == x)

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay x 0 = Nothing
divMay x y = Just (x / y)

-- needed because RebindableSyntax hides (if/then/else)
ifElse True x y = x
ifElse False x y = y

minMaxMay :: Ord a => (a -> a -> Bool) -> [a] -> Maybe a
minMaxMay p [] = Nothing
minMaxMay p (x:xs) = case (maximumMay xs) of
  (Just v) -> ifElse (p x v) (Just v) (Just x)
  _ -> Just x

maximumMay :: Ord a => [a] -> Maybe a
maximumMay = minMaxMay (<)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay = minMaxMay (>)

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd key = case lookupMay key gd of
  Nothing -> Nothing
  Just xs -> case tailMay xs of
    Nothing -> Nothing
    Just txs -> case maximumMay txs of
      Nothing -> Nothing
      Just x -> case headMay xs of
        Nothing -> Nothing
        Just h -> divMay (fromIntegral x) (fromIntegral h)

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f Nothing = Nothing
chain f (Just x) = f x

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link Nothing _ = Nothing
link (Just x) f = f x

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gd key = let xs = lookupMay key gd
                         txs = link xs tailMay
                         x = link txs maximumMay
                         h = link xs headMay
                         dx = link x (\x -> Just (fromIntegral x))
                         dh = link h (\x -> Just (fromIntegral x))
                         div1 = link dx (\x -> Just (divMay x))
                         div2 = link dh (\h -> link div1 (\f -> f h))
                     in div2

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries xs n1 n2 = let sm1 = lookupMay n1 xs
                           sm2 = lookupMay n2 xs
                           in link sm1 (\s1 ->
                                          link sm2 (\s2 -> Just (s1 + s2)))
mkMaybe :: a -> Maybe a
mkMaybe = Just

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f m1 m2 = link m1 (\v1 -> link m2 (\v2 -> mkMaybe (f v1 v2)))

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 xs n1 n2 = yLink (+) (lookupMay n1 xs) (lookupMay n2 xs)

tailProd :: Num a => [a] -> Maybe a
tailProd xs = transMaybe product (tailMay xs)

tailSum :: Num a => [a] -> Maybe a
tailSum xs = transMaybe sum (tailMay xs)

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f Nothing = Nothing
transMaybe f (Just a) = Just (f a)

tailMax :: Ord a => [a] -> Maybe a
tailMax xs = combine $ transMaybe maximumMay (tailMay xs)

tailMin :: Ord a => [a] -> Maybe a
tailMin xs = combine $ transMaybe maximumMay (tailMay xs)

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing = Nothing
combine (Just ma) = ma
