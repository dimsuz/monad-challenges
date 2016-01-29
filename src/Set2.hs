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
