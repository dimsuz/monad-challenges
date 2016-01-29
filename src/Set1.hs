module Set1 where

import MCPrelude

type Gen a = Seed -> (a, Seed)

nextRandInt :: [Integer] -> Gen [Integer]
nextRandInt xs = generalA (xs ++ ) (\s -> let (x, ns) = rand s in ([x], ns))

nextRandChar :: [Char] -> Gen [Char]
nextRandChar xs = generalA (xs ++ ) (\s -> let (x, ns) = randLetter s in ([x], ns))

fiveRands :: [Integer]
fiveRands = let (xs1, s1) = nextRandInt [] $ (mkSeed 1)
                (xs2, s2) = nextRandInt xs1 $ s1
                (xs3, s3) = nextRandInt xs2 $ s2
                (xs4, s4) = nextRandInt xs3 $ s3
                (xs5, s5) = nextRandInt xs4 $ s4
            in xs5

randLetter :: Gen Char
randLetter s = (toLetter x, ns) where (x, ns) = rand s


randString3 :: String
randString3 = let (xs1, s1) = nextRandChar [] $ (mkSeed 1)
                  (xs2, s2) = nextRandChar xs1 $ s1
                  (xs3, s3) = nextRandChar xs2 $ s2
              in xs3

randEven :: Gen Integer
randEven = generalA (*2) rand

randOdd :: Gen Integer
randOdd = generalA (+1) randEven

randTen :: Gen Integer
randTen = generalA (*10) rand

generalA :: (a -> a) -> Gen a -> Gen a
generalA op genfn = \s -> let (x, ns) = genfn s in (op x, ns)

randPair :: Gen (Char, Integer)
randPair s = let (c, s1) = randLetter s
                 (x, s2) = rand s1
                 cx = (c,x)
             in (cx, s2)

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair g1 g2 = \s -> let (x1, s1) = g1 s
                              (x2, s2) = g2 s1
                          in ((x1,x2), s2)

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f g1 g2 = \s -> let (x1, s1) = g1 s
                             (x2, s2) = g2 s1
                          in (f x1 x2, s2)

generalPair2 :: Gen a -> Gen b -> Gen (a,b)
generalPair2 = generalB (,)

--generalB :: (Int -> [Int] -> [Int]) -> Gen Int -> Gen [Int] -> Gen [Int]

repRandom :: [Gen a] -> Gen [a]
repRandom [] = mkGen []
repRandom (g:gs) = generalB (\x xs -> x : xs) g (repRandom gs)

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo g f = \s -> let (x, ns) = g s
                       ng = f x
                   in ng ns

mkGen :: a -> Gen a
mkGen x = \s -> (x, s)
