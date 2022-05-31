module Addition where

import Test.Hspec
import Test.QuickCheck


data NumVal = Pos | Neg deriving (Show, Eq, Ord)

recMult :: (Eq a, Num a, Ord a, Show a) => a -> a -> a
recMult _ 0 = 0
recMult x y 
    | xSign == ySign = abs x + recMult x (y `op` 1)
    | xSign == Neg = x + recMult x (y - 1)
    | otherwise = recMult y x
    where xSign = if x<0 then Neg else Pos
          ySign = if y<0 then Neg else Pos
          op = if ySign == Pos then (-) else (+)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise =
                go (n - d) d (count + 1)


main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1+1)>1 `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            2+2 `shouldBe` 4
        it "multiplications" $ do
            recMult 3 3 `shouldBe` 3*3
        it "x + 1 is always\
           \ greater than x" $ do
               property $ \x -> x + 1 > (x :: Int)
        it "my mult is equal to *" $ do
            property $ \x y -> recMult x y == (x :: Int) * (y :: Int)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

prop_equalMults :: Int -> Int -> Bool
prop_equalMults x y = recMult x y == x * y


runQc :: IO ()
runQc = quickCheck prop_additionGreater

runQcMults :: IO ()
runQcMults = quickCheck prop_equalMults


oneThroughThree :: Gen Int 
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genTuple :: (Arbitrary a, Arbitrary b)
         => Gen (a,b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a,b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c)
            => Gen (a,b,c)
genThreeple = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a,b,c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [a,b]

genMaybe :: (Arbitrary a) => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
    a <- arbitrary
    frequency [(1, return Nothing), (3, return (Just a))]