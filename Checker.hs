module Checker where

import AST
import Data.Validation
import Data.List
import Data.Maybe (isJust)
import GHC.Num.BigNat (bigNatAdd)

{-fromEither :: Either a b -> Validation a b
fromEither (Left a) = Failure a
fromEither (Right a) = Success a

toEither :: Validation a b -> Either a b
toEither (Failure a) = Left a
toEither (Success a) = Right a-}

smetanaToList :: Smetana -> [Step]
smetanaToList (Smetana a b) = a : smetanaToList b
smetanaToList End = []

getStepNum :: Step -> Int
getStepNum (Step n _) = n

stepsToInts :: [Step] -> [Int]
stepsToInts = map getStepNum

smetanaStepsToInts :: Smetana -> [Int]
smetanaStepsToInts = stepsToInts . smetanaToList

isIntsOrdered :: [Int] -> [Int]
isIntsOrdered (x:y:xs) = if y == (x + 1) then isIntsOrdered (y:xs) else x:isIntsOrdered (y:xs)
isIntsOrdered (x:xs) = []
isIntsOrdered [] = []

isSmetanaOrdered :: Smetana -> [Int]
isSmetanaOrdered = isIntsOrdered . smetanaStepsToInts

checkOrder :: Smetana -> Validation String Smetana
checkOrder smet = case isSmetanaOrdered smet of
    [] -> pure smet
    ns -> fromEither $ Left $ intercalate "\n" (map (\n -> concat ["after step ", show n, " should be step ", show (n + 1)]) ns) ++ "\n"

isFirstStepIs0 :: Smetana -> Validation String Smetana
isFirstStepIs0 smet = if 0 == head (smetanaStepsToInts smet) then pure smet else fromEither $ Left "program should start from step 1\n"

stepsOutOfBounds :: Smetana -> Validation String Smetana
stepsOutOfBounds smet = if any (isJust . isOutBound) (smetanaToList smet) then mconcat $ map justToFail {-$ intersperse (Just "\n")-} $ filter isJust $ map isOutBound $ smetanaToList smet else pure smet where
    len = length (smetanaToList smet)
    justToFail :: Maybe String -> Validation String Smetana
    justToFail (Just a) = Failure a
    isOutBound :: Step -> Maybe String
    isOutBound (Step n (Swap a b))
        | (a > len || a < 1) && (b > len || b < 1) = Just $ concat ["in step ", show n, " non existing step ", show a, " swapping with non existing step ", show b, "\n"]
        | a > len || a < 1 = Just $ concat ["in step ", show n, " non existing step ", show a, " swapping with step ", show b, "\n"]
        | b > len || b < 1 = Just $ concat ["in step ", show n, " step ", show a, " swapping with non existing step ", show b, "\n"]
        | otherwise = Nothing
    isOutBound (Step n (GoTo a))
        | a > len || a < 1 = Just $ concat ["in step ", show n, " go to unexisting step ", show a, "\n"]
        | otherwise = Nothing

allChecks :: Smetana -> Validation String Smetana
allChecks smet = isFirstStepIs0 smet <* checkOrder smet <* stepsOutOfBounds smet