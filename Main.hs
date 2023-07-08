{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Main where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.ByteString (Parser)
import Data.Functor.Identity (Identity)
import Data.Foldable
import Data.List
import Data.Validation (Validation (Failure), fromEither, toEither)
import Data.Maybe
import Data.Either (fromLeft)

data Smetana = Smetana Step Smetana | End deriving(Show)
data Step = Step Int32 Command deriving(Show)
data Command = Swap Int32 Int32 | GoTo Int32 deriving(Show)

smetanaParser :: ParsecT String u Identity Smetana
smetanaParser = do
    value <- stepParser
    next <- (char '\n' >> smetanaParser) <|> (eof >> return End)
    return (Smetana value next)

stepParser :: ParsecT String u Identity Step
stepParser = do
    string "Step"
    spaces
    num <- read <$> many1 digit
    char '.'
    spaces
    cmd <- commandParser
    char '.'
    return $ Step num cmd

commandParser :: ParsecT String u Identity Command
commandParser = swapParser <|> gotoParser

gotoParser :: ParsecT String u Identity Command
gotoParser = do
    string "Go"
    spaces
    string "to"
    spaces
    string "step"
    spaces
    num <- read <$> many1 digit
    return (GoTo num)

swapParser :: ParsecT String u Identity Command
swapParser = do
    string "Swap"
    spaces
    string "step"
    spaces
    num1 <- read <$> many1 digit
    spaces
    string "with"
    spaces
    string "step"
    spaces
    num2 <- read <$> many1 digit
    return (Swap num1 num2)

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

isFirstStepIs1 :: Smetana -> Validation String Smetana
isFirstStepIs1 smet = if 1 == head (smetanaStepsToInts smet) then pure smet else fromEither $ Left "program should start from step 1\n"

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
allChecks smet = mconcat $ map ($ smet) [isFirstStepIs1, checkOrder, stepsOutOfBounds]



main :: IO ()
main = do
    case parse smetanaParser "" test of
        Left err -> print err
        Right xs -> putStr $ fromLeft "WTF all OK" $ toEither $ allChecks xs

test = "Step 141. Go to step 3.\nStep 4. Swap step 4 with step 22.\nStep 3. Go to step 5."
