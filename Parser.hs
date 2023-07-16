module Parser where

import AST
import Data.Functor.Identity(Identity)
import Text.Parsec

smetanaParser :: Parsec String u Smetana
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