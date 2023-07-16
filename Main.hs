{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Main where

import AST
import Parser
import Checker
import CodeGen
import Data.Either (fromLeft)
import Data.Validation
import Text.Parsec
import Data.Functor.Contravariant (defaultComparison)
import Foreign (toBool)
import System.Process

newtype Flags = Flags { out :: OutputFlags }
data OutputFlags = ToC | ToO

stringsToOutputFlag :: [String] -> Validation String [OutputFlags]
stringsToOutputFlag [] = Success []
stringsToOutputFlag ["toC"] = Success [ToC]
stringsToOutputFlag ["toO"] = Success [ToO]
stringsToOutputFlag (x:xs) | length (x:xs) > 1 = Failure "error: multiple output flags\n" *> stringsToOutputFlag xs
stringsToOutputFlag (x:xs) = Failure ("unknown flag " ++ x ++ "\n") *> stringsToOutputFlag xs

defaultFlags :: Flags
defaultFlags = Flags { out = ToC }

flagParser :: Parsec String u String
flagParser = do
    char '-'
    many anyChar

inputParse :: Parsec String u (FilePath, [String])
inputParse = do
    file <- many anyChar
    char '.'
    string "smetana"
    skipMany1 space
    flags <- sepEndBy flagParser (skipMany1 space)
    return (file, flags)

writeTo :: Flags -> FilePath -> String -> IO ()
writeTo flags path str = case out flags of
    ToC -> writeFile (path ++ ".c") str
    ToO -> writeFile (path ++ ".c") str >> callProcess "gcc" [concat ["-o ", path, ".c ", path, ".out"]]

parseAndCompileFile :: FilePath -> Flags -> IO ()
parseAndCompileFile path flags = do
    str <- readFile (path ++ ".smetana")
    case parse smetanaParser "" str of
        Left err -> print err
        Right xs -> case toEither $ allChecks xs of
            Left err -> putStr err
            Right val -> writeTo flags path $ smetanaToCPP val

main :: IO ()
main = do
    input <- getLine 
    case parse inputParse "" input of
        Left err -> print err
        Right (path, str) -> case stringsToOutputFlag str of
            Failure err -> putStrLn err
            Success xs -> case xs of
                [] -> parseAndCompileFile path defaultFlags
                [a] -> parseAndCompileFile path (defaultFlags { out = a }) 
