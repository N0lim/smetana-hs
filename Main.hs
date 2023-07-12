{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Main where

import AST
import Parser
import Checker
import CodeGen
import Data.Either (fromLeft)
import Data.Validation
import Text.Parsec (parse)


main :: IO ()
main = do
    case parse smetanaParser "" test of
        Left err -> print err
        Right xs -> case toEither $ allChecks xs of
            Left err -> putStr err
            Right val -> putStr $ smetanaToCPP val

test = "Step 0. Go to step 1.\nStep 1. Swap step 2 with step 1.\nStep 2. Go to step 1."
