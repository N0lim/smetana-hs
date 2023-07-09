{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Main where

import AST
import Parser
import Checker
import Data.Either (fromLeft)
import Data.Validation (toEither)
import Text.Parsec (parse)


main :: IO ()
main = do
    case parse smetanaParser "" test of
        Left err -> print err
        Right xs -> putStr $ fromLeft "WTF all OK" $ toEither $ allChecks xs

test = "Step 141. Go to step 3.\nStep 4. Swap step 4 with step 22.\nStep 3. Go to step 5."
