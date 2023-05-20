
module Test where

import Main
import qualified Test.QuickCheck as Q


main :: IO ()
main = do
    Q.quickCheck prop_my_io_action
    --Q.quickCheck prop_isRegular