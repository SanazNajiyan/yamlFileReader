
module Test where

import Lib
import qualified Test.QuickCheck as Q


main :: IO ()
main = do
    Q.quickCheck Lib.prop_my_io_action
    --Q.quickCheck Lib.prop_isRegular