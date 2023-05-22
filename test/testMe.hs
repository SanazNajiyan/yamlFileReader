
module Test where

import qualified Test.QuickCheck as Q
import Lib

main :: IO ()
main = do
    --Q.quickCheck prop_my_io_action
    Q.quickCheck Lib.prop_isRegular