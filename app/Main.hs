{-# LANGUAGE DeriveGeneric #-}--to derive Generic instances


module Main where
import Lib
import qualified Data.Map as Map
import qualified Data.List as L
import System.IO (hFlush, stdout)
import Data.List.Split (splitOn)


--q.14
-- Finally, put all the pieces together into an interactive program that
-- - reads in a yaml file at a user-specified path (e.g. "/home/henkie/generated_configs/instrument_hierarchy.yaml")
-- - parses the yaml (like our "instruments-hierarchy.yaml") into a YamlTree
-- - regularizer into a regular YamlTree, producing a YamlTree that would pretty print to "instruments-hierarchy-regular.yaml" for our particular example
-- - checks for overlapping leaf labels and prints warnings and (interactively) converts the regular YamlTree into a WeightedYamlTree, in such a way that our example would generate the interaction in "instruments-interaction.log"
--------------------------------------------------------------------------------
main :: IO ()
main = do
--reads in a yaml file at a user-specified path (e.g. "/home/henkie/generated_configs/instrument_hierarchy.yaml")
--parses the yaml (like our "instruments-hierarchy.yaml") into a YamlTree
    putStrLn $ "Please input a source file location for the .yaml you want to parse! " 
    putStr "> "
    hFlush stdout
    path <- getLine
    yamlValueInput <- Lib.parse path
    let yamlTree' = Lib.convertToYAMLTree yamlValueInput
    let yamlTree = Lib.postProcessYamlTree yamlTree'
-- - regularizer into a regular YamlTree, producing a YamlTree that would pretty print to "instruments-hierarchy-regular.yaml" for our particular example
    putStrLn " "
    let regularized = Lib.regularize yamlTree
    Lib.yamlTreeToYamlFile "instruments-regular.yaml" regularized
    putStrLn " "
    --putStrLn $ "it is " ++ show (isRegular regularized) ++ " that the output YamlTree is regularize!!!" ++ " with the depth: " ++ show (depthi regularized)
    putStrLn " "
-- - checks for overlapping leaf labels and prints warnings
    let leafCountsList = Lib.leafCounts' (treeToList yamlTree) []
    checkOverlappingLabels $ Map.toList leafCountsList
--and (interactively) converts the regular YamlTree into a WeightedYamlTree, in such a way that our example would generate the interaction in "instruments-interaction.log"
    putStrLn " "
    wyTree <- Lib.userWeightRequest regularized
    --print wyTree
--- pretty prints the resulting WeightedYamlTree to a file (e.g. "/home/henkie/generated_configs/instrument_hierarchy_weighted.yaml")
    let st = Lib.wyTreePrint' wyTree
    writeFile "instrument_hierarchy_weighted.yaml" st
    let pathList = splitOn "\\" path
    -- Remove the last element from the list
    let keepThisPath = init pathList
    -- Join the remaining elements back into a string
    let keepThisPath' = unwords keepThisPath
    let outputFilePath = keepThisPath' ++ " instrument_hierarchy_weighted.yaml"
    let outputFilePathList = words outputFilePath
    let final = L.intercalate "\\" outputFilePathList
    putStrLn $ "\n" ++ "Output was written to \"" ++ final ++ "\"." --    -- Print the modified path
    writeFile final st
