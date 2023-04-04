{-# LANGUAGE DeriveGeneric #-}--to derive Generic instances


module Main where
import qualified Data.List as L
import Data.List (isPrefixOf)
import Data.List.Split as L
import Data.Function (on)
import Control.Monad as M (void)
import Control.Monad as M (guard)
import qualified Test.QuickCheck as Q
import Test.QuickCheck (Arbitrary(..), Gen, suchThat)
import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Monadic as Q (assert, monadicIO, pick, pre, run)
import Control.Applicative as AP
import Control.Monad (replicateM)
import Data.Yaml.Pretty
import Data.List as L (maximumBy, groupBy, sortOn)
import Data.Ord (comparing)
import Debug.Trace
import Control.Monad as MO (when)
import qualified Data.Char as C
import qualified Data.Yaml as Y
import qualified Data.Yaml.Parser as Y
import qualified Data.Yaml.Builder as YB
import Data.Aeson.Types (FromJSON(..), Value(..), withObject, parseJSON)
import qualified Data.Yaml as Y (encode, decode)
import Data.Yaml (FromJSON, parseJSON, ToJSON, ParseException)
import qualified Data.Yaml.Parser as Y
import Text.Libyaml as YL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.String (IsString(..))
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Yaml.Pretty as PP
import GHC.Generics
import qualified Data.Map.Strict as Map
import qualified Data.Map as Map

import Debug.Trace (trace)
--We (recursively) call a YamlTree regular if all of its (proper) subtrees have the same depth and are regular themselves. Please implement a QuickCheck test that checks whether a YamlTree is regular.


data YamlTree = YamlTree [(String, YamlTree)]
    deriving (Ord, Eq, Show, Generic)

--parse function reads the YAML file using the readYamlFile function from the yaml package and returns a Y.YamlValue representing the parsed YAML content
--with this implementation only YamlTree [("ExampleKey2",YamlTree [("ExampleKey2",YamlTree []),("ExampleKey2",YamlTree [])])] doesn't work!!!
--comment!!why you did it like that?
parse :: FilePath -> IO Y.YamlValue
parse path = do
  content <- BS.readFile path
  if BS.null content
    then return (Y.Mapping [] Nothing)
  else if (BS.strip content == BS.pack ":")
    then return (Y.Mapping [(T.empty, Y.Mapping [] Nothing)] Nothing)
  --else if ((last (BS.strip content)) == BS.pack ":")
  else if (BS.isSuffixOf (BS.pack ":") (BS.strip content))
    then return $ Y.Mapping [( TE.decodeUtf8 $ BS.init (BS.strip content), Y.Mapping [] Nothing)] Nothing
  else Y.readYamlFile path



-- parse :: FilePath -> IO Y.YamlValue
-- parse = Y.readYamlFile

convertToYAMLTree :: Y.YamlValue -> YamlTree
convertToYAMLTree (Y.Mapping list _) = YamlTree (L.map (\(xs,ys) -> (T.unpack xs, convertToYAMLTree ys)) list)
convertToYAMLTree (Y.Scalar xs _ _ _) = if BS.unpack xs == ""
  -- TODO: comment explaining why this is neccessary (otherwise we get empty nested values)
  then YamlTree []
  else YamlTree [(BS.unpack xs, YamlTree [])] --when we reach a Leaf
convertToYAMLTree (Y.Sequence list _) = error "Yaml lists are not supported" --YamlTree $ L.map (\x -> ("-", convertToYAMLTree x)) $ V.toList $ V.fromList list --In this implementation the list argument is first converted to a Vector using V.fromList, and then V.toList is used to convert the Vector to a list.
convertToYAMLTree (Y.Alias _) = error "Aliasses are not supported"

yamlTreeToYamlFile :: FilePath -> YamlTree -> IO ()
yamlTreeToYamlFile filePath tree = do
    let yamlText = yamlTreeToString tree
    writeFile filePath yamlText


indent :: Int -> String -> String
indent n line
  | null line = ""
  | otherwise = replicate n ' ' ++ line ++ "\n"

yamlTreeToStringIndented :: Int -> YamlTree -> String
yamlTreeToStringIndented _ (YamlTree []) = ""
yamlTreeToStringIndented indentLevel (YamlTree ((key, value):rest)) =
    subtreeText ++ yamlTreeToStringIndented indentLevel (YamlTree rest)
    where
        subtreeText = indent (indentLevel * 2) key' ++ indentedSubtreeText
        key' = if isStringWithSpaces key then "" else if null valueText then key else key ++ ":"
        indentedSubtreeText = if null valueText then "" else valueText
        valueText = if isStringWithSpaces key
                        then  L.intercalate "" $ map (indent $ (indentLevel + 1) * 2) (words key)
                        else yamlTreeToStringIndented (indentLevel + 1) value
--postProcessYamlTree is defined because when finding the leafs the keys with spaces are problematic and wrongly not taken as a separate leaf                        
--it splits the key into separate nodes with empty values, essentially creating a nested structure. If the key does not have spaces, it simply recursively applies the same function to its child nodes
postProcessYamlTree :: YamlTree -> YamlTree
postProcessYamlTree (YamlTree []) = YamlTree []
postProcessYamlTree (YamlTree ((k, yamlTree) : kyamls))
  | isStringWithSpaces k =
      let keys = words k
          subTrees = [ (key, YamlTree []) | key <- keys ]
          newSubTrees = map (\(key, _) -> (key, YamlTree [])) subTrees
          newKyaml = concatMap (\(k', st') -> [(k', st')]) newSubTrees
      in YamlTree newKyaml
  | otherwise =
      let kyaml = (k, postProcessYamlTree yamlTree)
          newKyamls = kyaml : map (\(k', yamlTree') -> (k', postProcessYamlTree yamlTree')) kyamls
      in YamlTree newKyamls

isStringWithSpaces :: String -> Bool
isStringWithSpaces s = ' ' `elem` s
--pretty printer for YamlTrees that generates a .yaml-file again from your hierarchy
yamlTreeToString :: YamlTree -> String
yamlTreeToString (YamlTree []) = ""
yamlTreeToString (YamlTree ((key, value):rest)) =
    subtreeText ++ yamlTreeToString (YamlTree rest)
    where
        subtreeText = indent 0 (key ++ ":") ++ indentedSubtreeText
        indentedSubtreeText = yamlTreeToStringIndented 1 value
---------q3-----
--If we pretty print a YAML tree, we should be able to parse it and get the same tree back.
--taking a generated YamlTree and check if it can be parsed back into the same tree
--The property checks whether parsing a YamlTree into a YamlValue and then pretty-printing it back into a YamlTree results in the same YamlTree
--The prop_parser_prettyprinter function takes a YamlTree as input, converts it to text using yamlTreeToString, and then parses the text using parseYaml. If the parse is successful and the resulting tree is equal to the original input tree, the function returns True. Otherwise, it returns False.

prop_my_io_action :: YamlTree -> Q.Property
prop_my_io_action yamltree =
  Q.monadicIO $ do
    let fileName = "output2.yaml"
    run $ yamlTreeToYamlFile fileName yamltree
    yamlValue <- run $ parse fileName
    let result = convertToYAMLTree yamlValue
    Q.assert (result == yamltree)
--rewrap extension- altq
--The instance FromJSON Y.YamlValue is defined simply as parseJSON = parseJSON,
--which means that it will use the default implementation of parseJSON provided
--by FromJSON. Since YamlValue is already an instance of FromJSON, this is
--sufficient to define the instance.
instance FromJSON Y.YamlValue where
    parseJSON = parseJSON


-------------nr.1 version-----
--In this version, arbitraryYamlTree generates a YamlTree of depth up to 5 by recursively calling itself with n-1
instance Q.Arbitrary YamlTree where
    arbitrary = arbitraryYamlTree 8
    
arbitraryYamlTree :: Int -> Gen YamlTree
arbitraryYamlTree 0 = return $ YamlTree []
arbitraryYamlTree n = do
    numKeys <- Q.choose (1, 3)
    keysAndVals <- replicateM numKeys arbitraryKeyAndVal
    return $ YamlTree (mergeTuples keysAndVals)
    where
        arbitraryKeyAndVal = do
            key <- arbitraryKey
            val <- if n == 1
                       then return $ YamlTree []
                       else arbitraryYamlTree (n-1)
            return (key, val)
        arbitraryKey = do
            key <- Q.elements ["Spring", "Summer", "autumn","ExampleSeason", "WINTER", "love", "HAPPY", "go", "LUCKY"]
            --key <- Q.listOf $ Q.elements validChars
            return $ if null key then "default_key" else key

-------------nr.2 choice with reasonable maxKeyLength and levels---------
-- instance Q.Arbitrary YamlTree where
--     arbitrary = Q.sized (arbitraryYamlTree 5)
    
-- arbitraryYamlTree :: Int -> Int -> Gen YamlTree
-- arbitraryYamlTree 0 _ = return $ YamlTree []
-- arbitraryYamlTree n maxKeyLength = do
--     numKeys <- Q.choose (1, 3)
--     keysAndVals <- replicateM numKeys arbitraryKeyAndVal
--     return $ YamlTree (mergeTuples keysAndVals)
--     where
--         arbitraryKeyAndVal = do
--             key <- arbitraryKey 8
--             val <- if n == 1
--                        then return $ YamlTree []
--                        else arbitraryYamlTree (n-1) maxKeyLength
--             return (key, val)
-- arbitraryKey :: Int -> Gen String
-- arbitraryKey maxKeyLength = do
--     let validChars = ['A','B','c','d', 'F', 'G']
--     keyLength <- Q.choose (1, 8)
--     key <- replicateM keyLength (Q.elements validChars)
--     return $ if null key then "default_key" else key
            
--Define a helper function to merge tuples with the same value of YamlTree []
mergeTuples :: [(String, YamlTree)] -> [(String, YamlTree)]
mergeTuples [] = []
mergeTuples ((key1, YamlTree []):(key2, YamlTree []):rest) = mergeTuples ((key1 ++ " " ++ key2, YamlTree []) : rest)
mergeTuples (kv:rest) = kv : mergeTuples rest

-------q4-----------
--finding the maximum depth of any sub-tree rooted at a value in kvs
depthi :: YamlTree -> Int
depthi (YamlTree []) = 1 -- base case, an empty YamlTree has depth 1
depthi (YamlTree kvs) = 1 + maximum (map (depthi . snd) kvs)

minDepth :: YamlTree -> Int
minDepth (YamlTree []) = 1
minDepth (YamlTree kvs) = 1 + minimum (map (depthi . snd) kvs)
---------q5
-- | Check whether a given YamlTree is regular
isRegular :: YamlTree -> Bool
isRegular (YamlTree []) = True
isRegular (YamlTree kvs) =
  let depths = map (depthi . snd) kvs
      isRegularSubtree (_, subtree) = isRegular subtree && depthi subtree == maximum depths
  in all isRegularSubtree kvs

-- | QuickCheck property to test whether a given YamlTree is regular
prop_isRegular :: YamlTree -> Bool
prop_isRegular = isRegular
-----------q6
--Write a function that "regularizes" a YamlTree into a regular one by copying levels in the hierarchy. Please ensure that this function regularizes the YamlTree implemented by "instruments-hierarchy.yaml" to the one implemented by "instruments-hierarchy-regular.yaml". (Again, we want a generally applicable solution. No hard-coding of the example allowed.)
-- data YamlTree = YamlTree [(String, YamlTree)]
regularize :: YamlTree -> YamlTree
regularize yamlTree@(YamlTree subTrees) =
  let maxDepth = depthi yamlTree
      newTrees = regularizeSubTrees maxDepth subTrees
  in YamlTree newTrees

regularizeSubTrees :: Int -> [(String, YamlTree)] -> [(String, YamlTree)]
regularizeSubTrees 0 [] = []
regularizeSubTrees 0 _  = error "Cannot shrink tree"
regularizeSubTrees depth trees = let x = map (regularizeSubTree (depth -1)) trees
  in trace (show x) x
            --we compare the inner yamlTreewith desired depth we want

regularizeSubTree :: Int -> (String, YamlTree) -> (String, YamlTree)
regularizeSubTree depth a@(key, val) =
    let (key', YamlTree val') = if depthi val /= depth then addLevel (depth - depthi val) (key, val)
                                     else a-- make if part a binding andcall it inregularizeSubTrees
    in (key', YamlTree (regularizeSubTrees depth val'))

addLevel :: Int ->(String, YamlTree) -> (String, YamlTree)
addLevel 0 tuple = tuple
addLevel i (key, val) = addLevel (i-1) (key , YamlTree [(key, val)])

treeToList :: YamlTree -> [(String, YamlTree)]
treeToList (YamlTree trees) = trees
-----------q7--------------------
--write an interactive (IO) program that traverses a YamlTree that checks whether any of the leaf labels overlap and generates a warning for them, specifying the multiple paths through the tree that lead to the leaf. For example, for "instruments-hierarchy.yaml", it should print
-- "WARNING: instrument "EUR" occurs in multiple places in hierarchy:
-- "currencies - usd-fx - EUR"
-- "currencies - eur-fx - EUR""
-- and for "instruments-hierarchy-regular.yaml" it should print
-- "WARNING: instrument "EUR" occurs in multiple places in hierarchy:
-- "currencies - currencies - usd-fx - EUR"
-- "currencies - currencies - eur-fx - EUR""
-- data YamlTree = YamlTree [(String, YamlTree)]

--how many of the same leaf! In the leafCount function, the path parameter is used to keep track of the paths to the current leaf node. When a leaf node is found, the function checks if there is already an entry in the list of leaf counts for that node (which could happen if the same leaf node appears in multiple paths in the tree). If an entry is found, the count and paths fields are updated to include the current node's count and path, respectively. If no entry is found, a new entry is added to the list with the current node's count and path.
leafCount :: (String, Int, [String]) -> [(String, Int, [String])] -> [(String, Int, [String])]
leafCount (k,v,path) kvs = case L.find (\(k',_,_) -> k' == k) kvs of
                        Just (_,count, paths) -> (k, count+v, path ++ paths) : filter (\(k',_,_) -> k' /= k) kvs
                        Nothing -> (k,v,path) : kvs

-- Traverse the tree and generate the leaf dictionary- generates a list of tuples representing the counts and paths of each leaf node
leafCounts :: YamlTree -> [(String, Int, [String])]
leafCounts (YamlTree []) = []
leafCounts (YamlTree kvs) = concatMap (\(k,v) -> if isLeaf (k,v) then leafCount (k,1,[]) (leafDict kvs []) else leafCounts v) kvs

isLeaf :: (String, YamlTree) -> Bool
isLeaf (_, YamlTree []) = True
isLeaf _ = False
-- Generate a list of tuples representing counts and paths to leaf nodes.
-- If the input is not a leaf node, recursively call `treeToList` and
-- `leafDict` with the updated path to flatten the subtree and accumulate
-- the result.
leafDict :: [(String, YamlTree)] -> [String] -> [(String, Int, [String])]
leafDict [] _ = []
leafDict ((k,v):kvs) path | isLeaf(k,v) = (k, 1, reverse (k:path)) : leafDict kvs path
                          | otherwise   = leafDict (treeToList v) (k:path) ++ leafDict kvs path


printWarning :: (String, [[String]]) -> IO ()
printWarning (label, paths) = do
  putStrLn $ "WARNING: instrument \"" ++ label ++ "\" occurs in multiple places in hierarchy:"
  mapM_ (\path -> putStrLn $ "- \"" ++ (unwords path) ++ "\"") paths


-- checkDuplicateLeafs :: YamlTree -> IO ()
-- checkDuplicateLeafs (YamlTree []) = print "sorry Tree empty"
-- checkDuplicateLeafs a@(YamlTree kvs) = do
--     let dict = mapM_ (\(k,v) -> (v,1)) (treeToList a)
---------------------------------------------------------------------------------
main :: IO ()
main = do
    -- yamlValue <- parse "instruments-hierarchy.yaml"
    -- let yamlTree = convertToYAMLTree yamlValue
    -- --let yamlTree = YamlTree [("a", YamlTree [("b", YamlTree [("c", YamlTree []), ("d", YamlTree [])]), ("e", YamlTree [])]), ("f", YamlTree [])]
    -- -- print (treeToList yamlTree)
    --------q7 test
    -- print ("leafCounts" ++ " :" ++ " ")
    -- print (leafCounts yamlTree)
    -- print ("leafDict" ++ " :" ++ " ")
    -- print (leafDict (treeToList yamlTree) [])
    --"leafDict= [("US10 US20 US30",1,["bonds","long-bonds","us-long-bonds","US10 US20 US30"]),("KR10",1,["bonds","long-bonds","KR10"]),("OAT BTP",1,["bonds","long-bonds","eu-short-bonds","OAT BTP"]),("US2 US3 US5",1,["bonds","short-bonds","us-short-bonds","US2 US3 US5"]),("SHATZ BTP3",1,["bonds","short-bonds","eu-short-bonds","SHATZ BTP3"]),("KR3",1,["bonds","short-bonds","KR3"]),("EURIBOR FED",1,["bonds","STIRs","EURIBOR FED"]),("CRUDE_W_mini BRENT-LAST HEATOIL GASOILINE",1,["energies","oil","CRUDE_W_mini BRENT-LAST HEATOIL GASOILINE"]),("GAS_US_mini GAS-LAST",1,["energies","gas","GAS_US_mini GAS-LAST"]),("INR-SGX",1,["currencies","usd-fx","INR-SGX"]),("EUR",1,["currencies","usd-fx","EUR"]),("GBP",1,["currencies","usd-fx","GBP"]),("EUR",1,["currencies","eur-fx","EUR"]),("EURGBP",1,["currencies","eur-fx","EURGBP"]),("SP500_mini R1000",1,["equities","us-equities","SP500_mini R1000"]),("AEX DAX",1,["equities","eu-equities","AEX DAX"]),("KOSPI",1,["equities","asia-equities","KOSPI"]),("VIX V2X VNKI",1,["vol","VIX V2X VNKI"])]
    -- let yamlTree = YamlTree [("a", YamlTree [("b", YamlTree [("c", YamlTree []), ("d", YamlTree [])]), ("e", YamlTree [])]), ("f", YamlTree [])]
    -- let overlappingLeaves = findOverlappingLeaves yamlTree
    -- mapM_ printWarning overlappingLeaves
    -- input  <- Y.decodeFileThrow "instruments-hierarchy.yaml"
    -- output <- Y.decodeFileThrow "instruments-hierarchy-regular.yaml"
    -- let regularized = regularize input
    -- putStrLn (show $ regularized == output)
    -- yamlValue <- parse "instruments-hierarchy.yaml"
    -- print yamlValue
    -- let yamlTree = convertToYAMLTree yamlValue
    -- print yamlTree
    -- let regularized = regularize yamlTree
    -- -- yamlValue2 <- parse "instruments-hierarchy-regular.yaml"
    -- -- let yamlTree2 = convertToYAMLTree yamlValue2
    -- -- -- --print (prop_isRegular yamlTree) --works fine
    -- -- putStrLn (show $ regularized == yamlTree2)
    -- print regularized
    -- -- print (depthi yamlTree)
    -- -- print (isRegular yamlTree2)
    -- yamlTreeToYamlFile "output1.yaml" regularized
    -- print (isRegular regularized)
    ----------------------testing partition---------not working []---
    -- let testInput = [("bonds",YamlTree [("long-bonds",YamlTree [("us-long-bonds",YamlTree [("US10 US20 US30",YamlTree [])]),("KR10",YamlTree [("KR10",YamlTree [])]),("eu-short-bonds",YamlTree [("OAT BTP",YamlTree [])])]),("short-bonds",YamlTree [("us-short-bonds",YamlTree [("US2 US3 US5",YamlTree [])]),("eu-short-bonds",YamlTree [("SHATZ BTP3",YamlTree [])]),("KR3",YamlTree [("KR3",YamlTree [])])]),("STIRs",YamlTree [("STIRs",YamlTree [("EURIBOR FED",YamlTree [])])])]),("energies",YamlTree [("energies",YamlTree [("oil",YamlTree [("CRUDE_W_mini BRENT-LAST HEATOIL GASOILINE",YamlTree [])]),("gas",YamlTree [("GAS_US_mini GAS-LAST",YamlTree [])])])]),("currencies",YamlTree [("currencies",YamlTree [("usd-fx",YamlTree [("INR-SGX EUR GBP",YamlTree [])]),("eur-fx",YamlTree [("EUR EURGBP",YamlTree [])])])]),("equities",YamlTree [("equities",YamlTree [("us-equities",YamlTree [("SP500_mini R1000",YamlTree [])]),("eu-equities",YamlTree [("AEX DAX",YamlTree [])]),("asia-equities",YamlTree [("KOSPI",YamlTree [])])])]),("vol",YamlTree [("vol",YamlTree [("vol",YamlTree [("VIX V2X VNKI",YamlTree [])])])])]
    -- let subtrees = concatMap (\(_, tree) -> treeToList tree) testInput
    -- let okTrees = L.filter (\(_, tree) -> depthi tree == 4) subtrees
    -- print okTrees
--------------------------------
    -- yamlTreeToYamlFile "output.yaml" yamlTree
    -- yamlTreeToYamlFile "output1.yaml" (YamlTree $ regularizedTree)
    -- print regularizedTree



    -- putStrLn "Error: regularize test failed"
    -- putStrLn "Input:"
    -- let testInput = YamlTree [("", YamlTree [])] --this passed even with "aa"
    -- let testInput = YamlTree [("bonds",YamlTree [("long-bonds",YamlTree [("us-long-bonds",YamlTree [("US10 US20 US30",YamlTree [])]),("KR10",YamlTree [("KR10",YamlTree [])]),("eu-short-bonds",YamlTree [("OAT BTP",YamlTree [])])]),("short-bonds",YamlTree [("us-short-bonds",YamlTree [("US2 US3 US5",YamlTree [])]),("eu-short-bonds",YamlTree [("SHATZ BTP3",YamlTree [])]),("KR3",YamlTree [("KR3",YamlTree [])])]),("STIRs",YamlTree [("STIRs",YamlTree [("EURIBOR FED",YamlTree [])])])]),("energies",YamlTree [("energies",YamlTree [("oil",YamlTree [("CRUDE_W_mini BRENT-LAST HEATOIL GASOILINE",YamlTree [])]),("gas",YamlTree [("GAS_US_mini GAS-LAST",YamlTree [])])])]),("currencies",YamlTree [("currencies",YamlTree [("usd-fx",YamlTree [("INR-SGX EUR GBP",YamlTree [])]),("eur-fx",YamlTree [("EUR EURGBP",YamlTree [])])])]),("equities",YamlTree [("equities",YamlTree [("us-equities",YamlTree [("SP500_mini R1000",YamlTree [])]),("eu-equities",YamlTree [("AEX DAX",YamlTree [])]),("asia-equities",YamlTree [("KOSPI",YamlTree [])])])]),("vol",YamlTree [("vol",YamlTree [("vol",YamlTree [("VIX V2X VNKI",YamlTree [])])])])]
    -- let regularized = regularize testInput
    -- print (regularized == YamlTree [("", YamlTree [])])
    -- putStrLn "Expected output:"
    -- print output
    -- putStrLn "Actual output:"
    -- print regularized
    --Q.verboseCheck (prop_isRegular)
    -- let prop_empty_tree = Q.property $ MO.when (nullYamlTree yamltree) Q.discard
    --     prop_nonempty_tree = Q.property prop_my_io_action
    -- Q.quickCheck prop_empty_tree
    -- Q.quickCheck prop_nonempty_tree
    --let testInput = YamlTree [("bonds",YamlTree [("long-bonds",YamlTree [("us-long-bonds",YamlTree [("US10 US20 US30",YamlTree [])]),("KR10",YamlTree [("KR10",YamlTree [])]),("eu-short-bonds",YamlTree [("OAT BTP",YamlTree [])])]),("short-bonds",YamlTree [("us-short-bonds",YamlTree [("US2 US3 US5",YamlTree [])]),("eu-short-bonds",YamlTree [("SHATZ BTP3",YamlTree [])]),("KR3",YamlTree [("KR3",YamlTree [])])]),("STIRs",YamlTree [("STIRs",YamlTree [("EURIBOR FED",YamlTree [])])])]),("energies",YamlTree [("energies",YamlTree [("oil",YamlTree [("CRUDE_W_mini BRENT-LAST HEATOIL GASOILINE",YamlTree [])]),("gas",YamlTree [("GAS_US_mini GAS-LAST",YamlTree [])])])]),("currencies",YamlTree [("currencies",YamlTree [("usd-fx",YamlTree [("INR-SGX EUR GBP",YamlTree [])]),("eur-fx",YamlTree [("EUR EURGBP",YamlTree [])])])]),("equities",YamlTree [("equities",YamlTree [("us-equities",YamlTree [("SP500_mini R1000",YamlTree [])]),("eu-equities",YamlTree [("AEX DAX",YamlTree [])]),("asia-equities",YamlTree [("KOSPI",YamlTree [])])])]),("vol",YamlTree [("vol",YamlTree [("vol",YamlTree [("VIX V2X VNKI",YamlTree [])])])])]
    --print (depth testInput)
    --let testInput = YamlTree []
    -- let testInput = YamlTree [("ExampleKey2",YamlTree [("ExampleKey2",YamlTree []),("ExampleKey2",YamlTree [])])] --this still needs to be handled!!!!!
    -- result <- Q.quickCheck (prop_my_io_action testInput)
    -- print result
    -- Q.verboseCheck (prop_my_io_action testInput)
    
     --print $ yamlTreeToString (YamlTree [("gg", YamlTree [])])
    -- print $ convertToYAMLTree (":\n")
    -- yamlTreeToYamlFile "output2.yaml" $ YamlTree [("ExampleKey2",YamlTree [("ExampleKey2",YamlTree []),("ExampleKey2",YamlTree [])])]
    -- yamlValue <- parse "output2.yaml"
    -- print yamlValue
    --print $ convertToYAMLTree yamlValue
    --print (T.pack ":")
    --Q.verboseCheck (prop_my_io_action)
    -- Q.quickCheck (prop_my_io_action)
    -- yamlValue <- parse "test.yaml"
    --let yamlTree = convertToYAMLTree yamlValue
    --Q.quickCheck $ prop_parser_prettyprinter
    --Q.quickCheck (prop_parser_prettyprinter)
    -- print $ prop_my_io_action (YamlTree [("abvc", YamlTree[("ttttt", YamlTree[])])])
    -----------test partition which is not working!!
    -- let test = testi 3 [("long-bond",YamlTree [("long-bonds",YamlTree [("us-long-bonds",YamlTree [("US S10 US20 US30",YamlTree [])]),("KR10",YamlTree [("",YamlTree [])]),("eu-short-bonds",YamlTree [("OAT BTP",YamlTree [])])])]),("short-bond",YamlTree [("short-bonds",YamlTree [("uus-short-bonds",YamlTree [("US2 US3 US5",YamlTree [])]),("eu-short-bonds",YamlTree [("SHA ATZ BTP3",YamlTree [])]),("KR3",YamlTree [("",YamlTree [])])])]),("VIX V2X VNK",YamlTree [("VNKI",YamlTree [])])]
    -- let (trimmedTrees, added) = L.partition (\(_, tree) -> depthi tree == 1) test -- this produces []
    -- print trimmedTrees
    -- print test --[("long-bonds", ("us-long-bonds",YamlTree [("US S10 US20 US30",YamlTree [])]))]))--[("long-bond",YamlTree [("long-bonds",YamlTree [("us-long-bonds",YamlTree [("US S10 US20 US30",YamlTree [])]),("KR10",YamlTree [("",YamlTree [])]),("eu-short-bonds",YamlTree [("OAT BTP",YamlTree [])])])]),("short-bond",YamlTree [("short-bonds",YamlTree [("uus-short-bonds",YamlTree [("US2 US3 US5",YamlTree [])]),("eu-short-bonds",YamlTree [("SHA ATZ BTP3",YamlTree [])]),("KR3",YamlTree [("",YamlTree [])])])]),("VIX V2X VNK",YamlTree [("VNKI",YamlTree [])])])
    -- print (isRegular (YamlTree $ test))
    -- print yamlValue
    -- let yamlTree = convertToYAMLTree yamlValue
    -- print yamlTree
    -- yamlTreeToYamlFile "output.yaml" yamlTree
    -- print $ yamlTreeToString (YamlTree [("", YamlTree[])])
    -- print $ yamlTreeToString $ YamlTree[]
    --print $ yamlTreeToString (YamlTree [("abvc", YamlTree[("ttttt", YamlTree[])])])
    -- print $ parseYaml ("abvc:\n  ttttt\n")
    --print $ parseYaml ""
    --print (BS.pack ("abvc:\n  ttttt\n"))
    --print $ Right $ Y.decodeEither' (BS.pack ("abvc:\n  ttttt\n"))
    --print $ prop_parser_prettyprinter $ YamlTree [("abvc", YamlTree[("ttttt", YamlTree[])])]
    --print $ prop_parser_prettyprinter (YamlTree [("bonds",YamlTree [("long-bonds",YamlTree [("us-long-bonds",YamlTree [("US10 US20 US30",YamlTree [])]),("KR10",YamlTree [("KR10",YamlTree [])]),("eu-short-bonds",YamlTree [("OAT BTP",YamlTree [])])]),("short-bonds",YamlTree [("us-short-bonds",YamlTree [("US2 US3 US5",YamlTree [])]),("eu-short-bonds",YamlTree [("SHATZ BTP3",YamlTree [])]),("KR3",YamlTree [("KR3",YamlTree [])])]),("STIRs",YamlTree [("STIRs",YamlTree [("EURIBOR FED",YamlTree [])])])]),("energies",YamlTree [("energies",YamlTree [("oil",YamlTree [("CRUDE_W_mini BRENT-LAST HEATOIL GASOILINE",YamlTree [])]),("gas",YamlTree [("GAS_US_mini GAS-LAST",YamlTree [])])])]),("currencies",YamlTree [("currencies",YamlTree [("usd-fx",YamlTree [("INR-SGX EUR GBP",YamlTree [])]),("eur-fx",YamlTree [("EUR EURGBP",YamlTree [])])])]),("equities",YamlTree [("equities",YamlTree [("us-equities",YamlTree [("SP500_mini R1000",YamlTree [])]),("eu-equities",YamlTree [("AEX DAX",YamlTree [])]),("asia-equities",YamlTree [("KOSPI",YamlTree [])])])]),("vol",YamlTree [("vol",YamlTree [("vol",YamlTree [("VIX V2X VNKI",YamlTree [])])])])])
    --print $ prop_parser_prettyprinter (YamlTree [("", YamlTree [])])
    --bool <- prop_parser_prettyprinter (YamlTree [("bonds",YamlTree [("long-bonds",YamlTree [("us-long-bonds",YamlTree [("US10 US20 US30",YamlTree [])]),("KR10",YamlTree [("KR10",YamlTree [])]),("eu-short-bonds",YamlTree [("OAT BTP",YamlTree [])])]),("short-bonds",YamlTree [("us-short-bonds",YamlTree [("US2 US3 US5",YamlTree [])]),("eu-short-bonds",YamlTree [("SHATZ BTP3",YamlTree [])]),("KR3",YamlTree [("KR3",YamlTree [])])]),("STIRs",YamlTree [("STIRs",YamlTree [("EURIBOR FED",YamlTree [])])])]),("energies",YamlTree [("energies",YamlTree [("oil",YamlTree [("CRUDE_W_mini BRENT-LAST HEATOIL GASOILINE",YamlTree [])]),("gas",YamlTree [("GAS_US_mini GAS-LAST",YamlTree [])])])]),("currencies",YamlTree [("currencies",YamlTree [("usd-fx",YamlTree [("INR-SGX EUR GBP",YamlTree [])]),("eur-fx",YamlTree [("EUR EURGBP",YamlTree [])])])]),("equities",YamlTree [("equities",YamlTree [("us-equities",YamlTree [("SP500_mini R1000",YamlTree [])]),("eu-equities",YamlTree [("AEX DAX",YamlTree [])]),("asia-equities",YamlTree [("KOSPI",YamlTree [])])])]),("vol",YamlTree [("vol",YamlTree [("vol",YamlTree [("VIX V2X VNKI",YamlTree [])])])])])
    --print bool
    -- print yamlTree)
    -- --let tree = YamlTree [("bonds",YamlTree [("long-bonds",YamlTree [("us-long-bonds",YamlTree [("US10 US20 US30",YamlTree [])]),("KR10",YamlTree [("KR10",YamlTree [])]),("eu-short-bonds",YamlTree [("OAT BTP",YamlTree [])])]),("short-bonds",YamlTree [("us-short-bonds",YamlTree [("US2 US3 US5",YamlTree [])]),("eu-short-bonds",YamlTree [("SHATZ BTP3",YamlTree [])]),("KR3",YamlTree [("KR3",YamlTree [])])]),("STIRs",YamlTree [("STIRs",YamlTree [("EURIBOR FED",YamlTree [])])])]),("energies",YamlTree [("energies",YamlTree [("oil",YamlTree [("CRUDE_W_mini BRENT-LAST HEATOIL GASOILINE",YamlTree [])]),("gas",YamlTree [("GAS_US_mini GAS-LAST",YamlTree [])])])]),("currencies",YamlTree [("currencies",YamlTree [("usd-fx",YamlTree [("INR-SGX EUR GBP",YamlTree [])]),("eur-fx",YamlTree [("EUR EURGBP",YamlTree [])])])]),("equities",YamlTree [("equities",YamlTree [("us-equities",YamlTree [("SP500_mini R1000",YamlTree [])]),("eu-equities",YamlTree [("AEX DAX",YamlTree [])]),("asia-equities",YamlTree [("KOSPI",YamlTree [])])])]),("vol",YamlTree [("vol",YamlTree [("vol",YamlTree [("VIX V2X VNKI",YamlTree [])])])])]
    -- --print (yamlTreeToStringIndented tree)
    -- yamlTreeToYamlFile "output.yaml" yamlTree
    -- --writeFile "output.yaml" (yamlTreeToTextIndented tree)
---------test postProcessing
    
  --let input = YamlTree [("bonds",YamlTree [("long-bonds",YamlTree [("us-long-bonds",YamlTree [("US10 US20 US30",YamlTree [])]),("KR10",YamlTree [("KR10",YamlTree [])]),("eu-short-bonds",YamlTree [("OAT BTP",YamlTree [])])]),("short-bonds",YamlTree [("us-short-bonds",YamlTree [("US2 US3 US5",YamlTree [])]),("eu-short-bonds",YamlTree [("SHATZ BTP3",YamlTree [])]),("KR3",YamlTree [("KR3",YamlTree [])])]),("STIRs",YamlTree [("STIRs",YamlTree [("EURIBOR FED",YamlTree [])])])]),("energies",YamlTree [("energies",YamlTree [("oil",YamlTree [("CRUDE_W_mini BRENT-LAST HEATOIL GASOILINE",YamlTree [])]),("gas",YamlTree [("GAS_US_mini GAS-LAST",YamlTree [])])])]),("currencies",YamlTree [("currencies",YamlTree [("usd-fx",YamlTree [("INR-SGX EUR GBP",YamlTree [])]),("eur-fx",YamlTree [("EUR EURGBP",YamlTree [])])])]),("equities",YamlTree [("equities",YamlTree [("us-equities",YamlTree [("SP500_mini R1000",YamlTree [])]),("eu-equities",YamlTree [("AEX DAX",YamlTree [])]),("asia-equities",YamlTree [("KOSPI",YamlTree [])])])]),("vol",YamlTree [("vol",YamlTree [("vol",YamlTree [("VIX V2X VNKI",YamlTree [])])])])]
  -- let  input = YamlTree [("bonds",YamlTree [("long-bonds",YamlTree [("us-long-bonds",YamlTree [("US10 US20 US30",YamlTree [])]),("KR10",YamlTree [("KR10",YamlTree [])])])])]
  -- print (postProcessYamlTree input) --output: YamlTree [("bonds",YamlTree [("long-bonds",YamlTree [("us-long-bonds",YamlTree [("US10",YamlTree []),("US20",YamlTree []),("US30",YamlTree [])]),("KR10",YamlTree [("KR10",YamlTree [])]),("eu-short-bonds",YamlTree [("OAT",YamlTree []),("BTP",YamlTree [])])]),("short-bonds",YamlTree [("us-short-bonds",YamlTree [("US2",YamlTree []),("US3",YamlTree []),("US5",YamlTree [])]),("eu-short-bonds",YamlTree [("SHATZ",YamlTree []),("BTP3",YamlTree [])]),("KR3",YamlTree [("KR3",YamlTree [])])]),("STIRs",YamlTree [("STIRs",YamlTree [("EURIBOR",YamlTree []),("FED",YamlTree [])])])]),("energies",YamlTree [("energies",YamlTree [("oil",YamlTree [("CRUDE_W_mini",YamlTree []),("BRENT-LAST",YamlTree []),("HEATOIL",YamlTree []),("GASOILINE",YamlTree [])]),("gas",YamlTree [("GAS_US_mini",YamlTree []),("GAS-LAST",YamlTree [])])])]),("currencies",YamlTree [("currencies",YamlTree [("usd-fx",YamlTree [("INR-SGX",YamlTree []),("EUR",YamlTree []),("GBP",YamlTree [])]),("eur-fx",YamlTree [("EUR",YamlTree []),("EURGBP",YamlTree [])])])]),("equities",YamlTree [("equities",YamlTree [("us-equities",YamlTree [("SP500_mini",YamlTree []),("R1000",YamlTree [])]),("eu-equities",YamlTree [("AEX",YamlTree []),("DAX",YamlTree [])]),("asia-equities",YamlTree [("KOSPI",YamlTree [])])])]),("vol",YamlTree [("vol",YamlTree [("vol",YamlTree [("VIX",YamlTree []),("V2X",YamlTree []),("VNKI",YamlTree [])])])])]
------------------------
  --arbitrary so that it never generates "" string.
--I need to handle empty yaml file . can not be parsed ?
--make a simplist yamlTree with text
--pretty prineter should
--try to replace the element of the tree to that generator
--arbitrary should parse your specific tree
--then make it general . using generator funciton like listOf

--I need to create an instance of the Arbitrary typeclass for your YamlTree type in order to use it with QuickCheck's quickCheck function. This instance tells QuickCheck how to generate random YamlTree values for testing.
-- instance Arbitrary MyType where
--   arbitrary = do
--     x <- arbitraryChar
--     y <- arbitraryChar
--     return (MyType x y)
    -- To generate a random value of type a, we need a generator for values of that type: Gen a. The default generator for values of any type is arbitrary, which is a method of QuickCheck's Arbitrary type class
    -- Q.quickCheckWith Q.stdArgs { Q.maxSuccess = 100 } prop_parser_prettyprinter
    -- y1 <- Q.generate arbitrary :: IO YamlTree
    -- y2 <- Q.generate arbitrary :: IO YamlTree
    -- putStrLn $ show y1
    -- putStrLn $ show y2
--SOALLLLLLA:problems: sanaz -> sanaz: in yaml file , seeing the result of prop not possible, asking about the YamlTree definition, YamlTree[YamlTree]??line 19 hierarchy.yaml        KR3:
--SOALLLLA: decodeFileThrow why doesn't respond-yaml file weird. not ideal structure. leaf missing, to regularize it is very inherent of . not how yaml works, how yaml lib works with spaces I needed to postprocess a bit
-- testi :: Int -> [(String, YamlTree)] -> [(String, YamlTree)]
-- testi depth trees
--   | depth == 0 = trees
--   | otherwise =
--     let subtrees = concatMap (\(_, tree) -> treeToList tree) trees

