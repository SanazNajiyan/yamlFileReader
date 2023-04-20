{-# LANGUAGE DeriveGeneric #-}--to derive Generic instances


module Main where
import qualified Data.List as L
import Data.List (isPrefixOf)
import Data.List.Split as L
import Data.Function (on)
import Control.Monad as M (void)
import System.IO (hFlush, stdout)
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
import qualified Data.Map as Map
import Debug.Trace (trace)
--We (recursively) call a YamlTree regular if all of its (proper) subtrees have the same depth and are regular themselves. Please implement a QuickCheck test that checks whether a YamlTree is regular.


data YamlTree = YamlTree [(String, YamlTree)]
    deriving (Ord, Eq, Show, Generic)

-- WeightedYamlTree should be the same as above but with a extra Float in the constructor
-- PYamlTree should be the same as the above, but with a type variable 'a' instead of the Float
-- then, redefine YamlTree and WeightedYamlTree in terms of PYamlTree (so fill in the 'a' with either () or Float)
-- this will break the types of a few functions maybe, so maybe you'll have to add a few ()'s where the types don't match-- SOAL
-- implement Functor (fmap) for PYamlTree
-- throwOutWeights wTree = fmap (const ()) wTree

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
-- leafDict :: [(String, YamlTree)] -> [String] -> [(String, Int, [String])]
-- leafDict [] _ = []
-- leafDict ((k,v):kvs) path | isLeaf(k,v) = (k, 1, reverse (k:path)) : leafDict kvs path
--                           | otherwise   = leafDict (treeToList v) (k:path) ++ leafDict kvs path
-- leafDict :: [(String, YamlTree)] -> [String] -> [(String, Int, [[String]])]
-- leafDict [] _ = []
-- leafDict ((k, v) : kvs) path
--   | isLeaf (k, v) = (k, 1, concat $ reverse (k : path) : []) : leafDict kvs path
--   | otherwise = leafDict (treeToList v) (k : path) ++ leafDict kvs path
-- leafDict :: [(String, YamlTree)] -> [String] -> [(String, Int, [String])]
-- leafDict [] _ = []
-- leafDict ((k, v) : kvs) path
--   | isLeaf (k, v) = [(k, 1, reverse (k : path))]
--   | otherwise = leafDict (treeToList v) [] ++ leafDict kvs path
type Path = [String]

leafCounts' :: [(String, YamlTree)] -> Path -> Map.Map String [Path]
leafCounts' [] _ = Map.empty
leafCounts' ((k,v) : kvs) path
  | isLeaf (k,v) = Map.insertWith (\x y -> x ++ y) k [reverse (k : path)] (leafCounts' kvs path)
  | otherwise = Map.unionWith (\x y -> x ++ y) (leafCounts' (treeToList v) (k : path)) (leafCounts' kvs path)



leafDict :: [(String, YamlTree)] -> [String] -> [(String, Int, [[String]])]
leafDict [] _ = []
leafDict ((k, v) : kvs) path
  | isLeaf (k, v) = [(k, 1, [reverse (k : path)])] ++ leafDict kvs path
  | otherwise = leafDict (treeToList v) (k : path) ++ leafDict kvs path
--maybe use map(\) instead of this later
--[(leaf1, 1, path1), (leaf2, 1, path2)..]
leafCount :: (String, Int, [String]) -> [(String, Int, [[String]])] -> [(String, Int, [[String]])]
leafCount (k, v, path) kvs =
  case L.find (\(k', _, _) -> k' == k) kvs of
    Just (k', count, paths) -> (k', count + v, paths ++ [path]) : filter (\(k'', _, _) -> k'' /= k) kvs
    Nothing -> (k, v, [path]) : kvs
--[(leaf1, 1, [[path1], [path2]]), (leaf2, 1, path2)..]

leafCounts :: YamlTree -> [(String, [[String]])]
leafCounts (YamlTree []) = []
leafCounts (YamlTree kvs) =
  concatMap
    ( \(k, v) ->
        if isLeaf (k, v) --If the value is a leaf, add a tuple to the result list with the key as the label and a list of paths that lead to the label. The paths are obtained by calling the leafCount function, which recursively counts the occurrence of the label in the tree and returns the paths that lead to it.
          then [(k, [(k : concat paths) | (_, _, paths) <- leafCount (k, 1, []) (leafDict kvs [])])]
          else [(k, [path ++ [k] | path <- concat [ps | (_, ps) <- leafCounts v]])] --If the value is not a leaf, recursively call leafCounts on the value and add the resulting list of tuples to the result list with the key as the label.
    ) kvs --else part means if this is not a leaf then something else is. which should be found recursively with leafCounts until leaf

  -- Check for overlapping labels in the leaf nodes and print warnings
checkOverlappingLabels :: [(String, [[String]])] -> IO ()
checkOverlappingLabels pathsList = do
  let overlappingLabels = L.filter (\(_,paths) -> length paths > 1) pathsList
  mapM_ (\(label, paths) -> printWarning label overlappingLabels) overlappingLabels

isLeaf :: (String, YamlTree) -> Bool
isLeaf (_, YamlTree []) = True
isLeaf _ = False

printWarning :: String -> [(String, [[String]])] -> IO ()
printWarning label pathsList = case L.find (\(l,_) -> l == label) pathsList of
                                  Just (_, paths) -> do
                                    putStrLn $ "WARNING: instrument \"" ++ label ++ "\" occurs in multiple places in hierarchy:"
                                    mapM_ (\path -> putStrLn $ "-- \"" ++ (unwords path) ++ "\"") paths
                                  Nothing -> return ()
----------q8
--Write a function longestPath :: YamlTree -> [String] that finds the longest path in a YamlTree.
--Here, counts is a Map that maps each leaf label to a list of all paths that end in that leaf. paths is a list of all those paths, obtained by concatenating the lists of paths for each leaf. Finally, maximumBy is used to obtain the longest path from paths
longestPath :: YamlTree -> Path
longestPath tree =
  let counts = leafCounts' (treeToList tree) []
      paths = Map.foldr (\p acc -> acc ++ p) [] counts
  in maximumBy (compare `on` length) paths
-- Using equational reasoning, write down a formal proof that length . longestPath = depth . Please specify all definitions you use in the proof and give them a label. Please clearly state any induction hypotheses you use. Please justify each proof step with the label of the definition or with I.H. for an induction hypothesis.
--q9:Now, please implement a variant of the YamlTree datatype that we call a WeightedYamlTree: a YamlTree that further carries a Float for the relative weight of each (proper) subtree (each of which is again a WeightedYamlTree).
data WeightedYamlTree = WeightedYamlTree Float [(String, WeightedYamlTree)] deriving Show
--this would be more appropriate as it allows the weight to be associated with any subtree, not just child nodes. I need to ask this
data WYTree = WYTree [(String, Float, WYTree)] deriving Show
--q10: Define a parameterized datatype data PYamlTree a = ... such that YamlTree and WeightedYamlTree could be reimplemented as special cases of it. Please make PYamlTree an instance of Functor and use fmap to define a function that "throws away the weights" of a WeightedYamlTree to produce the underlying YamlTree.
data PYamlTree a = PYamlTree a [(String, PYamlTree a)] deriving Show
--To make PYamlTree an instance of Functor, we can define the following fmap function:
instance Functor PYamlTree where
  fmap f (PYamlTree x ts) = PYamlTree (f x) [(k, fmap f t) | (k, t) <- ts] --to test construct a tree and fmap (+1) ..
--q11:Define a function find :: String -> YamlTree -> Bool that checks whether a node with a certain String label exists in a YamlTree. Please explain how the laziness of Haskell affects the efficiency of find.

find' :: String -> YamlTree -> Bool
find' key (YamlTree ts) = any (checkKey key) ts || any (find' key . snd) ts
  where
    checkKey k (k', _) = k == k'

--answer the effect of laziness:
-- find' searches the tree as far as necessary to find the node with the
-- specified label. This means that if the label is found early in the tree, the
-- function will terminate quickly, without searching the rest of the tree. On
-- the other hand. Yet, Haskell's lazy (early exit)
-- evaluation allows for efficient searching of large trees, since it only
-- evaluates the parts of the tree that are needed to find the label.
----q12
--q12:Write an interactive (IO) program that does a depth-first traversal of a
--YamlTree that asks the user to specify weights for each subtree to
--interactively convert the original YamlTree into a WeightedYamlTree. It should
--first ask whether the user wants to equal weight subtrees. If so, assign equal
--weights to the child nodes (subtrees) that add up to the weight of the parent
--node (the tree itself). If not, interactively prompt the user to input
--relative weights for the child nodes, which should then be used as the weights
--in the WeightedYamlTree after normalizing them to add up to the weight of the
--parent node. For the case of a single child node, no input should be asked
--from the user as no weighting decision has the be made. The whole tree should
--be assigned weight 1
-- Depth-first traversal of YamlTree to convert it into a WeightedYamlTree


traverseTree :: YamlTree -> WYTree
traverseTree (YamlTree ts) =
  let traverseSubtree (name, subtree) = (name, 1.0, traverseTree' subtree)
  in WYTree [("root", 1.0, WYTree $ map traverseSubtree ts)]

traverseTree' :: YamlTree -> WYTree
traverseTree' (YamlTree ts) =
  let traverseSubtree name subtree = (name, 1.0, traverseTree' subtree)
  in WYTree $ map (\(name, subtree) -> traverseSubtree name subtree) ts

--general normalization for non/equal weight distribution
normalizeWeights :: Float -> WYTree -> WYTree
normalizeWeights parentWeight (WYTree ts) =
  let n = length ts
      userTotalWeight = sum $ map (\(_, w, _) -> w) ts
      calculateAdjustedWeight userGivenWeight = parentWeight * (userGivenWeight / userTotalWeight)
      subtrees = map (\(name, userGivenWeight, t) -> (name, calculateAdjustedWeight userGivenWeight , (normalizeWeights (calculateAdjustedWeight userGivenWeight) t))) ts
  in WYTree subtrees
--test = WYTree [("root",1.0,WYTree [("bonds",1.0,WYTree [("long-bonds",1.0,WYTree [("us-long-bonds",1.0,WYTree [("US10",1.0,WYTree []),("US20",1.0,WYTree []),("US30",1.0,WYTree [])]),("KR10",1.0,WYTree []),("eu-short-bonds",1.0,WYTree [("OAT",1.0,WYTree []),("BTP",1.0,WYTree [])])]),("short-bonds",1.0,WYTree [("us-short-bonds",1.0,WYTree [("US2",1.0,WYTree []),("US3",1.0,WYTree []),("US5",1.0,WYTree [])]),("eu-short-bonds",1.0,WYTree [("SHATZ",1.0,WYTree []),("BTP3",1.0,WYTree [])]),("KR3",1.0,WYTree [])]),("STIRs",1.0,WYTree [("EURIBOR",1.0,WYTree []),("FED",1.0,WYTree [])])]),("energies",1.0,WYTree [("oil",1.0,WYTree [("CRUDE_W_mini",1.0,WYTree []),("BRENT-LAST",1.0,WYTree []),("HEATOIL",1.0,WYTree []),("GASOILINE",1.0,WYTree [])]),("gas",1.0,WYTree [("GAS_US_mini",1.0,WYTree []),("GAS-LAST",1.0,WYTree [])])]),("currencies",1.0,WYTree [("usd-fx",1.0,WYTree [("INR-SGX",1.0,WYTree []),("EUR",1.0,WYTree []),("GBP",1.0,WYTree [])]),("eur-fx",1.0,WYTree [("EUR",1.0,WYTree []),("EURGBP",1.0,WYTree [])])]),("equities",1.0,WYTree [("us-equities",1.0,WYTree [("SP500_mini",1.0,WYTree []),("R1000",1.0,WYTree [])]),("eu-equities",1.0,WYTree [("AEX",1.0,WYTree []),("DAX",1.0,WYTree [])]),("asia-equities",1.0,WYTree [("KOSPI",1.0,WYTree [])])]),("vol",1.0,WYTree [("VIX",1.0,WYTree []),("V2X",1.0,WYTree []),("VNKI",1.0,WYTree [])])])]
userWeightRequest :: WYTree -> IO WYTree
userWeightRequest t@(WYTree ts) = do
  --putStrLn $ "Child nodes of " ++ show t ++ ": " ++ show ts
  putStrLn $ "n:    " ++ show (length ts)
  equal <- isEqualWeight
  if equal
    then return $ normalizeWeights 1.0 t
    --change this part
  else do
      let n = length ts
      weights <- promptForWeights n
      return $ weightCalculator' weights t

weightCalculator' :: [Float] -> WYTree -> WYTree
weightCalculator' weights (WYTree ts) = WYTree $ zipWith applyWeight weights ts
  where
    applyWeight w (name, _, t) = (name, w, weightCalculator' weights t)

--When the user inputs the weights for the child nodes, they are essentially
--indicating how important each child node is relative to the other child nodes.
--The sum of the weights should equal the weight of the parent, as the total
--weight should be distributed among the child nodes

--here I should have recursion. after asking 2 quesitons: 1- what is the weight of this node 2- do you want equal weights for this? if not, then I recurse
promptForWeights :: Int -> IO [Float]
promptForWeights n = do
  putStrLn "Please enter the weight for each subtree:"
  replicateM n getFloat
    where
      getFloat = do
        putStr "> "
        hFlush stdout
        readLn

isEqualWeight :: IO Bool
isEqualWeight = do
  putStrLn "Do all subtrees have equal weights? (y/n)"
  ans <- getLine
  case ans of
    "y" -> return True
    "n" -> return False
    _ -> do
      putStrLn "Invalid answer. Please answer y/n."
      isEqualWeight
--q13

--q13:Now, please write a pretty printer for WeightedYamlTrees that produces
--"instruments_hierarchy_weighted.yaml" for the YamlTree corresponding to
--"instruments_hierarchy_regular.yaml", for example. (Again, no hard-coding!)

uglyPrint' :: WYTree -> String
uglyPrint' (WYTree x) = concatMap (\y -> uglyPrint y [("instruments", 1.0)]) x
--find the function to pretty print the float!!!
uglyPrint :: (String, Float, WYTree) -> [(String, Float)] -> String
uglyPrint (name, weight, (WYTree ts)) parents = let
  depth = length parents
  portionPerParent = show weight ++ "/" ++ "instruments" -- undefined--go through parent list and do calculation and then printing
  indentation = concat $ replicate depth "    "
  isLeaf = length ts == 0
  comment = if isLeaf
    then "  "
    else "##"
  in
    comment ++ indentation ++ name ++ ": " ++ portionPerParent ++ "\n" ++
      concatMap (\tree -> uglyPrint tree ((name, weight) : parents)) ts

--------------------------------------------------------------------------------
main :: IO ()
main = do
    yamlValue <- parse "instruments-hierarchy.yaml"
    let yamlTree' = convertToYAMLTree yamlValue
    let yamlTree = postProcessYamlTree yamlTree'
    -- print'<- userWeightRequest $ equalWYTree $ traverseTree yamlTree
    -- print print'
    putStr $ uglyPrint' $ normalizeWeights 1.0 $ traverseTree' yamlTree
    --print print'

    --print (yamlTree)
    -- --let yamlTree = YamlTree [("a", YamlTree [("b", YamlTree [("c", YamlTree []), ("d", YamlTree [])]), ("e", YamlTree [])]), ("f", YamlTree [])]
    -- -- print (treeToList yamlTree)
    --------q7 test
    -- let leafCountsList = leafCounts' (treeToList yamlTree) []
    -- print leafCountsList
    -- checkOverlappingLabels $ Map.toList leafCountsList
--------q8
    -- print(longestPath yamlTree)
    -- print (find' "us-long-bonds" yamlTree)
    -- output <- traverseTree yamlTree
    -- print output
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

--------
-- a = WeightedYamlTree 1.0 [("bonds",WeightedYamlTree 0.5 [("long-bonds",
--                                                           WeightedYamlTree 0.5 [("us-long-bonds",
--                                                                                  WeightedYamlTree 0.25 [("US10",WeightedYamlTree 0.0833 []),
--                                                                                                         ("US20",WeightedYamlTree 0.0833 []),
--                                                                                                         ("US30",WeightedYamlTree 0.0833 [])]),
--                                                                                 ("eu-long-bonds",WeightedYamlTree 0.25 [])])]),
--                             ("energies",
--                             WeightedYamlTree 0.5 [("oil",
--                                                     WeightedYamlTree 0.16 [("CRUDE_W_mini",WeightedYamlTree 0.16 [])]),
--                                                   ("gas",
--                                                     WeightedYamlTree 0.16 [("GAS_US_mini",WeightedYamlTree 0.08 []),
--                                                                           ("GAS-LAST",WeightedYamlTree 0.08 [])]),
--                                                   ("KR3",WeightedYamlTree 0.16 [])]
--                             )]