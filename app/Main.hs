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
import Text.Printf
import Data.List as L (maximumBy, groupBy, sortOn)
import Data.Ord (comparing)
import Control.Monad as MO (when)
import qualified Data.Char as C
import qualified Data.Yaml as Y
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
import Text.Read (readMaybe)
import Debug.Trace (trace)
import Data.List.Split (splitOn)


data YamlTree = YamlTree [(String, YamlTree)]
    deriving (Ord, Eq, Show, Generic)

--parse function reads the YAML file using the readYamlFile function from the yaml package and returns a Y.YamlValue representing the parsed YAML content
parse :: FilePath -> IO Y.YamlValue
parse path = do
  content <- BS.readFile path

  -- Case 1: Empty file
  if BS.null content
    then return (Y.Mapping [] Nothing)

  -- Case 2: File contains only a single colon
  else if (BS.strip content == BS.pack ":")
    then return (Y.Mapping [(T.empty, Y.Mapping [] Nothing)] Nothing)

  -- Case 3: File ends with a colon then Yaml mapping with single key-val created
  else if (BS.isSuffixOf (BS.pack ":") (BS.strip content))
    then return $ Y.Mapping [(TE.decodeUtf8 $ BS.init (BS.strip content), Y.Mapping [] Nothing)] Nothing

  -- Default case: Read the YAML file
  else Y.readYamlFile path

--this is the base of parsing but I realized I should handle cases separatly
-- parse :: FilePath -> IO Y.YamlValue
-- parse = Y.readYamlFile

convertToYAMLTree :: Y.YamlValue -> YamlTree
convertToYAMLTree (Y.Mapping list _) = -- If YamlValue is a mapping
  YamlTree (L.map (\(xs,ys) -> (T.unpack xs, convertToYAMLTree ys)) list) -- Convert each key-value pair to a tuple of strings and recursively convert the value to a YamlTree

convertToYAMLTree (Y.Scalar xs _ _ _) = -- If YamlValue is a scalar
  if BS.unpack xs == "" -- Check if the scalar value is empty
    then YamlTree [] -- If empty, create an empty YamlTree
    else YamlTree [(BS.unpack xs, YamlTree [])] -- leaf node

convertToYAMLTree (Y.Sequence list _) = -- If YamlValue is a sequence (list)
  error "Yaml lists are not supported" -- Error: YAML lists are not supported in this implementation

convertToYAMLTree (Y.Alias _) = -- If YamlValue is an alias
  error "Aliases are not supported" -- Error: Aliases are not supported

--the function writes the YAML string to the specified file  
yamlTreeToYamlFile :: FilePath -> YamlTree -> IO ()
yamlTreeToYamlFile filePath tree = do
  let yamlText = yamlTreeToString tree -- Convert the YamlTree to a YAML string representation
  writeFile filePath yamlText -- Write the YAML string to the specified file path

--the function converts a YamlTree to a String representation with indentation
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
                        else yamlTreeToStringIndented (indentLevel + 1) value --recursive handling of values

indent :: Int -> String -> String
indent n line
  | null line = ""
  | otherwise = replicate n ' ' ++ line ++ "\n"
                        
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
--The prop_parser_prettyprinter function takes a YamlTree as input, converts it to text using yamlTreeToString, and then parses the text using parseYaml. If the parse is successful and the resulting tree is equal to the original input tree, the function returns True. Otherwise, it returns False.

prop_my_io_action :: YamlTree -> Q.Property
prop_my_io_action yamltree =
  Q.monadicIO $ do
    let fileName = "output2.yaml"
    run $ yamlTreeToYamlFile fileName yamltree
    yamlValue <- run $ parse fileName
    let result = convertToYAMLTree yamlValue
    Q.assert (result == yamltree)
--Since Y.YamlValue is already an instance of FromJSON, this default implementation is sufficient to define the instance
instance FromJSON Y.YamlValue where
    parseJSON = parseJSON


-------------nr.1 version Arbitrary instance for YamlTree-----
--defines an Arbitrary instance for YamlTree using QuickCheck. The arbitraryYamlTree function generates an arbitrary YamlTree with a maximum depth of 8
instance Q.Arbitrary YamlTree where
    arbitrary = arbitraryYamlTree 8
    
arbitraryYamlTree :: Int -> Gen YamlTree
arbitraryYamlTree 0 = return $ YamlTree [] --depth 0 returns empty YamlTree
arbitraryYamlTree n = do
    numKeys <- Q.choose (1, 3)
    keysAndVals <- replicateM numKeys arbitraryKeyAndVal --generates a list of random key-val pairs
    return $ YamlTree (mergeTuples keysAndVals)
    where
        arbitraryKeyAndVal = do
            key <- arbitraryKey
            val <- if n == 1
                       then return $ YamlTree []
                       else arbitraryYamlTree (n-1) --recursively generate a YamlTree as the value
            return (key, val)
        arbitraryKey = do
            key <- Q.elements ["Spring", "Summer", "autumn","ExampleSeason", "WINTER", "love", "HAPPY", "go", "LUCKY"]
            --key <- Q.listOf $ Q.elements validChars
            return $ if null key then "default_key" else key

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

---------q5
--Check whether a given YamlTree is regular
isRegular :: YamlTree -> Bool
isRegular (YamlTree []) = True
isRegular (YamlTree kvs) =
  let depths = map (depthi . snd) kvs
      isRegularSubtree (_, subtree) = isRegular subtree && depthi subtree == maximum depths
  in all isRegularSubtree kvs

--QuickCheck property to test whether a given YamlTree is regular
prop_isRegular :: YamlTree -> Bool
prop_isRegular = isRegular . regularize
-----------q6
--Write a function that "regularizes" a YamlTree into a regular one by copying levels in the hierarchy. Please ensure that this function regularizes the YamlTree implemented by "instruments-hierarchy.yaml" to the one implemented by "instruments-hierarchy-regular.yaml". (Again, we want a generally applicable solution. No hard-coding of the example allowed.)
--the idea is to make all branches have the same depth even if it requires adding empty levels to shallower branches
regularize :: YamlTree -> YamlTree
regularize yamlTree@(YamlTree subTrees) =
  let maxDepth = depthi yamlTree
      newTrees = regularizeSubTrees maxDepth subTrees
  in YamlTree newTrees
--This function recursively adjusts the depth of each sub-tree to match the desired maxDepth.
regularizeSubTrees :: Int -> [(String, YamlTree)] -> [(String, YamlTree)]
regularizeSubTrees 0 [] = []
regularizeSubTrees 0 _  = error "Cannot shrink tree"
regularizeSubTrees depth trees = map (regularizeSubTree (depth -1)) trees --mapping regularizeSubTree over the list of subtrees
--function takes a depth and a tuple representing a key-value pair in a sub-tree. It checks if the current depth of the value (val) matches the desired depth. If it does not match, it calls the addLevel function to add empty levels to the val to match the desired depth. Otherwise unchanged
regularizeSubTree :: Int -> (String, YamlTree) -> (String, YamlTree)
regularizeSubTree depth a@(key, val) =
    let (key', YamlTree val') = if depthi val /= depth then addLevel (depth - depthi val) (key, val)
                                     else a-- make if part a binding and call it in regularizeSubTrees
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

type Path = [String]
--LeafCounts' keeps track of how many times each leaf label appears and the different paths that lead to those labels 
leafCounts' :: [(String, YamlTree)] -> Path -> Map.Map String [Path]
leafCounts' [] _ = Map.empty
leafCounts' ((k,v) : kvs) path
  | isLeaf (k,v) = Map.insertWith (\x y -> x ++ y) k [reverse (k : path)] (leafCounts' kvs path) --if it is a leaf it constructs a new key-value pair in the Map. value is a list containing reversed path
  | otherwise = Map.unionWith (\x y -> x ++ y) (leafCounts' (treeToList v) (k : path)) (leafCounts' kvs path) --otherwise recurse on leafCounts' to process remaining tuple Map.unionWith is used to merge the resulting Map. it concatenates the list of paths for common keys

-- checkOverlappingLables's purpose is to identify any leaf labels that occur in multiple places and showing different paths they occur
checkOverlappingLabels :: [(String, [[String]])] -> IO ()
checkOverlappingLabels pathsList = do
  let overlappingLabels = L.filter (\(_,paths) -> length paths > 1) pathsList
  mapM_ (\(label, paths) -> printWarning label overlappingLabels) overlappingLabels

isLeaf :: (String, YamlTree) -> Bool
isLeaf (_, YamlTree []) = True
isLeaf _ = False
--printWarning takes two arguments: label, which is the leaf label for which the warning is generated, and pathsList, which is a list of tuples where the first element is a label of a leaf node, and the second element is a list of paths to that leaf node
printWarning :: String -> [(String, [[String]])] -> IO ()
printWarning label pathsList = case L.find (\(l,_) -> l == label) pathsList of
                                  Just (_, paths) -> do
                                    putStrLn $ "WARNING: instrument \"" ++ label ++ "\" occurs in multiple places in hierarchy:"
                                    mapM_ (\path -> putStrLn $ " \"" ++ (unwords path) ++ "\"") paths --Each path is converted to a string using unwords to concatenate the elements with spaces, and it is enclosed in double quotes (-- "path")
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
--it applies the function f to the value x, creating a new transformed value f x.
--then, it recursively applies fmap f to each child node in the list ts. This means that it calls fmap f on each child node and obtains a new transformed child node.
instance Functor PYamlTree where
  fmap f (PYamlTree x ts) = PYamlTree (f x) [(k, fmap f t) | (k, t) <- ts] 
--q11:Define a function find :: String -> YamlTree -> Bool that checks whether a node with a certain String label exists in a YamlTree. Please explain how the laziness of Haskell affects the efficiency of find.

find' :: String -> YamlTree -> Bool
find' key (YamlTree ts) = any (checkKey key) ts || any (find' key . snd) ts
  where
    checkKey k (k', _) = k == k'

----------answer of the effect of laziness:
-- find' searches the tree as far as necessary to find the node with the
-- specified label. This means that if the label is found early in the tree, the
-- function will terminate quickly, without searching the rest of the tree(early exit)
-- minimum evaluation allows for efficient searching of large trees, since it only
-- evaluates the parts of the tree that are needed to find the label.
------------------------------------------------------------------------------------
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
-- my approach: Depth-first traversal of YamlTree and convert it into a WeightedYamlTree

userWeightRequest :: YamlTree -> IO WYTree
userWeightRequest yamlTree@(YamlTree cs) = normalizeWeights 1.0 yamlTree


promptForWeights :: [String] -> IO [Float]
promptForWeights parents = do
 -- putStrLn $ "Please specify relative weight of each parent:"
  getFloats parents []
    where
      getFloats [] accum = do --If the list of parents is empty, it means all weights have been collected
        let totalWeight = sum accum
        if totalWeight > 0
          then do
            let normalized = map (/ totalWeight) accum
            let normalizedStr = L.intercalate " : " (map (printf "%.2f") normalized)
            putStrLn $ "Your chosen weights sum to \"" ++ show totalWeight ++ "\". Normalizing to \"1\" gives relative weights of \"" ++ normalizedStr ++ "\" for \"" ++ L.intercalate " : " parents ++ "\"!"
            return normalized
          else do
            putStrLn "Please specify at least one positive weight for the parents!"
            getFloats parents []
      getFloats (p:ps) accum = do
        putStr $ "Please specify relative weight of \"" ++ p ++ "\"! \n> "
        hFlush stdout
        input <- getLine
        let parsed = readMaybe input :: Maybe Float
        case parsed of
          Just w -> if w > 0
                      then getFloats ps (w:accum)
                      else do
                        putStrLn "Please specify a positive weight for the parent!"
                        getFloats (p:ps) accum
          Nothing -> do
            putStrLn "Please specify a valid number for the weight of the parent!"
            getFloats (p:ps) accum


isEqualWeight :: [String] -> IO Bool
isEqualWeight parents = do
  putStrLn " "
  putStrLn $ "Equal weight " ++ L.intercalate ": " parents ++ " ? (Type \"no\" or \"NO\" or \"n\" or \"N\" to give custom weights. Any other input will result in equal weights.)"
  putStr "> "
  hFlush stdout
  ans <- getLine
  let ans' = map C.toLower ans
  case ans' of
    "n" -> return False
    "no" -> return False
    _ -> return True


normalizeWeights :: Float -> YamlTree -> IO WYTree
normalizeWeights _ (YamlTree []) = return (WYTree [])
normalizeWeights x (YamlTree [(y, YamlTree [])]) = return (WYTree [(y, x, WYTree [])])
normalizeWeights parentWeight (YamlTree cs) =
  if length cs == 1
    then do
      putStrLn $ "Category \"" ++ (fst $ head cs) ++ "\" only has a single option, so no weighting decision to make!"
      return $ WYTree [(fst $ head cs, parentWeight, WYTree [])]
      let treeStrings = map (map fst . treeToList . snd) cs
      subtrees <- mapM (\(name, yamlTree) -> normalizeWeights parentWeight yamlTree) cs
      return $ WYTree $ zipWith (\((n, _), ts) t -> (n, parentWeight, t)) (zip cs treeStrings) subtrees
  else do
    equal <- isEqualWeight (map fst cs)
    if equal
      then do
        putStrLn $ "Equal weighting \"" ++ L.intercalate " : " (map fst cs) ++ "\"!"
        let subTrees = map snd cs
        let treeStrings = map (map fst . treeToList . snd) cs
        let weight = parentWeight / fromIntegral (length cs)
        subtrees <- mapM (\(name, yamlTree) -> normalizeWeights weight yamlTree) cs
        return $ WYTree $ zipWith (\((n, _), ts) t -> (n, weight, t)) (zip cs treeStrings) subtrees
    else do
      putStrLn $ "Not equal weighting \"" ++ L.intercalate " : " (map fst cs) ++ "\"!"
      weights <- promptForWeights (map fst cs)
      let treeStrings = map (map fst . treeToList . snd) cs
      let totalWeight = sum weights
      let normalized = map (\x -> (x / totalWeight) * parentWeight) weights
      subtrees <- mapM (\((name, yamlTree), weight) -> do
        wyTree <- normalizeWeights weight yamlTree
        return (name, weight, wyTree)) (zip cs normalized)
      return (WYTree subtrees)
--12. code is wrong: you are missing a reverse; also leaf weights do not work; have you tested this?

--q13:Now, please write a pretty printer for WeightedYamlTrees that produces
--"instruments_hierarchy_weighted.yaml" for the YamlTree corresponding to
--"instruments_hierarchy_regular.yaml", for example. (Again, no hard-coding!)
----e.g.##            us-long-bonds: 0.022/instruments - 0.11/bonds - 0.33/long-bonds



wyTreePrint' :: WYTree -> String
wyTreePrint' (WYTree x) = "instrument_weights:\n" ++ concatMap (\y -> wyTreePrint y []) x

wyTreePrint :: (String, Float, WYTree) -> [(String, Float)] -> String
wyTreePrint (name, weight, (WYTree ts)) parents =
  let
    depth = length parents
    portionPerParent = replicatePortions parents weight
    indentation = concat (replicate depth "    ")
    hyphen = depth == 0
    isLeaf = length ts == 0
    comment = if isLeaf then "      " else "##    "
    nodeValue = if isLeaf
      then formatFloat weight ++ " ##"
      else formatFloat weight ++ "/instruments "
    delHyphen = if hyphen || isLeaf then " " else "- "  
  in
    comment ++ indentation ++ name ++ ": " ++ nodeValue ++ delHyphen ++ portionPerParent ++ "\n" ++
    concatMap (\tr -> wyTreePrint tr (parents ++ [(name, weight)])) ts

formatFloat :: Float -> String
formatFloat f = printf "%.2f" f


portions :: [(String, Float)] -> Float -> [String]
portions [] _ = []
portions ((parent, parentWeight):rest) weight =
  let
    formattedWeight = formatFloat (weight / parentWeight)
    result = formattedWeight ++ "/" ++ parent
  in
    result : portions rest weight

replicatePortions :: [(String, Float)] -> Float -> String
replicatePortions ps w = L.intercalate " - " (portions ps w)

--instruments-hierarchy.yaml 
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
    yamlValueInput <- parse path
    let yamlTree' = convertToYAMLTree yamlValueInput
    let yamlTree = postProcessYamlTree yamlTree'
    print yamlTree
-- - regularizer into a regular YamlTree, producing a YamlTree that would pretty print to "instruments-hierarchy-regular.yaml" for our particular example
    putStrLn " "
    let regularized = regularize yamlTree
    yamlTreeToYamlFile "instruments-regular.yaml" regularized
    putStrLn " "
    --putStrLn $ "it is " ++ show (isRegular regularized) ++ " that the output YamlTree is regularize!!!" ++ " with the depth: " ++ show (depthi regularized)
    putStrLn " "
-- - checks for overlapping leaf labels and prints warnings
    let leafCountsList = leafCounts' (treeToList yamlTree) []
    checkOverlappingLabels $ Map.toList leafCountsList
--and (interactively) converts the regular YamlTree into a WeightedYamlTree, in such a way that our example would generate the interaction in "instruments-interaction.log"
    putStrLn " "
    wyTree <- userWeightRequest regularized
    --print wyTree
--- pretty prints the resulting WeightedYamlTree to a file (e.g. "/home/henkie/generated_configs/instrument_hierarchy_weighted.yaml")
    let st = wyTreePrint' wyTree
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
    ---here I tested my quickCheck tests
    -- Q.quickCheck prop_isRegular
   --Q.quickCheck (prop_my_io_action) --cabal run yamltree= if u want to run the application  / cabal test= if u want to run the test
     
   
   
   
   
   
   
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
