{-# LANGUAGE DeriveGeneric #-}--to derive Generic instances


module Main where
import qualified Data.List as L
import Data.List (isPrefixOf)
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
import Data.List (maximumBy)
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
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Yaml.Pretty as PP
import GHC.Generics
import qualified Data.Map.Strict as Map
import qualified Data.Map as Map

--We (recursively) call a YamlTree regular if all of its (proper) subtrees have the same depth and are regular themselves. Please implement a QuickCheck test that checks whether a YamlTree is regular.


data YamlTree = YamlTree [(String, YamlTree)]
    deriving (Eq, Show, Generic)
----not sure if needed!!
-- instance Eq YamlTree where
--     YamlTree [] == YamlTree [] = True
--     YamlTree [(k1, v1)] == YamlTree [(k2, v2)] = k1 == k2 && v1 == v2
--     YamlTree ((k1, v1):rest1) == YamlTree ((k2, v2):rest2) =
--         k1 == k2 && v1 == v2 && YamlTree rest1 == YamlTree rest2
--     _ == _ = False    
--parse function reads the YAML file using the readYamlFile function from the yaml package and returns a Y.YamlValue representing the parsed YAML content
--with this implementation only YamlTree [("ExampleKey2",YamlTree [("ExampleKey2",YamlTree []),("ExampleKey2",YamlTree [])])] doesn't work!!!
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
convertToYAMLTree (Y.Scalar xs _ _ _) = YamlTree [(BS.unpack xs, YamlTree [])] --when we reach a Leaf
--convertToYAMLTree (Y.Sequence list _) = YamlTree $ L.map (\x -> ("-", convertToYAMLTree x)) $ V.toList $ V.fromList list --In this implementation the list argument is first converted to a Vector using V.fromList, and then V.toList is used to convert the Vector to a list.
convertToYAMLTree _ = YamlTree []

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

--The instance FromJSON Y.YamlValue is defined simply as parseJSON = parseJSON, which means that it will use the default implementation of parseJSON provided by FromJSON. Since YamlValue is already an instance of FromJSON, this is sufficient to define the instance.
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

instance Arbitrary Y.YamlValue where
    arbitrary = Q.oneof [arbitraryMapping, arbitraryScalar]
    
arbitraryMapping :: Gen Y.YamlValue
arbitraryMapping = Y.Mapping <$> arbitraryKeyVals <*> pure Nothing
    where 
        arbitraryKeyVals = Q.listOf1 arbitraryKV
        arbitraryKV = (,) <$> arbitraryText <*> arbitrary
arbitraryScalar :: Q.Gen Y.YamlValue
arbitraryScalar = Y.Scalar 
    <$> arbitraryByteString 
    <*> arbitraryTag 
    <*> arbitraryStyle 
    <*> pure Nothing
    where
        arbitraryByteString = TE.encodeUtf8 . T.pack <$> Q.listOf1 arbitraryChar
        arbitraryTag = pure YL.StrTag -- only generate string tags for simplicity
        arbitraryStyle = Q.elements [YL.Plain, YL.SingleQuoted, YL.DoubleQuoted, YL.Literal]
        arbitraryChar = Q.elements ['a'..'z']
arbitraryText :: Q.Gen T.Text
arbitraryText = T.pack <$> Q.listOf1 arbitraryChar

arbitraryChar :: Q.Gen Char
arbitraryChar = Q.elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

instance Q.Arbitrary YL.Style where
    arbitrary = Q.elements [YL.Plain, YL.SingleQuoted, YL.DoubleQuoted, YL.Literal]      
-------q4-----------
--finding the maximum depth of any sub-tree rooted at a value in kvs
depth :: YamlTree -> Int
depth (YamlTree []) = 1 -- base case, an empty YamlTree has depth 1
depth (YamlTree kvs) = 1 + maximum (map (depth . snd) kvs)  

minDepth :: YamlTree -> Int
minDepth (YamlTree []) = 1
minDepth (YamlTree kvs) = 1 + minimum (map (depth . snd) kvs) 
---------q5
-- | Check whether a given YamlTree is regular
isRegular :: YamlTree -> Bool
isRegular (YamlTree []) = True
isRegular (YamlTree kvs) =
  let depths = map (depth . snd) kvs
      isRegularSubtree (_, subtree) = isRegular subtree && depth subtree == maximum depths
  in all isRegularSubtree kvs

-- | QuickCheck property to test whether a given YamlTree is regular
prop_isRegular :: YamlTree -> Bool
prop_isRegular = isRegular
-----------q6
--Write a function that "regularizes" a YamlTree into a regular one by copying levels in the hierarchy. Please ensure that this function regularizes the YamlTree implemented by "instruments-hierarchy.yaml" to the one implemented by "instruments-hierarchy-regular.yaml". (Again, we want a generally applicable solution. No hard-coding of the example allowed.)
-- data YamlTree = YamlTree [(String, YamlTree)]
regularize :: YamlTree -> YamlTree
regularize yamlTree@(YamlTree treeList) =
  if isRegular yamlTree then yamlTree else if isRegular yamlTree
    then yamlTree
    else YamlTree $ concatMap (regularizeSubTree desiredDepth (map (\(k, v) -> (k, v)) treeList)) trimmedTrees ++ added
        where
            subtrees = map snd treeList
            maxDepth = maximum $ map depth subtrees
            (trimmedTrees, added) = L.partition (\t -> depth t == maxDepth) subtrees
            desiredDepth = min (maxDepth + 1) (maximum $ map depth subtrees)

regularizeSubTree :: Int -> [(String, YamlTree)] -> (String, YamlTree) -> [(String, YamlTree)]
regularizeSubTree desiredDepth siblings (key, value) =
  let fullKey = key
      fullKeyExists (k, _) = k == fullKey
      depthDiff = maximum [1, desiredDepth - depth value]
      repeatedValue = generateRepeatedValue depthDiff
      subTreeList = case L.find fullKeyExists siblings of
        Just (_, subTree) -> concatMap (\t -> regularizeSubTree (desiredDepth - 1) (map snd (flattenSubTree siblings)) t) (flattenSubTree subTree)
        Nothing -> [(fullKey, repeatedValue)]
  in [(fullKey, value)] ++ subTreeList

-- flattenSubTree :: [(String, YamlTree)] -> [(String, YamlTree)]
-- flattenSubTree = concatMap flatten
--   where
--     flatten (k, YamlTree kvs) = (k, YamlTree kvs) : flattenSubTree kvs
flattenSubTree :: YamlTree -> [(String, YamlTree)]
flattenSubTree (YamlTree treeList) =
    concatMap flattenSubTree' treeList
    where
    flattenSubTree' (key, value) =
        (key, value) : flattenSubTree value

        
generateRepeatedValue :: Int -> YamlTree
generateRepeatedValue depthDiff = YamlTree $ replicate depthDiff ("value", YamlTree [])

-- regularize :: YamlTree -> YamlTree
-- regularize yamlTree@(YamlTree treeList) =
--   if isRegular yamlTree
--     then yamlTree
--     else YamlTree $ concatMap (regularizeSubTree desiredDepth treeList trimmedTrees) trimmedTrees ++ added
--   where
--     subtrees = map snd treeList
--     desiredDepth = min (maximum $ map depth subtrees + 1) (maximum $ map depth subtrees)
--     (trimmedTrees, added) = L.partition (\t -> depth t == desiredDepth - 1) treeList

-- regularizeSubTree :: Int -> [(String, YamlTree)] -> [(String, YamlTree)] -> (String, YamlTree) -> [(String, YamlTree)]
-- regularizeSubTree desiredDepth siblings trimmedTrees (key, value) =
--   let fullKey = key
--       fullKeyExists (k, _) = k == fullKey
--       depthDiff = maximum [1, desiredDepth - depth value]
--       repeatedValue = generateRepeatedValue depthDiff
--       subTreeList = case L.find fullKeyExists siblings of
--         Just (_, subTree) -> concatMap (regularizeSubTree (desiredDepth - 1) (flattenSubTree siblings) (flattenSubTree trimmedTrees)) (flattenSubTree subTree)
--         Nothing -> [(fullKey, repeatedValue)]
--   in [(fullKey, value)] ++ subTreeList

-- flattenSubTree :: [(String, YamlTree)] -> [(String, YamlTree)]
-- flattenSubTree = concatMap flatten
--   where
--     flatten (k, YamlTree kvs) = (k, YamlTree kvs) : flattenSubTree kvs


-- regularizeSubTree :: Int -> [(String, YamlTree)] -> [(String, YamlTree)] -> (String, YamlTree) -> [(String, YamlTree)]
-- regularizeSubTree desiredDepth siblings trimmedTrees (key, value) =
--   let fullKey = key
--       fullKeyExists (k, _) = k == fullKey
--       depthDiff = maximum [1, desiredDepth - depth value]
--       repeatedValue = generateRepeatedValue depthDiff
--       subTreeList = case L.find fullKeyExists siblings of
--         Just (_, subTree) -> concatMap (regularizeSubTree (desiredDepth - 1) (flattenSubTree siblings)) (flattenSubTree subTree)
--         Nothing -> [(fullKey, repeatedValue)]
--   in [(fullKey, value)] ++ subTreeList

-- regularizeSubTree :: Int -> [(String, YamlTree)] -> (String, YamlTree) -> [(String, YamlTree)]
-- regularizeSubTree desiredDepth siblings (key, value) =
--   let fullKey = key
--       fullKeyExists (k, _) = k == fullKey
--       depthDiff = max 0 (desiredDepth - depth value)
--       repeatedValue = generateRepeatedValue depthDiff (YamlTree [])
--       subTreeList = case L.find fullKeyExists siblings of
--         Just (_, subTree) -> concatMap (regularizeSubTree (desiredDepth - 1) (flattenSubTree siblings)) (flattenSubTree subTree)
--         Nothing -> [(fullKey, repeatedValue)]
--   in [(fullKey, value)] ++ subTreeList

-- flattenSubTree :: [(String, YamlTree)] -> [(String, YamlTree)]
-- flattenSubTree treeList = flattenTreeList treeList []
--   where
--     flattenTreeList [] acc = acc
--     flattenTreeList ((k, YamlTree kvs):rest) acc =
--       flattenTreeList (kvs ++ rest) ((k, YamlTree kvs) : acc)

-- generateRepeatedValue :: Int -> YamlTree -> YamlTree
-- generateRepeatedValue depthDiff value = YamlTree $ replicate depthDiff ("value", YamlTree [])

-- regularize :: YamlTree -> YamlTree
-- regularize yamlTree@(YamlTree treeList) =
--   if isRegular yamlTree
--     then yamlTree
--     else YamlTree $ concatMap (regularizeSubTree (desiredDepth - 1) treeList) trimmedTrees ++ added
--   where
--     subtrees = map snd treeList
--     maxDepth = maximum $ map depth subtrees
--     (trimmedTrees, added) = L.partition (\t -> depth t == maxDepth) treeList
--     desiredDepth = min (maxDepth + 1) (maximum $ map depth subtrees)

-- regularizeSubTree :: Int -> [(String, YamlTree)] -> (String, YamlTree) -> [(String, YamlTree)]
-- regularizeSubTree desiredDepth siblings (key, value) =
--   let fullKey = key
--       fullKeyExists (k, _) = k == fullKey
--       depthDiff = maximum [1, desiredDepth - depth value]
--       repeatedValue = generateRepeatedValue depthDiff
--       subTreeList = case L.find fullKeyExists siblings of
--         Just (_, subTree) -> concatMap (regularizeSubTree (desiredDepth - 1) (flattenSubTree siblings)) (flattenSubTree subTree)
--         Nothing -> [(fullKey, repeatedValue)]
--   in [(fullKey, value)] ++ subTreeList

-- flattenSubTree :: [(String, YamlTree)] -> [(String, YamlTree)]
-- flattenSubTree = concatMap flatten
--   where
--     flatten (k, YamlTree kvs) = (k, YamlTree kvs) : flattenSubTree kvs

-- generateRepeatedValue :: Int -> YamlTree
-- generateRepeatedValue depthDiff = YamlTree $ replicate depthDiff ("value", YamlTree [])

-- regularize :: YamlTree -> YamlTree
-- regularize yamlTree@(YamlTree treeList) =
--   if isRegular yamlTree
--     then yamlTree
--     else YamlTree $ concatMap (regularizeSubTree (desiredDepth - 1)) trimmedTrees ++ added
--   where
--     subtrees = map snd treeList
--     maxDepth = maximum $ map depth subtrees
--     (trimmedTrees, added) = L.partition (\t -> depth t == maxDepth) treeList
--     desiredDepth = min (maxDepth + 1) (maximum $ map depth subtrees)

-- regularizeSubTree :: Int -> (String, YamlTree) -> [(String, YamlTree)]
-- regularizeSubTree desiredDepth (key, value) =
--   let fullKey = key
--       fullKeyExists (k, _) = k == fullKey
--       depthDiff = maximum [1, desiredDepth - depth value]
--       repeatedValue = generateRepeatedValue depthDiff
--       subTreeList = case L.find fullKeyExists (flattenSubTree value) of
--         Just (_, subTree) -> concatMap (regularizeSubTree (desiredDepth - 1)) (flattenSubTree subTree)
--         Nothing -> [(fullKey, repeatedValue)]
--   in [(fullKey, value)] ++ subTreeList

-- flattenSubTree :: YamlTree -> [(String, YamlTree)]
-- flattenSubTree (YamlTree []) = []
-- flattenSubTree (YamlTree ((key, value):xs)) =
--   (key, value) : flattenSubTree value ++ flattenSubTree (YamlTree xs)

-- generateRepeatedValue :: Int -> YamlTree
-- generateRepeatedValue depthDiff = YamlTree $ replicate depthDiff ("value", YamlTree [])

-- regularize :: YamlTree -> YamlTree
-- regularize yamlTree@(YamlTree treeList) =
--   if isRegular yamlTree
--     then yamlTree
--     else YamlTree $ concatMap (regularizeSubTree (desiredDepth - 1) treeList) trimmedTrees ++ added
--   where
--     subtrees = map snd treeList
--     maxDepth = maximum $ map depth subtrees
--     (trimmedTrees, added) = L.partition (\(k, t) -> depth t == maxDepth) treeList
--     desiredDepth = min (maxDepth + 1) (maximum $ map depth subtrees)

-- regularizeSubTree :: Int -> [(String, YamlTree)] -> (String, YamlTree) -> [(String, YamlTree)]
-- regularizeSubTree desiredDepth treeList (key, value) =
--   let fullKey = key
--       fullKeyExists (k, _) = k == fullKey
--       depthDiff = maximum [1, desiredDepth - depth value]
--       repeatedValue = generateRepeatedValue depthDiff
--       subTreeList = case L.find fullKeyExists (flattenSubTree treeList) of
--         Just (_, subTree) -> concatMap (regularizeSubTree (desiredDepth - 1) (flattenSubTree treeList)) (flattenSubTree subTree)
--         Nothing -> [(fullKey, repeatedValue)]
--   in [(fullKey, value)] ++ subTreeList

-- flattenSubTree :: [(String, YamlTree)] -> [(String, YamlTree)]
-- flattenSubTree kvs = concatMap flatten kvs
--   where
--     flatten (k, v) = (k, v) : flattenSubTree (flattenTree v)
--     flattenTree (YamlTree t) = t

-- generateRepeatedValue :: Int -> YamlTree
-- generateRepeatedValue depthDiff = YamlTree $ replicate depthDiff ("value", YamlTree [])

-- regularize :: YamlTree -> YamlTree
-- regularize yamlTree@(YamlTree treeList) =
--   if isRegular yamlTree
--     then yamlTree
--     else YamlTree $ concatMap (regularizeSubTree (desiredDepth - 1)) trimmedTrees ++ added
--   where
--     subtrees = map snd treeList
--     maxDepth = maximum $ map depth subtrees
--     (trimmedTrees, added) = L.partition (\t -> depth t == maxDepth) treeList
--     desiredDepth = min (maxDepth + 1) (maximum $ map depth subtrees)

-- regularizeSubTree :: Int -> (String, YamlTree) -> [(String, YamlTree)]
-- regularizeSubTree desiredDepth (key, value) =
--   let fullKey = key
--       fullKeyExists (k, _) = k == fullKey
--       depthDiff = maximum [1, desiredDepth - depth value]
--       repeatedValue = generateRepeatedValue depthDiff
--       subTreeList = case L.find fullKeyExists (flattenSubTree value) of
--         Just (_, subTree) -> concatMap (regularizeSubTree (desiredDepth - 1)) (flattenSubTree subTree)
--         Nothing -> [(fullKey, repeatedValue)]
--   in [(fullKey, value)] ++ subTreeList



-- flattenSubTree :: YamlTree -> [(String, YamlTree)]
-- flattenSubTree (YamlTree kvs) = concatMap flatten kvs
--     where
--     flatten (k, v) = (k, v) : flattenSubTree v

-- generateRepeatedValue :: Int -> YamlTree
-- generateRepeatedValue depthDiff = YamlTree $ replicate depthDiff ("value", YamlTree [])



-- regularizeYamlTree :: YamlTree -> YamlTree
-- regularizeYamlTree yamlTree@(YamlTree treeList) =
--   if isRegular yamlTree then yamlTree
--   else YamlTree $ concatMap (regularizeSubTree "") treeList
--   where
--     regularizeSubTree :: String -> (String, YamlTree) -> [(String, YamlTree)]
--     regularizeSubTree prefix (key, value) =
--       let fullKey = prefix ++ key
--           fullKeyExists (k, _) = k == fullKey
--           depthDiff = maximum [1, desiredDepth - depth value]
--           repeatedValue = generateRepeatedValue depthDiff
--           subTreeList = case L.find fullKeyExists treeList of
--             Just (_, subTree) -> concatMap (regularizeSubTree $ fullKey ++ "....") (flattenSubTree subTree)
--             Nothing -> [(fullKey, repeatedValue)]
--       in [(fullKey, value)] ++ subTreeList
--     desiredDepth = maximum $ map (depth . snd) treeList
--     flattenSubTree :: YamlTree -> [(String, YamlTree)]
--     flattenSubTree (YamlTree kvs) = concatMap flatten kvs
--       where
--         flatten (k, v) = (k, v) : flattenSubTree v

-- regularizeYamlTree :: YamlTree -> YamlTree
-- regularizeYamlTree yamlTree@(YamlTree treeList) =
--   if isRegular yamlTree then yamlTree
--   else YamlTree $ concatMap (regularizeSubTree "") treeList
--   where
--     regularizeSubTree :: String -> (String, YamlTree) -> [(String, YamlTree)]
--     regularizeSubTree prefix (key, value) =
--       let fullKey = prefix ++ key
--           fullKeyExists (k, _) = k == fullKey
--           depthDiff = maximum [1, desiredDepth - depth value]
--           repeatedValue = generateRepeatedValue depthDiff
--           subTreeList = case L.find fullKeyExists treeList of
--             Just (_, subTree) -> concatMap (regularizeSubTree $ fullKey ++ "..") (flattenSubTree subTree)
--             Nothing -> [("value", repeatedValue)]
--       in [(fullKey, value)] ++ subTreeList
--     desiredDepth = maximum $ map (depth . snd) treeList
-- flattenSubTree :: YamlTree -> [(String, YamlTree)]
-- flattenSubTree (YamlTree kvs) = concatMap flatten kvs
--     where
--     flatten (k, v) = (k, v) : flattenSubTree v

-- regularizeYamlTree :: YamlTree -> YamlTree
-- regularizeYamlTree yamlTree@(YamlTree treeList) =
--   if isRegular yamlTree then yamlTree
--   else YamlTree $ concatMap (regularizeSubTree "") treeList
--   where
--     regularizeSubTree :: String -> (String, YamlTree) -> [(String, YamlTree)]
--     regularizeSubTree prefix (key, value) =
--       let fullKey = prefix ++ key
--           fullKeyExists (k, _) = k == fullKey
--           depthDiff = maximum [1, desiredDepth - depth value]
--           repeatedValue = YamlTree $ replicate depthDiff ("value", YamlTree [])
--           subTreeList = case L.find fullKeyExists treeList of
--             Just (_, subTree) -> concatMap (regularizeSubTree $ fullKey ++ ".") (flattenSubTree subTree)
--             Nothing -> [("value", repeatedValue)]
--       in [(fullKey, value)] ++ subTreeList
--     desiredDepth = maximum $ map (depth . snd) treeList
-- flattenSubTree :: YamlTree -> [(String, YamlTree)]
-- flattenSubTree (YamlTree kvs) = concatMap flatten kvs
--     where
--     flatten (k, v) = (k, v) : flattenSubTree v
   

-- regularizeYamlTree :: YamlTree -> YamlTree
-- regularizeYamlTree yamlTree@(YamlTree treeList) =
--   if isRegular yamlTree then yamlTree
--   else YamlTree $ concatMap (regularizeSubTree "") treeList

-- regularizeSubTree :: String -> YamlTree -> [(String, YamlTree)]
-- regularizeSubTree prefix yamlSubTree@(YamlTree []) = [(prefix, yamlSubTree)]
-- regularizeSubTree prefix yamlSubTree@(YamlTree subTreeList) =
--     let maxDepth = maximum $ map (getDepth . snd) subTreeList
--         repeatValues = repeatValueToDepth maxDepth
--     in if isRegular yamlSubTree
--         then [(prefix, yamlSubTree)]
--         else (prefix, yamlSubTree) : concatMap (regularizeSubTree' repeatValues) subTreeList
--     where
--         regularizeSubTree' :: [String] -> (String, YamlTree) -> [(String, YamlTree)]
--         regularizeSubTree' repeatValues (key, value) =
--             let newPrefix = prefix ++ "." ++ key
--             in case lookup key subTreeList of
--                     Nothing -> regularizeSubTree newPrefix value
--                     Just subTree -> [(newPrefix ++ "." ++ rKey, rValue) | (rKey, rValue) <- regularizeSubTree newPrefix subTree]
  

-- regularizeYamlTree :: YamlTree -> YamlTree
-- regularizeYamlTree yamlTree@(YamlTree treeList) =
--   if isRegular yamlTree then yamlTree 
--   else YamlTree $ concatMap (regularizeSubTree "") treeList
--   where
--     regularizeSubTree :: String -> (String, YamlTree) -> [(String, YamlTree)]
--     regularizeSubTree prefix (key, subTree@(YamlTree subTreeList)) =
--       let newPrefix = if null prefix then key else prefix ++ "." ++ key
--       in if isRegular subTree
--          then [(newPrefix, subTree)]
--          else (newPrefix, subTree) : concatMap (regularizeSubTree newPrefix) subTreeList
-- regularizeYamlTree :: YamlTree -> YamlTree
-- regularizeYamlTree yamlTree@(YamlTree treeList) = 
--   if isRegular yamlTree then yamlTree 
--   else YamlTree $ map (\(k, v) -> (k, regularizeSubTree v)) treeList
--   where
--     regularizeSubTree :: YamlTree -> YamlTree
--     regularizeSubTree yamlSubTree@(YamlTree []) = yamlSubTree
--     regularizeSubTree yamlSubTree@(YamlTree subTreeList) = 
--       if isRegular yamlSubTree then yamlSubTree
--       else YamlTree [(key, regularizeSubTree value) | (key, value) <- subTreeList] 

-- regularizeYamlTree :: YamlTree -> YamlTree
-- regularizeYamlTree (YamlTree treeList) = YamlTree $ map (\(k, v) -> (k, regularizeSubTree v)) treeList
--   where
--     regularizeSubTree :: YamlTree -> YamlTree
--     regularizeSubTree (YamlTree []) = YamlTree []
--     regularizeSubTree (YamlTree subTreeList) = YamlTree [(key ++ "aaaaaa", regularizeSubTree value) | (key, value) <- subTreeList] 

-- regularize :: YamlTree -> YamlTree
-- regularize yt = regularize' yt (depth yt)
-- regularizeYamlTree :: YamlTree -> YamlTree
-- regularizeYamlTree (YamlTree []) = YamlTree []
-- regularizeYamlTree (YamlTree xs) = YamlTree (concatMap copyLevels xs)
--   where
--     copyLevels :: (String, YamlTree) -> [(String, YamlTree)]
--     copyLevels (name, tree@(YamlTree children)) =
--       case children of
--         [] -> [(name, tree)]
--         _  -> [(name, subtree) | (childName, childTree) <- children,
--                                  subtree <- regularizeSubtree childName childTree]
    

-- regularizeYamlTree :: Int -> YamlTree -> YamlTree
-- regularizeYamlTree depth (YamlTree items) =
--   if depth == 0
--     then YamlTree []
--     else YamlTree $ map (\(key, value) -> (key, regularizeYamlTree (depth - 1) value)) items
-- | Regularize a YamlTree by copying levels until the desired depth is reached
-- regularizeYamlTree :: Int -> YamlTree -> YamlTree
-- regularizeYamlTree depth (YamlTree items) =
--   if depth == 0
--     then YamlTree []
--     else YamlTree $ map (\(key, value) -> (key, regularizeYamlTree (depth - 1) value)) items

-- regularizeSubtree :: String -> YamlTree -> [YamlTree]
-- regularizeSubtree name (YamlTree children) =
--     case children of
--     [] -> [YamlTree [(name, YamlTree [])]]
--     _  -> [YamlTree [(name, subtree)] | (childName, childTree) <- children,
--                                         subtree <- regularizeSubtree childName childTree]

---------------------------------------------------------------------------------
main :: IO ()
main = do
    -- input  <- Y.decodeFileThrow "instruments-hierarchy.yaml"
    -- output <- Y.decodeFileThrow "instruments-hierarchy-regular.yaml"
    -- let regularized = regularize input
    -- putStrLn (show $ regularized == output)
    yamlValue <- parse "instruments-hierarchy.yaml"
    let yamlTree = convertToYAMLTree yamlValue
    let regularized = regularize yamlTree
    yamlValue2 <- parse "instruments-hierarchy-regular.yaml"
    let yamlTree2 = convertToYAMLTree yamlValue2
    --print (prop_isRegular yamlTree) --works fine
    putStrLn (show $ regularized == yamlTree2)
    print regularized
    print (depth yamlTree) 
    print (isRegular yamlTree2)
    yamlTreeToYamlFile "output2.yaml" regularized
    print (isRegular regularized)
    --print yamlTree2



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
    --Q.quickCheck (prop_my_io_action)
    -- yamlValue <- parse "test.yaml"
    --let yamlTree = convertToYAMLTree yamlValue
    --Q.quickCheck $ prop_parser_prettyprinter
    --Q.quickCheck (prop_parser_prettyprinter)
    -- print $ prop_my_io_action (YamlTree [("abvc", YamlTree[("ttttt", YamlTree[])])])

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
--SOALLLLA: decodeFileThrow why doesn't respond
