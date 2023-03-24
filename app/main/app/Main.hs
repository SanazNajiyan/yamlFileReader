{-# LANGUAGE DeriveGeneric #-}--to derive Generic instances


module Main where
import qualified Data.List as L
import Data.List (isPrefixOf)
import qualified Data.Char as C
import qualified Data.Yaml as Y
import Data.Yaml (encode)
import qualified Data.Yaml.Parser as Y
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.String (IsString(..))
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml.Pretty as PP
import GHC.Generics
import Data.Aeson as A
import qualified Data.Map as Map


data YamlTree = YamlTree [(String, YamlTree)]
    deriving (Show, Generic)

parse :: FilePath -> IO Y.YamlValue
parse = Y.readYamlFile 

convertToYAMLTree :: Y.YamlValue -> YamlTree
convertToYAMLTree (Y.Mapping list _) = YamlTree (L.map (\(xs,ys) -> (T.unpack xs, convertToYAMLTree ys)) list)
convertToYAMLTree (Y.Scalar xs _ _ _) = YamlTree [(BS.unpack xs, YamlTree [])] --when we reach a Leaf
--convertToYAMLTree (Y.Sequence list _) = YamlTree $ L.map (\x -> ("-", convertToYAMLTree x)) $ V.toList $ V.fromList list --In this implementation the list argument is first converted to a Vector using V.fromList, and then V.toList is used to convert the Vector to a list.
convertToYAMLTree _ = YamlTree []

yamlTreeToYamlFile :: FilePath -> YamlTree -> IO ()
yamlTreeToYamlFile filePath tree = do
    let yamlText = yamlTreeToText tree
    writeFile filePath yamlText

yamlTreeToText :: YamlTree -> String
yamlTreeToText (YamlTree []) = ""
yamlTreeToText (YamlTree ((key, value):rest)) =
    indent (key ++ ":\n") ++ subtreeText ++ yamlTreeToText (YamlTree rest)
    where subtreeText = indent $ trimEnd $ unlines $ L.map trimEnd $ lines $ yamlTreeToText value


indent :: String -> String
indent line = L.replicate (2 * level) ' ' ++ line ++ "\n"
    where 
        level = length $ L.takeWhile (== ' ') line

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn delimiter xs = go xs []
    where 
        go [] acc = [reverse acc]
        go ys acc
            | delimiter `isPrefixOf` ys = let (as, bs) = L.splitAt (length delimiter) ys
                                        in reverse acc : go (L.drop (length delimiter) bs) []
            | otherwise = case ys of
                            (y:ys') -> go ys' (y:acc)
                            [] -> [reverse acc]

trimEnd :: String -> String
trimEnd = reverse . L.dropWhile . reverse

yamlTreeToYamlObject :: YamlTree -> Y.Value
yamlTreeToYamlObject (YamlTree list) = Y.object $ L.map toPair list
  where
    toPair (key, value) = (fromString key Y..= yamlTreeToYamlObject value)


main :: IO ()
main = do 
    yamlValue <- parse "instruments-hierarchy-regular.yaml"
    let yamlTree = convertToYAMLTree yamlValue
    yamlTreeToYamlFile "output.yaml" yamlTree    
