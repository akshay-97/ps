module Main where

-- read and write to files using js sync functions
import Data.Foldable as F
import Data.Maybe
import Data.String
import Node.FS.Sync
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff (forE, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array as A
import Data.List as L
import Data.List.Lazy (fromFoldable)
import Data.Map as Map
import Data.String.Unsafe as U
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Unsafe.Coerce
import Data.Traversable

type Values  = Array String
type FileMap = Map.Map Char Values

-- utilities
readFilePath :: String
readFilePath = "./data/sample.txt"

encoding :: String -> Encoding
encoding "ASCII" =  ASCII
encoding _       =  UTF8

readTestFile :: forall e. String -> Eff (fs :: FS, exception :: EXCEPTION | e) String
readTestFile encode =  readTextFile (encoding encode) readFilePath

writeToFile :: forall e. String -> String -> Eff (fs :: FS, exception :: EXCEPTION | e) Unit
writeToFile = writeTextFile UTF8

makeListInput :: String -> Array String
makeListInput = validateInput <<< split (Pattern "\n")

validateInput :: Array String -> Array String
validateInput = A.filter (\x -> length x > 0) 

makeWriteFilePath :: Char -> String
makeWriteFilePath fileName = "./data/output/" <> singleton fileName <> ".txt"

makeText :: Array String -> String
makeText = F.foldl (\x acc -> acc <> x <> "\n") ""

-- operations
processInput :: Array String -> FileMap
processInput = makeMap Map.empty

makeMap :: FileMap -> Array String -> FileMap
makeMap = F.foldl updateMap

updateMap :: FileMap -> String -> FileMap
updateMap map inp =  let key      = U.char $ take 1 inp
                         maybeVal = Map.lookup key map
                      in case maybeVal of
                          Just a  -> Map.update (update' inp) key map
                          Nothing -> Map.insert key (A.singleton inp) map
                          where
                            update' inp a = Just $ A.insert inp a

-- flow
main :: forall e. Eff (console :: CONSOLE, fs :: FS, exception :: EXCEPTION | e) Unit
main = do
  file <- readTestFile "UTF"
  let map = (processInput <<< makeListInput) file
      key' = Map.keys map
  writeFlow map key'

writeFlow :: forall e. FileMap -> L.List Char -> Eff (fs :: FS, exception :: EXCEPTION | e) Unit
writeFlow map keys =
  traverse_ write keys
  where
    write key =
      maybe (pure unit)
      (writeToFile (makeWriteFilePath key) <<< makeText)
      $ Map.lookup key map