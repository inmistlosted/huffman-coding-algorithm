module Main where

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as B
import           Data.List            (foldl', insertBy, sortBy)
import qualified Data.Map             as M
import           Data.Ord             (comparing)
import           Huffman

main :: IO ()
main = do
  testFile1 <- readFile "resources/test1.txt"
  testFile2 <- readFile "resources/test2.txt"
  testFile3 <- readFile "resources/test3.txt"
  let testText = lines testFile1 ++ lines testFile2 ++ lines testFile3
  let frequencies = histogram (concat testText)
  putStrLn "\nSymbols and their occurancies:\n"
  mapM_ print frequencies
  let sortedFrequencies = sortBy (comparing swap) frequencies
  putStrLn "\nSorted by the occurancies count:\n"
  mapM_ print sortedFrequencies
  let huffmanTree = sortedHuffman sortedFrequencies
  putStrLn "\nHaffman tree:\n"
  print huffmanTree
  putStrLn "\nSymbols codes:\n"
  let encoding = codes huffmanTree
  let showCode (s, bits) = show s ++ " -> " ++ showBits bits
  mapM_ (putStrLn . showCode) (M.toList encoding)
  putStrLn "\nEncoded representation:\n"
  let encoded = map (encode encoding) testText
  mapM_ (print . showBits) encoded
  putStrLn "\nWriting result to file 'result.bin'\n"
  let encBits0 = padToEight (concat encoded)
  let bits = bitpack encBits0
  B.writeFile "result.bin" bits
  let Right encBits1 = bitunpack . S.pack . B.unpack $ bits
  putStrLn "\nDecoded representation:\n"
  let decoded = map (decode huffmanTree) encoded
  mapM_ print decoded
