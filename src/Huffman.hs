module Huffman where

import           Control.Concurrent        (forkIO)
import           Control.Monad
import qualified Data.Binary.BitPut        as P
import qualified Data.Binary.Strict.BitGet as G
import qualified Data.ByteString           as S
import qualified Data.ByteString.Lazy      as B
import           Data.Char                 (intToDigit)
import           Data.List                 (foldl', insertBy, sortBy)
import qualified Data.Map                  as M
import           Data.Maybe                (fromJust)
import           Data.Ord                  (comparing)

data HuffmanTree a
  = LeafNode a Int
  | InternalNode Int (HuffmanTree a) (HuffmanTree a)
  deriving (Eq)

-- вивід дерева Хаффмана
instance Show a => Show (HuffmanTree a) where
  show = go ""
    where
      spaces = map (const ' ')
      paren s = "(" ++ s ++ ")"
      go ss (LeafNode s o) = "--" ++ paren (show o) ++ show s ++ "\n"
      go ss (InternalNode o l r) =
        let root = "--" ++ paren (show o) ++ "-+"
            ss' = ss ++ tail (spaces root)
            lbranch = go (ss' ++ "|") l
            rbranch = go (ss' ++ " ") r
         in root ++ lbranch ++ ss' ++ "|\n" ++ ss' ++ "`" ++ rbranch

frequency :: HuffmanTree a -> Int
frequency (LeafNode _ x)       = x
frequency (InternalNode x _ _) = x

-- будуємо дерево Хафмана знизу вгору зі списку символів, відсортованих за частотою
sortedHuffman :: [(a, Int)] -> HuffmanTree a
sortedHuffman
    -- спочатку перетворюємо кожен кортеж у листок, а потім об’єднуємо
 = combine . map toLeaf
    -- комбінуємо дерева найнижчих частот і повторно вставляти результат у упорядкований за частотою список
  where
    combine [t] = t
    combine (ta:tb:ts) = combine . insertBy (comparing frequency) (merge ta tb) $ ts
    -- робимо внутрішній вузол з двох дерев, частота - це сума двох частот дерев
    merge ta tb = InternalNode (frequency ta + frequency tb) ta tb
    -- робимо лист із символу, частотний кортеж
    toLeaf = uncurry LeafNode

-- проходимо по дереву Хаффмана, генеруючи карту від символу до його шляху в дереві Хаффмана (де False - ліворуч, а True - праворуч)
codes :: Ord a => HuffmanTree a -> M.Map a [Bool]
codes = M.fromList . go []
    -- листові вузли позначають кінець шляху до символу
  where
    go p (LeafNode s _)       = [(s, reverse p)]
    -- будуємо зворотній шлях
    go p (InternalNode _ l r) = go (False : p) l ++ go (True : p) r

-- виходячи з таблиці, що відображає символи до відповідних бітових шляхів дерева Хафмана, замінюємо кожен символ його бітовим шляхом
encode :: Ord a => M.Map a [Bool] -> [a] -> [Bool]
encode tbl = concatMap get
  where
    get x = fromJust (M.lookup x tbl)

-- зі списку бітів проходимо по даному дереві хаффмана та виводимо його розшифрований символ, досягнувши листа
decode :: HuffmanTree a -> [Bool] -> [a]
decode t0 xs0 = go t0 xs0
    -- досягнуто листа, випускаємо символ
  where
    go (LeafNode s _) bs = s : go t0 bs
    -- обираємо шлях в залежності від біта
    go (InternalNode _ l r) (b:bs)
      | not b = go l bs
      | otherwise = go r bs
    go _ [] = []

-- підрахувати кількість випадків, коли кожен символ зустрічається у списку
histogram :: Ord a => [a] -> [(a, Int)]
histogram = M.toList . foldl' insert M.empty
  where
    insert a k = M.insertWith (+) k 1 a

swap :: (a, b) -> (b, a)
swap ~(a, b) = (b, a)

showBits :: [Bool] -> String
showBits = map (intToDigit . fromEnum)

bitpack :: [Bool] -> B.ByteString
bitpack = P.runBitPut . mapM_ P.putBit

bitunpack :: S.ByteString -> Either String [Bool]
bitunpack bs0 = G.runBitGet bs0 $ go []
  where
    go a = do
      e <- G.isEmpty
      if e
        then return (reverse a)
        else G.getBit >>= go . (: a)

padToEight :: [Bool] -> [Bool]
padToEight bits =
  let len = length bits
      rem = len `mod` 8
      extra = 8 - rem
      padding = replicate extra False
   in bits ++ padding
