{- Inspection tests for
== Zipping and unzipping Sublist ==
+ zip
+ zip3
+ zip4
+ zip5
+ zip6
+ zip7
+ zipWith
+ zipWith3
+ zipWith4
+ zipWith5
+ zipWith6
+ zipWith7
+ unzip
+ unzip3
+ unzip4
+ unzip5
+ unzip6
+ unzip7
-}

{-# language TemplateHaskell #-}
{-# language NoImplicitPrelude #-}
{-# language BangPatterns #-}
{-# options_ghc -O -fplugin Test.Inspection.Plugin #-}
module InspectionTests.Zipping where

import Data.Char (Char)
import Data.Maybe (Maybe(..))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word8)
import Test.Inspection

import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.List
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Vector
import qualified Data.Word
import Consy


{- zip -}
consZip, listZip :: [a] -> [b] -> [(a,b)]
consZip = zip
-- listZip = zipWith (,)
listZip [] _ = []
listZip _ [] = []
listZip (a:as) (b:bs) = (a,b) : listZip as bs
inspect ('consZip === 'listZip)

consZipText, textZip :: Text -> Text -> [(Char, Char)]
consZipText = zip
textZip = Data.Text.zip
inspect ('consZipText === 'textZip)

consZipLazyText, lazyTextZip :: Data.Text.Lazy.Text -> Data.Text.Lazy.Text -> [(Char, Char)]
consZipLazyText = zip
lazyTextZip = Data.Text.Lazy.zip
inspect ('consZipLazyText === 'lazyTextZip)

consZipVector, vectorZip :: Vector a -> Vector b -> Vector (a,b)
consZipVector = zip
vectorZip = Data.Vector.zip
inspect ('consZipVector === 'vectorZip)

consZipBS, bsZip ::  Data.ByteString.ByteString -> Data.ByteString.ByteString -> [(Word8, Word8)]
consZipBS = zip
bsZip = Data.ByteString.zip
inspect ('consZipBS === 'bsZip)

consZipLBS, lbsZip :: Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString -> [(Word8, Word8)]
consZipLBS = zip
lbsZip = Data.ByteString.Lazy.zip
inspect ('consZipLBS === 'lbsZip)

-- {-# noinline consZipSeq #-}
-- {-# noinline seqZip #-}
consZipSeq, seqZip :: Seq a -> Seq b -> Seq (a,b)
consZipSeq = zip
seqZip = Data.Sequence.zip
inspect ('consZipSeq === 'seqZip)


{- zip3 -}
consZip3, listZip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
consZip3 = zip3
-- listZip3 = zipWith3 (,,)
listZip3 (a:as) (b:bs) (c:cs) = (a,b,c) : listZip3 as bs cs
listZip3 _ _ _ = []
inspect ('consZip3 === 'listZip3)

consZip3Vector, vectorZip3 :: Vector a -> Vector b -> Vector c -> Vector (a,b,c)
consZip3Vector = zip3
vectorZip3 = Data.Vector.zip3
inspect ('consZip3Vector === 'vectorZip3)

-- {-# noinline consZip3Seq #-}
-- {-# noinline seqZip3 #-}
consZip3Seq, seqZip3 :: Seq a -> Seq b -> Seq c -> Seq (a,b,c)
consZip3Seq = zip3
seqZip3 = Data.Sequence.zip3
inspect ('consZip3Seq === 'seqZip3)


{- zip4 -}
consZip4, listZip4 :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
consZip4 = zip4
-- listZip4 = zipWith4 (,,,)
listZip4 (a:as) (b:bs) (c:cs) (d:ds) = (a,b,c,d) : listZip4 as bs cs ds
listZip4 _ _ _ _ = []
inspect ('consZip4 === 'listZip4)

consZip4Vector, vectorZip4 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector (a,b,c,d)
consZip4Vector = zip4
vectorZip4 = Data.Vector.zip4
inspect ('consZip4Vector === 'vectorZip4)

-- {-# noinline consZip4Seq #-}
-- {-# noinline seqZip4 #-}
consZip4Seq, seqZip4 :: Seq a -> Seq b -> Seq c -> Seq d -> Seq (a,b,c,d)
consZip4Seq = zip4
seqZip4 = Data.Sequence.zip4
inspect ('consZip4Seq === 'seqZip4)


{- zip5 -}
consZip5, listZip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
consZip5 = zip5
-- listZip5 = zipWith4 (,,,)
listZip5 (a:as) (b:bs) (c:cs) (d:ds) (e:es) = (a,b,c,d,e) : listZip5 as bs cs ds es
listZip5 _ _ _ _ _ = []
inspect ('consZip5 === 'listZip5)

consZip5Vector, vectorZip5 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector (a,b,c,d,e)
consZip5Vector = zip5
vectorZip5 = Data.Vector.zip5
inspect ('consZip5Vector === 'vectorZip5)


{- zip6 -}
consZip6, listZip6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a,b,c,d,e,f)]
consZip6 = zip6
-- listZip6 = zipWith5 (,,,,)
listZip6 (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs )= (a,b,c,d,e,f) : listZip6 as bs cs ds es fs
listZip6 _ _ _ _ _ _ = []
inspect ('consZip6 === 'listZip6)

consZip6Vector, vectorZip6 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f -> Vector (a,b,c,d,e,f)
consZip6Vector = zip6
vectorZip6 = Data.Vector.zip6
inspect ('consZip6Vector === 'vectorZip6)


{- zip7 -}
consZip7, listZip7 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [(a,b,c,d,e,f,g)]
consZip7 = zip7
-- listZip7 = zipWith6 (,,,,,)
listZip7 (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) = (a,b,c,d,e,f,g) : listZip7 as bs cs ds es fs gs
listZip7 _ _ _ _ _ _ _ = []
inspect ('consZip7 === 'listZip7)


{- zipWith -}
consZipWith, listZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
consZipWith f = zipWith f
listZipWith f = go
  where
    go [] _ = []
    go _ [] = []
    go (a:as) (b:bs) = f a b : go as bs
inspect ('consZipWith === 'listZipWith)

consZipWithText, textZipWith :: (Char -> Char -> Char) -> Text -> Text -> Text
consZipWithText = zipWith
textZipWith = Data.Text.zipWith
inspect ('consZipWithText === 'textZipWith)

consZipWithLazyText, lazyTextZipWith :: (Char -> Char -> Char) -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text -> Data.Text.Lazy.Text
consZipWithLazyText = zipWith
lazyTextZipWith = Data.Text.Lazy.zipWith
inspect ('consZipWithLazyText === 'lazyTextZipWith)

consZipWithVector, vectorZipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
consZipWithVector = zipWith
vectorZipWith = Data.Vector.zipWith
inspect ('consZipWithVector === 'vectorZipWith)

consZipWithBS, bsZipWith :: (Word8 -> Word8 -> a) -> Data.ByteString.ByteString -> Data.ByteString.ByteString -> [a]
consZipWithBS = zipWith
bsZipWith = Data.ByteString.zipWith
inspect ('consZipWithBS === 'bsZipWith)

consZipWithLBS, lbsZipWith :: (Word8 -> Word8 -> a) -> Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString -> [a]
consZipWithLBS = zipWith
lbsZipWith = Data.ByteString.Lazy.zipWith
inspect ('consZipWithLBS === 'lbsZipWith)

-- {-# noinline consZipWithSeq #-}
-- {-# noinline seqZipWith #-}
consZipWithSeq, seqZipWith :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
consZipWithSeq = zipWith
seqZipWith = Data.Sequence.zipWith
inspect ('consZipWithSeq === 'seqZipWith)


{- zipWith3 -}
consZipWith3, listZipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
consZipWith3 f = zipWith3 f
listZipWith3 f = go
  where
    go (a:as) (b:bs) (c:cs) = f a b c : go as bs cs
    go _ _ _ = []
inspect ('consZipWith3 === 'listZipWith3)

consZipWith3Vector, vectorZipWith3 :: (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
consZipWith3Vector = zipWith3
vectorZipWith3 = Data.Vector.zipWith3
inspect ('consZipWith3Vector === 'vectorZipWith3)

-- {-# noinline consZipWith3Seq #-}
-- {-# noinline seqZipWith3 #-}
consZipWith3Seq, seqZipWith3 :: (a -> b -> c -> d) -> Seq a -> Seq b -> Seq c -> Seq d
consZipWith3Seq = zipWith3
seqZipWith3 = Data.Sequence.zipWith3
inspect ('consZipWith3Seq === 'seqZipWith3)


{- zipWith4 -}
consZipWith4, listZipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
consZipWith4 f = zipWith4 f
listZipWith4 f = go
  where
    go (a:as) (b:bs) (c:cs) (d:ds) = f a b c d : go as bs cs ds
    go _ _ _ _ = []
inspect ('consZipWith4 === 'listZipWith4)

consZipWith4Vector, vectorZipWith4 :: (a -> b -> c -> d -> e) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
consZipWith4Vector = zipWith4
vectorZipWith4 = Data.Vector.zipWith4
inspect ('consZipWith4Vector === 'vectorZipWith4)

-- {-# noinline consZipWith4Seq #-}
-- {-# noinline seqZipWith4 #-}
consZipWith4Seq, seqZipWith4 :: (a -> b -> c -> d -> e) -> Seq a -> Seq b -> Seq c -> Seq d -> Seq e
consZipWith4Seq = zipWith4
seqZipWith4 = Data.Sequence.zipWith4
inspect ('consZipWith4Seq === 'seqZipWith4)


{- zipWith5 -}
consZipWith5, listZipWith5 :: (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
consZipWith5 f = zipWith5 f
listZipWith5 f = go
  where
    go (a:as) (b:bs) (c:cs) (d:ds) (e:es) = f a b c d e : go as bs cs ds es
    go _ _ _ _ _ = []
inspect ('consZipWith5 === 'listZipWith5)

consZipWith5Vector, vectorZipWith5 :: (a -> b -> c -> d -> e -> f) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f
consZipWith5Vector = zipWith5
vectorZipWith5 = Data.Vector.zipWith5
inspect ('consZipWith5Vector === 'vectorZipWith5)


{- zipWith6 -}
consZipWith6, listZipWith6 :: (a -> b -> c -> d -> e -> f -> g) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
consZipWith6 ff = zipWith6 ff
listZipWith6 ff = go
  where
    go (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs)= ff a b c d e f : go as bs cs ds es fs
    go _ _ _ _ _ _ = []
inspect ('consZipWith6 === 'listZipWith6)

consZipWith6Vector, vectorZipWith6 :: (a -> b -> c -> d -> e -> f -> g) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f -> Vector g
consZipWith6Vector = zipWith6
vectorZipWith6 = Data.Vector.zipWith6
inspect ('consZipWith6Vector === 'vectorZipWith6)


{- zipWith7 -}
consZipWith7, listZipWith7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h]
consZipWith7 f = zipWith7 f
listZipWith7 z = go
  where
    go (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) = z a b c d e f g : go as bs cs ds es fs gs
    go _ _ _ _ _ _ _ = []
inspect ('consZipWith7 === 'listZipWith7)


{- unzip -}
consUnzip, listUnzip :: [(a,b)] -> ([a],[b])
consUnzip = unzip
listUnzip =  Data.List.foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])
inspect ('consUnzip === 'listUnzip)

consUnzipVector, vectorUnzip :: Vector (a, b) -> (Vector a, Vector b)
consUnzipVector = unzip
vectorUnzip = Data.Vector.unzip
inspect ('consUnzipVector === 'vectorUnzip)

consUnzipBS, bsUnzip :: [(Word8, Word8)] -> (Data.ByteString.ByteString, Data.ByteString.ByteString)
consUnzipBS = unzip
bsUnzip = Data.ByteString.unzip
inspect ('consUnzipBS === 'bsUnzip)

consUnzipLBS, lbsUnzip :: [(Word8, Word8)] -> (Data.ByteString.Lazy.ByteString, Data.ByteString.Lazy.ByteString)
consUnzipLBS = unzip
lbsUnzip = Data.ByteString.Lazy.unzip
inspect ('consUnzipLBS === 'lbsUnzip)


{- unzip3 -}
consUnzip3, listUnzip3 :: [(a,b,c)] -> ([a],[b],[c])
consUnzip3 = unzip3
listUnzip3 =  Data.List.foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs)) ([],[],[])
inspect ('consUnzip3 === 'listUnzip3)

consUnzip3Vector, vectorUnzip3 :: Vector (a, b, c) -> (Vector a, Vector b, Vector c)
consUnzip3Vector = unzip3
vectorUnzip3 = Data.Vector.unzip3
inspect ('consUnzip3Vector === 'vectorUnzip3)


{- unzip4 -}
consUnzip4, listUnzip4 :: [(a,b,c,d)] -> ([a],[b],[c],[d])
consUnzip4 = unzip4
listUnzip4 =  Data.List.foldr (\(a,b,c,d) ~(as,bs,cs,ds) -> (a:as,b:bs,c:cs,d:ds)) ([],[],[],[])
inspect ('consUnzip4 === 'listUnzip4)

consUnzip4Vector, vectorUnzip4 :: Vector (a, b, c, d) -> (Vector a, Vector b, Vector c, Vector d)
consUnzip4Vector = unzip4
vectorUnzip4 = Data.Vector.unzip4
inspect ('consUnzip4Vector === 'vectorUnzip4)


{- unzip5 -}
consUnzip5, listUnzip5 :: [(a,b,c,d,e)] -> ([a],[b],[c],[d],[e])
consUnzip5 = unzip5
listUnzip5 =  Data.List.foldr (\(a,b,c,d,e) ~(as,bs,cs,ds,es) -> (a:as,b:bs,c:cs,d:ds,e:es)) ([],[],[],[],[])
inspect ('consUnzip5 === 'listUnzip5)

consUnzip5Vector, vectorUnzip5 :: Vector (a, b, c, d, e) -> (Vector a, Vector b, Vector c, Vector d, Vector e)
consUnzip5Vector = unzip5
vectorUnzip5 = Data.Vector.unzip5
inspect ('consUnzip5Vector === 'vectorUnzip5)


{- unzip6 -}
consUnzip6, listUnzip6 :: [(a,b,c,d,e,f)] -> ([a],[b],[c],[d],[e],[f])
consUnzip6 = unzip6
listUnzip6 =  Data.List.foldr (\(a,b,c,d,e,f) ~(as,bs,cs,ds,es,fs) -> (a:as,b:bs,c:cs,d:ds,e:es,f:fs)) ([],[],[],[],[],[])
inspect ('consUnzip6 === 'listUnzip6)

consUnzip6Vector, vectorUnzip6 :: Vector (a, b, c, d, e, f) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f)
consUnzip6Vector = unzip6
vectorUnzip6 = Data.Vector.unzip6
inspect ('consUnzip6Vector === 'vectorUnzip6)


{- unzip7 -}
consUnzip7, listUnzip7 :: [(a,b,c,d,e,f,g)] -> ([a],[b],[c],[d],[e],[f],[g])
consUnzip7 = unzip7
listUnzip7 =  Data.List.foldr (\(a,b,c,d,e,f,g) ~(as,bs,cs,ds,es,fs,gs) -> (a:as,b:bs,c:cs,d:ds,e:es,f:fs,g:gs)) ([],[],[],[],[],[],[])
inspect ('consUnzip7 === 'listUnzip7)
