module Kata.AlphabetCipher
       (
         Letter
       , PlainText
       , Key
       , CipherText

       , encode
       , decode
       , decypher
       ) where

import Control.Arrow ((>>>))
import Data.Char as Char (ord, chr)
import Data.List (isPrefixOf)
import Data.Array.Unboxed (UArray, array, (!))

-- | We only support 'a'..'z' here.
-- TODO. Could enforce with newtype.
type Letter = Char

type PlainText = [Letter]
type Key = [Letter]
type CipherText = [Letter]

-- | To be frank, it probably is not worth using a pre-calculated
-- array in this situation because the computation without an array
-- is so simple.
encoderArray :: UArray (Letter, Letter) Letter
encoderArray = array (('a', 'a'), ('z', 'z')) $ do
  key <- ['a'..'z']
  plain <- ['a'..'z']
  return $!
    (
      (key, plain)
    , toLetter ((toIndex key + toIndex plain) `mod` numLetters)
    )

encodeLetter :: Letter -> Letter -> Letter
encodeLetter key plain = encoderArray ! (key, plain)

decoderArray :: UArray (Letter, Letter) Letter
decoderArray = array (('a', 'a'), ('z', 'z')) $ do
  key <- ['a'..'z']
  cipher <- ['a'..'z']
  return $!
    (
      (key, cipher)
    , toLetter ((toIndex cipher - toIndex key) `mod` numLetters)
    )

decodeLetter :: Letter -> Letter -> Letter
decodeLetter key cipher = decoderArray ! (key, cipher)

numLetters :: Int
numLetters = (zOrd - aOrd) + 1

aOrd :: Int
aOrd = ord 'a'

zOrd :: Int
zOrd = ord 'z'

toIndex :: Letter -> Int
toIndex = ord >>> subtract aOrd

toLetter :: Int -> Letter
toLetter = (+ aOrd) >>> chr

-- | Repeat the key to get an encoder for the key
encode :: Key -> (PlainText -> CipherText)
encode = cycle >>> zipWith encodeLetter

decode :: Key -> (CipherText -> PlainText)
decode = cycle >>> zipWith decodeLetter

decypher :: CipherText -> PlainText -> Key
decypher cipher plain = deduceKey $ zipWith decodeLetter plain cipher

-- | Deduce the key that is repeated. Lazily find the single smallest prefix
-- that could be a key (this heuristic could be wrong).
--
-- If there is none, then assume the whole string is the key, although
-- this could be false if the texts are too small for the key to have
-- been repeated!
deduceKey :: String -> Key
deduceKey seen =
  case [ guess
       | (guess, rest) <- possibleSplits seen
       , rest `isPrefixOf` cycle guess
       ] of
    (key:_) -> key
    _ -> seen

-- | Stream of all possible splits of a list into a nonempty prefix
-- and nonempty suffix, starting with smallest prefix of 1.
possibleSplits :: [a] -> [([a], [a])]
possibleSplits xs = map (\i -> splitAt i xs) [1 .. (length xs - 1)]
