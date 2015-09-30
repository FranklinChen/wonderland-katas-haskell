module Kata.AlphabetCipherSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Kata.AlphabetCipher
  (Letter, PlainText, Key, encode, decode, decypher)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "AlphabetCipher" $ do
    it "can encode given a secret keyword" $ do
      encode "vigilance" "meetmeontuesdayeveningatseven" `shouldBe`
        "hmkbxebpxpmyllyrxiiqtoltfgzzv"
      encode "scones" "meetmebythetree" `shouldBe`
        "egsgqwtahuiljgs"
    it "can decode an encrypted message given a secret keyword" $ do
      decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv" `shouldBe`
        "meetmeontuesdayeveningatseven"
      decode "scones" "egsgqwtahuiljgs" `shouldBe`
        "meetmebythetree"
    it "can extract the secret keyword given an encrypted message and the original message" $ do
      decypher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog" `shouldBe`
        "vigilance"
      decypher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs" `shouldBe`
        "scones"
    it "round trips plain to cipher to plain" $ do
      forAll ((,) <$> genKey <*> genPlain) $ \(key, plain) ->
        decode key (encode key plain) == plain

genKey :: Gen Key
genKey = listOf1 genLetter

genPlain :: Gen PlainText
genPlain = listOf genLetter

genLetter :: Gen Letter
genLetter = choose ('a', 'z')
