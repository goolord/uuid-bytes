module Main where

import Data.Bytes
import Data.WideWord.Word128
import Test.Tasty
import Test.Tasty.HUnit
import UUID

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "UUID encoding test" $
      fromByteArray (encodeHyphenated uuidW128) @?= uuidBytes 
  , testCase "UUID decoding test" $
      decodeHyphenated uuidBytes @?= Just uuidW128
  , testCase "UUID encoding/decoding idempotence test" $
      decodeHyphenated (fromByteArray $ encodeHyphenated uuidW128) @?= Just uuidW128
  , testCase "UUIDUH encoding test" $
      fromByteArray (encodeUnhyphenated uuidW128) @?= uuidBytesUH
  , testCase "UUIDUH decoding test" $
      decodeUnhyphenated uuidBytesUH @?= Just uuidW128
  , testCase "UUIDUH encoding/decoding idempotence test" $
      decodeUnhyphenated (fromByteArray $ encodeUnhyphenated uuidW128) @?= Just uuidW128
  ]

uuidBytes :: Bytes
uuidBytes = fromAsciiString "123e4567-e89b-12d3-a456-426655440000"

uuidBytesUH :: Bytes
uuidBytesUH = fromAsciiString "123e4567e89b12d3a456426655440000"

uuidW128 :: Word128
uuidW128 = 24249434048109030647017182302883282944
