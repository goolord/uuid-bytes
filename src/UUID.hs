{-# LANGUAGE
    OverloadedStrings
  , GeneralizedNewtypeDeriving
  , DataKinds
  , ApplicativeDo
#-}

module UUID
  ( encodeHyphenated
  , builderHyphenated
  , decodeHyphenated
  , parserHyphenated
  , encodeUnhyphenated
  , builderUnhyphenated
  , parserUnhyphenated
  , decodeUnhyphenated
  , decodeLenient
  ) where

import Arithmetic.Nat
import Data.Bits
import Data.Bytes.Builder.Bounded
import Data.Bytes
import Data.Bytes.Parser
import Data.Bytes.Parser.Latin
import Data.Primitive.ByteArray
import Data.WideWord.Word128
import Data.Word

-- | In its canonical textual representation, 
-- the 16 octets of a UUID are represented as 32 hexadecimal (base-16) digits, 
-- displayed in 5 groups separated by hyphens,
-- in the form 8-4-4-4-12 for a total of 36 characters 
-- (32 alphanumeric characters and 4 hyphens)
--
-- UUIDs can also be represented as a base62 encoding of a Word128

encodeHyphenated :: Word128 -> ByteArray
encodeHyphenated uuid = run constant (builderHyphenated uuid)

builderHyphenated :: Word128 -> Builder 36
builderHyphenated uuid = 
           word32PaddedLowerHex w1
  `append` ascii '-'
  `append` word16PaddedLowerHex w2
  `append` ascii '-'
  `append` word16PaddedLowerHex w3
  `append` ascii '-'
  `append` word16PaddedLowerHex w4
  `append` ascii '-'
  `append` word32PaddedLowerHex w5
  `append` word16PaddedLowerHex w6
  where
  (w1,w2,w3,w4,w5,w6) = toWords uuid

decodeHyphenated :: Bytes -> Maybe Word128
decodeHyphenated uuid = parseBytesMaybe (parserHyphenated ()) uuid

-- | Parser type from @bytesmith@
parserHyphenated :: e -> Parser e s Word128
parserHyphenated err = do
  w1 <- hexFixedWord32 err
  skipChar1 err '-'
  w2 <- hexFixedWord16 err
  skipChar1 err '-'
  w3 <- hexFixedWord16 err
  skipChar1 err '-'
  w4 <- hexFixedWord16 err
  skipChar1 err '-'
  w5 <- hexFixedWord32 err
  w6 <- hexFixedWord16 err
  pure $ Word128
    { word128Hi64 = fromW32W16Word64 w1 w2 w3
    , word128Lo64 = fromW16W32W16Word64 w4 w5 w6
    }

toWords :: Word128 -> (Word32,Word16,Word16,Word16,Word32,Word16)
toWords (Word128 a b) =
  -- Note: implementing this as 2 Word64 shifts with 'unsafeShiftR'
  -- is up to 40% faster than using 128-bit shifts on a Word128 value.
  ( fromIntegral (unsafeShiftR a 32)
  , fromIntegral (unsafeShiftR a 16)
  , fromIntegral a
  , fromIntegral (unsafeShiftR b 48)
  , fromIntegral (unsafeShiftR b 16)
  , fromIntegral b
  )

parserUnhyphenated :: e -> Parser e s Word128
parserUnhyphenated err = 
      Word128 
  <$> hexFixedWord64 err
  <*> hexFixedWord64 err

decodeUnhyphenated :: Bytes -> Maybe Word128
decodeUnhyphenated uuid = parseBytesMaybe (parserUnhyphenated ()) uuid

builderUnhyphenated :: Word128 -> Builder 32
builderUnhyphenated = word128PaddedLowerHex

encodeUnhyphenated :: Word128 -> ByteArray
encodeUnhyphenated uuid = run constant (builderUnhyphenated uuid)

parserLenient :: e -> Parser e s Word128
parserLenient err = do
  w1 <- hexFixedWord32 err
  skipChar '-'
  w2 <- hexFixedWord16 err
  skipChar '-'
  w3 <- hexFixedWord16 err
  skipChar '-'
  w4 <- hexFixedWord16 err
  skipChar '-'
  w5 <- hexFixedWord32 err
  w6 <- hexFixedWord16 err
  pure $ Word128
    { word128Hi64 = fromW32W16Word64 w1 w2 w3
    , word128Lo64 = fromW16W32W16Word64 w4 w5 w6
    }

-- | decodes uuid with out without hyphens
decodeLenient :: Bytes -> Maybe Word128
decodeLenient uuid = parseBytesMaybe (parserLenient ()) uuid

fromW32W16Word64 :: Word32 -> Word16 -> Word16 -> Word64
fromW32W16Word64 a b c =
      shiftL (fromIntegral a) 32
  .|. shiftL (fromIntegral b) 16
  .|. (fromIntegral c)

fromW16W32W16Word64 :: Word16 -> Word32 -> Word16 -> Word64
fromW16W32W16Word64 a b c = 
      shiftL (fromIntegral a) 48
  .|. shiftL (fromIntegral b) 16
  .|. (fromIntegral c)
