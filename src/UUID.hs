{-# LANGUAGE
    OverloadedStrings
  , GeneralizedNewtypeDeriving
  , DataKinds
  , ApplicativeDo
#-}

module UUID
  ( encodeHyphenated
  , decodeHyphenated
  , builderHyphenated
  , parserHyphenated
  ) where

import Data.ByteArray.Builder.Bounded
import Data.Bytes
import Data.Bytes.Parser
import Data.Word
import Data.Bits
import Data.Primitive.ByteArray
import Data.WideWord.Word128
import Data.Bytes.Parser.Latin
import Arithmetic.Nat

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
           word16PaddedLowerHex w1
  `append` word16PaddedLowerHex w2
  `append` ascii '-'
  `append` word16PaddedLowerHex w3
  `append` ascii '-'
  `append` word16PaddedLowerHex w4
  `append` ascii '-'
  `append` word16PaddedLowerHex w5
  `append` ascii '-'
  `append` word16PaddedLowerHex w6
  `append` word16PaddedLowerHex w7
  `append` word16PaddedLowerHex w8
  where
  (w1,w2,w3,w4,w5,w6,w7,w8) = toWord16s uuid

decodeHyphenated :: Bytes -> Maybe Word128
decodeHyphenated uuid = parseBytesMaybe (parserHyphenated ()) uuid

-- | Parser type from @bytesmith@
parserHyphenated :: e -> Parser e s Word128
parserHyphenated err = do
  w1 <- hexFixedWord16 err
  w2 <- hexFixedWord16 err
  skipChar1 err '-'
  w3 <- hexFixedWord16 err
  skipChar1 err '-'
  w4 <- hexFixedWord16 err
  skipChar1 err '-'
  w5 <- hexFixedWord16 err
  skipChar1 err '-'
  w6 <- hexFixedWord16 err
  w7 <- hexFixedWord16 err
  w8 <- hexFixedWord16 err
  pure $ Word128
    { word128Hi64 = fromWord16sWord64 w1 w2 w3 w4
    , word128Lo64 = fromWord16sWord64 w5 w6 w7 w8
    }

fromWord16sWord64 ::
     Word16 -> Word16 -> Word16 -> Word16
  -> Word64
fromWord16sWord64 e f g h =
      shiftL (fromIntegral e) 48
  .|. shiftL (fromIntegral f) 32
  .|. shiftL (fromIntegral g) 16
  .|. (fromIntegral h)

toWord16s :: Word128 -> (Word16,Word16,Word16,Word16,Word16,Word16,Word16,Word16)
toWord16s (Word128 a b) =
  -- Note: implementing this as 2 Word64 shifts with 'unsafeShiftR'
  -- is up to 40% faster than using 128-bit shifts on a Word128 value.
  ( fromIntegral (unsafeShiftR a 48)
  , fromIntegral (unsafeShiftR a 32)
  , fromIntegral (unsafeShiftR a 16)
  , fromIntegral a
  , fromIntegral (unsafeShiftR b 48)
  , fromIntegral (unsafeShiftR b 32)
  , fromIntegral (unsafeShiftR b 16)
  , fromIntegral b
  )
