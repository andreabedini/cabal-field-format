module Distribution.UTF8
  ( -- * Unicode

    -- ** Conversions
    fromUTF8BS,
    fromUTF8LBS,
    toUTF8BS,
    toUTF8LBS,
    validateUTF8,

    -- ** File I/O
    readUTF8File,
    withUTF8FileContents,

    -- ** BOM
    ignoreBOM,

    -- ** Misc
    normaliseLineEndings,
  )
where

import Control.Monad ((>=>))
import Data.Bits (Bits (..), shiftL, (.&.), (.|.))
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (chr, ord)
import Data.Word
import System.IO (IOMode (..), withBinaryFile)

-- ------------------------------------------------------------

-- * Unicode stuff

-- ------------------------------------------------------------

-- | Decode 'String' from UTF8-encoded 'BS.ByteString'
--
-- Invalid data in the UTF8 stream (this includes code-points @U+D800@
-- through @U+DFFF@) will be decoded as the replacement character (@U+FFFD@).
fromUTF8BS :: SBS.ByteString -> String
fromUTF8BS = decodeStringUtf8 . SBS.unpack

-- | Variant of 'fromUTF8BS' for lazy 'BS.ByteString's
fromUTF8LBS :: LBS.ByteString -> String
fromUTF8LBS = decodeStringUtf8 . LBS.unpack

-- | Encode 'String' to UTF8-encoded 'SBS.ByteString'
--
-- Code-points in the @U+D800@-@U+DFFF@ range will be encoded
-- as the replacement character (i.e. @U+FFFD@).
toUTF8BS :: String -> SBS.ByteString
toUTF8BS = SBS.pack . encodeStringUtf8

-- | Variant of 'toUTF8BS' for lazy 'BS.ByteString's
toUTF8LBS :: String -> LBS.ByteString
toUTF8LBS = LBS.pack . encodeStringUtf8

-- | Check that strict 'ByteString' is valid UTF8. Returns 'Just offset' if it's not.
validateUTF8 :: SBS.ByteString -> Maybe Int
validateUTF8 = go 0
  where
    go off bs = case SBS.uncons bs of
      Nothing -> Nothing
      Just (c, bs')
        | c <= 0x7F -> go (off + 1) bs'
        | c <= 0xBF -> Just off
        | c <= 0xDF -> twoBytes off c bs'
        | c <= 0xEF -> moreBytes off 3 0x800 bs' (fromIntegral $ c .&. 0xF)
        | c <= 0xF7 -> moreBytes off 4 0x10000 bs' (fromIntegral $ c .&. 0x7)
        | c <= 0xFB -> moreBytes off 5 0x200000 bs' (fromIntegral $ c .&. 0x3)
        | c <= 0xFD -> moreBytes off 6 0x4000000 bs' (fromIntegral $ c .&. 0x1)
        | otherwise -> Just off

    twoBytes off c0 bs = case SBS.uncons bs of
      Nothing -> Just off
      Just (c1, bs')
        | c1 .&. 0xC0 == 0x80 ->
            if d >= (0x80 :: Int)
              then go (off + 2) bs'
              else Just off
        | otherwise -> Just off
        where
          d = (fromIntegral (c0 .&. 0x1F) `shiftL` 6) .|. fromIntegral (c1 .&. 0x3F)

    moreBytes :: Int -> Int -> Int -> SBS.ByteString -> Int -> Maybe Int
    moreBytes off 1 overlong cs' acc
      | overlong <= acc,
        acc <= 0x10FFFF,
        acc < 0xD800 || 0xDFFF < acc =
          go (off + 1) cs'
      | otherwise =
          Just off
    moreBytes off byteCount overlong bs acc = case SBS.uncons bs of
      Just (cn, bs')
        | cn .&. 0xC0 == 0x80 ->
            moreBytes (off + 1) (byteCount - 1) overlong bs' ((acc `shiftL` 6) .|. fromIntegral cn .&. 0x3F)
      _ -> Just off

-- | Ignore a Unicode byte order mark (BOM) at the beginning of the input
ignoreBOM :: String -> String
ignoreBOM ('\xFEFF' : string) = string
ignoreBOM string = string

-- | Reads a UTF8 encoded text file as a Unicode String
--
-- Reads lazily using ordinary 'readFile'.
readUTF8File :: FilePath -> IO String
readUTF8File f = ignoreBOM . fromUTF8LBS <$> LBS.readFile f

-- | Reads a UTF8 encoded text file as a Unicode String
--
-- Same behaviour as 'withFileContents'.
withUTF8FileContents :: FilePath -> (String -> IO a) -> IO a
withUTF8FileContents name action =
  withBinaryFile
    name
    ReadMode
    (LBS.hGetContents >=> (action . ignoreBOM . fromUTF8LBS))

-- | Fix different systems silly line ending conventions
normaliseLineEndings :: String -> String
normaliseLineEndings [] = []
normaliseLineEndings ('\r' : '\n' : s) = '\n' : normaliseLineEndings s -- windows
normaliseLineEndings ('\r' : s) = '\n' : normaliseLineEndings s -- old OS X
normaliseLineEndings (c : s) = c : normaliseLineEndings s

-- | Decode 'String' from UTF8-encoded octets.
--
-- Invalid data in the UTF8 stream (this includes code-points @U+D800@
-- through @U+DFFF@) will be decoded as the replacement character (@U+FFFD@).
--
-- See also 'encodeStringUtf8'
decodeStringUtf8 :: [Word8] -> String
decodeStringUtf8 = go
  where
    go :: [Word8] -> String
    go [] = []
    go (c : cs)
      | c <= 0x7F = chr (fromIntegral c) : go cs
      | c <= 0xBF = replacementChar : go cs
      | c <= 0xDF = twoBytes c cs
      | c <= 0xEF = moreBytes 3 0x800 cs (fromIntegral $ c .&. 0xF)
      | c <= 0xF7 = moreBytes 4 0x10000 cs (fromIntegral $ c .&. 0x7)
      | c <= 0xFB = moreBytes 5 0x200000 cs (fromIntegral $ c .&. 0x3)
      | c <= 0xFD = moreBytes 6 0x4000000 cs (fromIntegral $ c .&. 0x1)
      | otherwise = replacementChar : go cs

    twoBytes :: Word8 -> [Word8] -> String
    twoBytes c0 (c1 : cs')
      | c1 .&. 0xC0 == 0x80 =
          let d =
                (fromIntegral (c0 .&. 0x1F) `shiftL` 6)
                  .|. fromIntegral (c1 .&. 0x3F)
           in if d >= 0x80
                then chr d : go cs'
                else replacementChar : go cs'
    twoBytes _ cs' = replacementChar : go cs'

    moreBytes :: Int -> Int -> [Word8] -> Int -> [Char]
    moreBytes 1 overlong cs' acc
      | overlong <= acc,
        acc <= 0x10FFFF,
        acc < 0xD800 || 0xDFFF < acc =
          chr acc : go cs'
      | otherwise =
          replacementChar : go cs'
    moreBytes byteCount overlong (cn : cs') acc
      | cn .&. 0xC0 == 0x80 =
          moreBytes
            (byteCount - 1)
            overlong
            cs'
            ((acc `shiftL` 6) .|. fromIntegral cn .&. 0x3F)
    moreBytes _ _ cs' _ =
      replacementChar : go cs'

    replacementChar = '\xfffd'

-- | Encode 'String' to a list of UTF8-encoded octets
--
-- Code-points in the @U+D800@-@U+DFFF@ range will be encoded
-- as the replacement character (i.e. @U+FFFD@).
--
-- See also 'decodeUtf8'
encodeStringUtf8 :: String -> [Word8]
encodeStringUtf8 [] = []
encodeStringUtf8 (c : cs)
  | c <= '\x07F' =
      w8
        : encodeStringUtf8 cs
  | c <= '\x7FF' =
      (0xC0 .|. w8ShiftR 6)
        : (0x80 .|. (w8 .&. 0x3F))
        : encodeStringUtf8 cs
  | c <= '\xD7FF' =
      (0xE0 .|. w8ShiftR 12)
        : (0x80 .|. (w8ShiftR 6 .&. 0x3F))
        : (0x80 .|. (w8 .&. 0x3F))
        : encodeStringUtf8 cs
  | c <= '\xDFFF' =
      0xEF
        : 0xBF
        : 0xBD -- U+FFFD
        : encodeStringUtf8 cs
  | c <= '\xFFFF' =
      (0xE0 .|. w8ShiftR 12)
        : (0x80 .|. (w8ShiftR 6 .&. 0x3F))
        : (0x80 .|. (w8 .&. 0x3F))
        : encodeStringUtf8 cs
  | otherwise =
      (0xf0 .|. w8ShiftR 18)
        : (0x80 .|. (w8ShiftR 12 .&. 0x3F))
        : (0x80 .|. (w8ShiftR 6 .&. 0x3F))
        : (0x80 .|. (w8 .&. 0x3F))
        : encodeStringUtf8 cs
  where
    w8 = fromIntegral (ord c) :: Word8
    w8ShiftR :: Int -> Word8
    w8ShiftR = fromIntegral . shiftR (ord c)
