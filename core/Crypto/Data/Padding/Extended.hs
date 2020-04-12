{-# OPTIONS_GHC -Wall #-}

module Crypto.Data.Padding.Extended
  ( module Crypto.Data.Padding,
    unpadZero,
  )
where

import Crypto.Data.Padding
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

unpadZero :: ByteString -> ByteString
unpadZero = fst . BS.spanEnd (== 0)
