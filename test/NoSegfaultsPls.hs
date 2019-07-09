{-# language
        OverloadedStrings
  #-}

module Main (main) where

import Control.Exception (assert)
import qualified Data.Primitive.Pinned as P

main :: IO ()
main = do
  bsConvWorks

bsConvWorks :: IO ()
bsConvWorks = do
  let bs = "please no segfaults maam please...."
  let pinned = P.byteStringToPinned bs
  print pinned
  let bs' = P.pinnedToByteString pinned
  print bs'
  pure $ assert (bs == bs') ()
