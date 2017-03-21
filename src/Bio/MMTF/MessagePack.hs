{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bio.MMTF.MessagePack where

import Bio.MMTF.Type
import Bio.MMTF.Decode
import Bio.MMTF.Decode.MessagePack

import Data.MessagePack (MessagePack (..))

instance MessagePack MMTF where
  toObject = undefined
  fromObject obj = do mp <- transformObjectMap obj
                      f <- formatData mp
                      s <- structureData mp
                      m <- modelData mp
                      c <- chainData mp
                      g <- groupData mp
                      a <- atomData mp
                      pure $ MMTF f s m c g a
