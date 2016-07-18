{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GHC.Generics (Generic)
import Foreign.Storable.Generic
import Foreign.Marshal.Alloc
import Foreign.Ptr (Ptr)
import Data.Int
import Control.DeepSeq



data C2 = C2 Int8 Int32 Int16 Int8 deriving (Show, Generic, GStorable, NFData)


c2_def = C2 3 10 8 0


data C2hw = C2hw Int8 Int32 Int16 Int8 deriving (Show,Generic, NFData)


c2hw_def = C2hw 3 10 8 0


instance Storable C2hw where
    sizeOf                         _ = 12
    alignment                      _ = 4
    peekByteOff ptr off              = C2hw 
        <$> (peekByteOff ptr off        :: IO Int8 ) 
        <*> (peekByteOff ptr (off + 4)  :: IO Int32) 
        <*> (peekByteOff ptr (off + 8)  :: IO Int16) 
        <*> (peekByteOff ptr (off + 10) :: IO Int8 ) 
                                       
    pokeByteOff ptr off (C2hw i8a i32 i16 i8b) = do
        pokeByteOff ptr  off       i8a
        pokeByteOff ptr (off + 4)  i32
        pokeByteOff ptr (off + 8)  i16
        pokeByteOff ptr (off + 10) i8b



main = do
    ptr <- malloc :: IO (Ptr C2)
    free ptr
    ptr2 <- malloc :: IO (Ptr C2hw)
    free ptr2 