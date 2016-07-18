{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE AllowAmbiguousTypes #-}
{-#LANGUAGE ScopedTypeVariables #-}

import Criterion.Main
import Criterion.Types
import TestCases
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array (mallocArray, peekArray, pokeArray)
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Generic
import Foreign.Storable.Generic.Internal

import GHC.Generics (from, to)

import GHC.Exts

import Control.DeepSeq
import Data.Proxy


mallocFree :: forall a. (Storable a) => a -> IO ()
mallocFree a = do
    ptr <- mallocBytes $ (sizeOf a)
    free ptr

singularTests = 
   [ bgroup "mallocfree" $ 
      [ bgroup "Handwritten" $
          [ bench "C1" $ nfIO (mallocFree c1hw_def)
          , bench "C2" $ nfIO (mallocFree c2hw_def)
          , bench "C3" $ nfIO (mallocFree c3hw_def)
          , bench "C4" $ nfIO (mallocFree c4hw_def)
          , bench "C5" $ nfIO (mallocFree c5hw_def)
          ]
      , bgroup "GStorable" $
          [ bench "C1" $ nfIO (mallocFree c1_def)
          , bench "C2" $ nfIO (mallocFree c2_def)
          , bench "C3" $ nfIO (mallocFree c3_def)
          , bench "C4" $ nfIO (mallocFree c4_def)
          , bench "C5" $ nfIO (mallocFree c5_def)
          ]
      ]
   , bgroup "sizeOf" $ 
       [ bgroup "Handwritten" $
           [ bench "C1" $ nf sizeOf c1hw_def
           , bench "C2" $ nf sizeOf c2hw_def
           , bench "C3" $ nf sizeOf c3hw_def
           , bench "C4" $ nf sizeOf c4hw_def
           , bench "C5" $ nf sizeOf c5hw_def
           ]
       
       , bgroup "GStorable" $
           [ bench "C1" $ nf sizeOf c1_def
           , bench "C2" $ nf sizeOf c2_def
           , bench "C3" $ nf sizeOf c3_def
           , bench "C4" $ nf sizeOf c4_def
           , bench "C5" $ nf sizeOf c5_def
           ]
       ]
   , bgroup "alignment" $ 
       [ bgroup "Handwritten" $
           [ bench "C1" $ nf alignment c1hw_def
           , bench "C2" $ nf alignment c2hw_def
           , bench "C3" $ nf alignment c3hw_def
           , bench "C4" $ nf alignment c4hw_def
           , bench "C5" $ nf alignment c5hw_def
           ]
       , bgroup "GStorable" $
           [ bench "C1" $ nf alignment c1_def
           , bench "C2" $ nf alignment c2_def
           , bench "C3" $ nf alignment c3_def
           , bench "C4" $ nf alignment c4_def
           , bench "C5" $ nf alignment c5_def
           ]
       ]
   , bgroup "peek" $
       [ bgroup "Handwritten" $
           [ env (malloc @C1hw) $ \ptr -> bench "C1" $ nfIO (peek ptr)
           , env (malloc @C2hw) $ \ptr -> bench "C2" $ nfIO (peek ptr)
           , env (malloc @C3hw) $ \ptr -> bench "C3" $ nfIO (peek ptr)
           , env (malloc @C4hw) $ \ptr -> bench "C4" $ nfIO (peek ptr)
           , env (malloc @C5hw) $ \ptr -> bench "C5" $ nfIO (peek ptr)
           ]
       , bgroup "GStorable" $
           [ env (malloc @C1  ) $ \ptr -> bench "C1" $ nfIO (peek ptr)
           , env (malloc @C2  ) $ \ptr -> bench "C2" $ nfIO (peek ptr)
           , env (malloc @C3  ) $ \ptr -> bench "C3" $ nfIO (peek ptr)
           , env (malloc @C4  ) $ \ptr -> bench "C4" $ nfIO (peek ptr)
           , env (malloc @C5  ) $ \ptr -> bench "C5" $ nfIO (peek ptr)
           ]
       ] 
  , bgroup "poke" $
      [ bgroup "Handwritten" $     
          [ env malloc $ \ptr -> bench "C1" $ nfIO (poke ptr c1hw_def) 
          , env malloc $ \ptr -> bench "C2" $ nfIO (poke ptr c2hw_def)
          , env malloc $ \ptr -> bench "C3" $ nfIO (poke ptr c3hw_def)
          , env malloc $ \ptr -> bench "C4" $ nfIO (poke ptr c4hw_def)
          , env malloc $ \ptr -> bench "C5" $ nfIO (poke ptr c5hw_def)
          ]
      , bgroup "GStorable" $
          [ env malloc $ \ptr -> bench "C1" $ nfIO (poke ptr c1_def) 
          , env malloc $ \ptr -> bench "C2" $ nfIO (poke ptr c2_def)
          , env malloc $ \ptr -> bench "C3" $ nfIO (poke ptr c3_def)
          , env malloc $ \ptr -> bench "C4" $ nfIO (poke ptr c4_def)
          , env malloc $ \ptr -> bench "C5" $ nfIO (poke ptr c5_def)
          ]
      ]
  ]
-- With get number of fields it forces the rewrite rules here.
getterTest = 
    [ bgroup "C1" $ 
        [--bench "flipped 0" $ nf ((flip internalGetOffset) 0) (from c1_def) 
        bench "0" $ nf (getOffset (c1_def)) 0
        ]
    , bgroup "C2" $
        [--bench "flipped 0" $ nf ((flip internalGetOffset) 0) (from c2_def) 
        bench "0" $ nf (getOffset (c2_def)) 0
        --,bench "flipped 1" $ nf ((flip internalGetOffset) 1) (from c2_def) 
        ,bench "1" $ nf (getOffset (c2_def)) 1
        --,bench "flipped 2" $ nf ((flip internalGetOffset) 2) (from c2_def) 
        ,bench "2" $ nf (getOffset (c2_def)) 2
        ]
    ]

getterRuntimeIxTest = do 
     putStrLn "provide an index"
     n <- read <$> getLine
     return [bench ("runtime ix: " ++ show n) $ nf (getOffset (c2_def)) n]


-- Our benchmark harness.
main = do 
    defaultMain $ [ bgroup "internals" getterTest, bgroup "comparison" singularTests]
    -- defaultMain $ singularTests

