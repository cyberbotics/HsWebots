{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.Keyboard where
import           Control.Exception.Safe         ( try
                                                , SomeException(..)
                                                , throwIO
                                                )
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String

import qualified Language.C.Inline         as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types          as C
import Language.C.Inline.Cpp (cppTypePairs)
import Foreign.C.Types
import Control.Monad (forM_,forM)
import qualified Codec.Picture             as I

import qualified Data.Vector.Storable      as V
import qualified Foreign.ForeignPtr        as F
import qualified Foreign.Ptr               as F
import qualified Data.ByteString.Internal  as BSI

import Webots.Types

C.context $ C.baseCtx `mappend` cppTypePairs typeMaps
  
C.include "<math.h>"
C.include "<stdio.h>"
C.include "<stdlib.h>"
C.include "<webots/keyboard.h>"

wb_keyboard_enable :: CInt -> IO () 
wb_keyboard_enable sampling_period =
   [C.exp| void { wb_keyboard_enable($(int sampling_period)) } |]

wb_keyboard_disable :: IO () 
wb_keyboard_disable  =
   [C.exp| void { wb_keyboard_disable() } |]

wb_keyboard_get_sampling_period :: IO CInt 
wb_keyboard_get_sampling_period  =
   [C.exp| int { wb_keyboard_get_sampling_period() } |]

wb_keyboard_get_key :: IO CInt 
wb_keyboard_get_key  =
   [C.exp| int { wb_keyboard_get_key() } |]
