{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.Emitter where
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
C.include "<webots/emitter.h>"

wb_emitter_send :: WbDeviceTag -> Ptr () -> CInt -> IO CInt 
wb_emitter_send tag dat size =
   [C.exp| int { wb_emitter_send($(WbDeviceTag tag), $(void* dat), $(int size)) } |]

wb_emitter_get_buffer_size :: WbDeviceTag -> IO CInt 
wb_emitter_get_buffer_size tag =
   [C.exp| int { wb_emitter_get_buffer_size($(WbDeviceTag tag)) } |]

wb_emitter_set_channel :: WbDeviceTag -> CInt -> IO () 
wb_emitter_set_channel tag channel =
   [C.exp| void { wb_emitter_set_channel($(WbDeviceTag tag), $(int channel)) } |]

wb_emitter_get_channel :: WbDeviceTag -> IO CInt 
wb_emitter_get_channel tag =
   [C.exp| int { wb_emitter_get_channel($(WbDeviceTag tag)) } |]

wb_emitter_get_range :: WbDeviceTag -> IO CDouble 
wb_emitter_get_range tag =
   [C.exp| double { wb_emitter_get_range($(WbDeviceTag tag)) } |]

wb_emitter_set_range :: WbDeviceTag -> CDouble -> IO () 
wb_emitter_set_range tag range =
   [C.exp| void { wb_emitter_set_range($(WbDeviceTag tag), $(double range)) } |]
