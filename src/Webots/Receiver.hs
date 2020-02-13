{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.Receiver where
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
C.include "<webots/receiver.h>"

wb_receiver_enable :: WbDeviceTag -> CInt -> IO () 
wb_receiver_enable tag sampling_period =
   [C.exp| void { wb_receiver_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_receiver_disable :: WbDeviceTag -> IO () 
wb_receiver_disable tag =
   [C.exp| void { wb_receiver_disable($(WbDeviceTag tag)) } |]

wb_receiver_get_sampling_period :: WbDeviceTag -> IO CInt 
wb_receiver_get_sampling_period tag =
   [C.exp| int { wb_receiver_get_sampling_period($(WbDeviceTag tag)) } |]

wb_receiver_set_channel :: WbDeviceTag -> CInt -> IO () 
wb_receiver_set_channel tag channel =
   [C.exp| void { wb_receiver_set_channel($(WbDeviceTag tag), $(int channel)) } |]

wb_receiver_get_channel :: WbDeviceTag -> IO CInt 
wb_receiver_get_channel tag =
   [C.exp| int { wb_receiver_get_channel($(WbDeviceTag tag)) } |]

wb_receiver_get_queue_length :: WbDeviceTag -> IO CInt 
wb_receiver_get_queue_length tag =
   [C.exp| int { wb_receiver_get_queue_length($(WbDeviceTag tag)) } |]

wb_receiver_next_packet :: WbDeviceTag -> IO () 
wb_receiver_next_packet tag =
   [C.exp| void { wb_receiver_next_packet($(WbDeviceTag tag)) } |]

wb_receiver_get_data_size :: WbDeviceTag -> IO CInt 
wb_receiver_get_data_size tag =
   [C.exp| int { wb_receiver_get_data_size($(WbDeviceTag tag)) } |]
