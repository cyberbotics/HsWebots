{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.Radio where
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
C.include "<webots/radio.h>"

wb_radio_message_new :: CInt -> String -> String -> IO WbRadioMessage 
wb_radio_message_new length body destination =
   withCString body $ \body' -> withCString destination $ \destination' -> [C.exp| WbRadioMessage { wb_radio_message_new($(int length), $(const char* body'), $(const char* destination')) } |]

wb_radio_message_delete :: WbRadioMessage -> IO () 
wb_radio_message_delete msg =
   [C.exp| void { wb_radio_message_delete($(WbRadioMessage msg)) } |]

wb_radio_message_get_destination :: WbRadioMessage -> IO String 
wb_radio_message_get_destination msg =
   peekCString =<< [C.exp| const char* { wb_radio_message_get_destination($(WbRadioMessage msg)) } |]

wb_radio_message_get_length :: WbRadioMessage -> IO CInt 
wb_radio_message_get_length msg =
   [C.exp| int { wb_radio_message_get_length($(WbRadioMessage msg)) } |]

wb_radio_message_get_body :: WbRadioMessage -> IO String 
wb_radio_message_get_body msg =
   peekCString =<< [C.exp| const char* { wb_radio_message_get_body($(WbRadioMessage msg)) } |]

wb_radio_enable :: WbDeviceTag -> CInt -> IO () 
wb_radio_enable tag sampling_period =
   [C.exp| void { wb_radio_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_radio_disable :: WbDeviceTag -> IO () 
wb_radio_disable tag =
   [C.exp| void { wb_radio_disable($(WbDeviceTag tag)) } |]

wb_radio_set_address :: WbDeviceTag -> String -> IO () 
wb_radio_set_address tag address =
   withCString address $ \address' -> [C.exp| void { wb_radio_set_address($(WbDeviceTag tag), $(const char* address')) } |]

wb_radio_get_address :: WbDeviceTag -> IO String 
wb_radio_get_address tag =
   peekCString =<< [C.exp| const char* { wb_radio_get_address($(WbDeviceTag tag)) } |]

wb_radio_set_frequency :: WbDeviceTag -> CDouble -> IO () 
wb_radio_set_frequency tag hz =
   [C.exp| void { wb_radio_set_frequency($(WbDeviceTag tag), $(double hz)) } |]

wb_radio_get_frequency :: WbDeviceTag -> IO CDouble 
wb_radio_get_frequency tag =
   [C.exp| double { wb_radio_get_frequency($(WbDeviceTag tag)) } |]

wb_radio_set_channel :: WbDeviceTag -> CInt -> IO () 
wb_radio_set_channel tag channel =
   [C.exp| void { wb_radio_set_channel($(WbDeviceTag tag), $(int channel)) } |]

wb_radio_get_channel :: WbDeviceTag -> IO CInt 
wb_radio_get_channel tag =
   [C.exp| int { wb_radio_get_channel($(WbDeviceTag tag)) } |]

wb_radio_set_bitrate :: WbDeviceTag -> CInt -> IO () 
wb_radio_set_bitrate tag bits_per_second =
   [C.exp| void { wb_radio_set_bitrate($(WbDeviceTag tag), $(int bits_per_second)) } |]

wb_radio_get_bitrate :: WbDeviceTag -> IO CInt 
wb_radio_get_bitrate tag =
   [C.exp| int { wb_radio_get_bitrate($(WbDeviceTag tag)) } |]

wb_radio_set_rx_sensitivity :: WbDeviceTag -> CDouble -> IO () 
wb_radio_set_rx_sensitivity tag dBm =
   [C.exp| void { wb_radio_set_rx_sensitivity($(WbDeviceTag tag), $(double dBm)) } |]

wb_radio_get_rx_sensitivity :: WbDeviceTag -> IO CDouble 
wb_radio_get_rx_sensitivity tag =
   [C.exp| double { wb_radio_get_rx_sensitivity($(WbDeviceTag tag)) } |]

wb_radio_set_tx_power :: WbDeviceTag -> CDouble -> IO () 
wb_radio_set_tx_power tag dBm =
   [C.exp| void { wb_radio_set_tx_power($(WbDeviceTag tag), $(double dBm)) } |]

wb_radio_get_tx_power :: WbDeviceTag -> IO CDouble 
wb_radio_get_tx_power tag =
   [C.exp| double { wb_radio_get_tx_power($(WbDeviceTag tag)) } |]

wb_radio_set_callback :: WbDeviceTag -> (WbRadioEvent -> IO ()) -> IO () 
wb_radio_set_callback tag callback = do
   callback' <- $(C.mkFunPtr [t| WbRadioEvent -> IO () |]) callback
   [C.exp| void { wb_radio_set_callback($(WbDeviceTag tag), $(void (*callback')(WbRadioEvent))) } |]

wb_radio_event_get_radio :: WbRadioEvent -> IO WbDeviceTag 
wb_radio_event_get_radio ev =
   [C.exp| WbDeviceTag { wb_radio_event_get_radio($(WbRadioEvent ev)) } |]

wb_radio_event_get_data :: WbRadioEvent -> IO String
wb_radio_event_get_data ev =
   peekCString =<< [C.exp| char* { wb_radio_event_get_data($(WbRadioEvent ev)) } |]

wb_radio_event_get_data_size :: WbRadioEvent -> IO CInt 
wb_radio_event_get_data_size ev =
   [C.exp| int { wb_radio_event_get_data_size($(WbRadioEvent ev)) } |]

wb_radio_event_get_emitter :: WbRadioEvent -> IO String
wb_radio_event_get_emitter ev =
   peekCString =<< [C.exp| char* { wb_radio_event_get_emitter($(WbRadioEvent ev)) } |]

wb_radio_event_get_rssi :: WbRadioEvent -> IO CDouble 
wb_radio_event_get_rssi ev =
   [C.exp| double { wb_radio_event_get_rssi($(WbRadioEvent ev)) } |]

wb_radio_send :: WbDeviceTag -> WbRadioMessage -> CDouble -> IO () 
wb_radio_send tag msg delay =
   [C.exp| void { wb_radio_send($(WbDeviceTag tag), $(WbRadioMessage msg), $(double delay)) } |]
