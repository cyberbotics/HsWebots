{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.LightSensor where
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
C.include "<webots/light_sensor.h>"

wb_light_sensor_enable :: WbDeviceTag -> CInt -> IO () 
wb_light_sensor_enable tag sampling_period =
   [C.exp| void { wb_light_sensor_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_light_sensor_disable :: WbDeviceTag -> IO () 
wb_light_sensor_disable tag =
   [C.exp| void { wb_light_sensor_disable($(WbDeviceTag tag)) } |]

wb_light_sensor_get_sampling_period :: WbDeviceTag -> IO CInt 
wb_light_sensor_get_sampling_period tag =
   [C.exp| int { wb_light_sensor_get_sampling_period($(WbDeviceTag tag)) } |]

wb_light_sensor_get_value :: WbDeviceTag -> IO CDouble 
wb_light_sensor_get_value tag =
   [C.exp| double { wb_light_sensor_get_value($(WbDeviceTag tag)) } |]
