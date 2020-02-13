{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.PositionSensor where
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
C.include "<webots/position_sensor.h>"

wb_position_sensor_enable :: WbDeviceTag -> CInt -> IO () 
wb_position_sensor_enable tag sampling_period =
   [C.exp| void { wb_position_sensor_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_position_sensor_disable :: WbDeviceTag -> IO () 
wb_position_sensor_disable tag =
   [C.exp| void { wb_position_sensor_disable($(WbDeviceTag tag)) } |]

wb_position_sensor_get_sampling_period :: WbDeviceTag -> IO CInt 
wb_position_sensor_get_sampling_period tag =
   [C.exp| int { wb_position_sensor_get_sampling_period($(WbDeviceTag tag)) } |]

wb_position_sensor_get_value :: WbDeviceTag -> IO CDouble 
wb_position_sensor_get_value tag =
   [C.exp| double { wb_position_sensor_get_value($(WbDeviceTag tag)) } |]

wb_position_sensor_get_type :: WbDeviceTag -> IO WbJointType 
wb_position_sensor_get_type tag =
   [C.exp| WbJointType { wb_position_sensor_get_type($(WbDeviceTag tag)) } |]

wb_position_sensor_get_motor :: WbDeviceTag -> IO WbDeviceTag 
wb_position_sensor_get_motor tag =
   [C.exp| WbDeviceTag { wb_position_sensor_get_motor($(WbDeviceTag tag)) } |]

wb_position_sensor_get_brake :: WbDeviceTag -> IO WbDeviceTag 
wb_position_sensor_get_brake tag =
   [C.exp| WbDeviceTag { wb_position_sensor_get_brake($(WbDeviceTag tag)) } |]
