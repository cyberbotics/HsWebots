{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.Motor where
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
C.include "<webots/motor.h>"

wb_motor_set_position :: WbDeviceTag -> CDouble -> IO () 
wb_motor_set_position tag position =
   [C.exp| void { wb_motor_set_position($(WbDeviceTag tag), $(double position)) } |]

wb_motor_set_acceleration :: WbDeviceTag -> CDouble -> IO () 
wb_motor_set_acceleration tag acceleration =
   [C.exp| void { wb_motor_set_acceleration($(WbDeviceTag tag), $(double acceleration)) } |]

wb_motor_set_velocity :: WbDeviceTag -> CDouble -> IO () 
wb_motor_set_velocity tag velocity =
   [C.exp| void { wb_motor_set_velocity($(WbDeviceTag tag), $(double velocity)) } |]

wb_motor_set_force :: WbDeviceTag -> CDouble -> IO () 
wb_motor_set_force tag force =
   [C.exp| void { wb_motor_set_force($(WbDeviceTag tag), $(double force)) } |]

wb_motor_set_torque :: WbDeviceTag -> CDouble -> IO () 
wb_motor_set_torque tag torque =
   [C.exp| void { wb_motor_set_torque($(WbDeviceTag tag), $(double torque)) } |]

wb_motor_set_available_force :: WbDeviceTag -> CDouble -> IO () 
wb_motor_set_available_force tag force =
   [C.exp| void { wb_motor_set_available_force($(WbDeviceTag tag), $(double force)) } |]

wb_motor_set_available_torque :: WbDeviceTag -> CDouble -> IO () 
wb_motor_set_available_torque tag torque =
   [C.exp| void { wb_motor_set_available_torque($(WbDeviceTag tag), $(double torque)) } |]

wb_motor_set_control_pid :: WbDeviceTag -> CDouble -> CDouble -> CDouble -> IO () 
wb_motor_set_control_pid tag p i d =
   [C.exp| void { wb_motor_set_control_pid($(WbDeviceTag tag), $(double p), $(double i), $(double d)) } |]

wb_motor_enable_force_feedback :: WbDeviceTag -> CInt -> IO () 
wb_motor_enable_force_feedback tag sampling_period =
   [C.exp| void { wb_motor_enable_force_feedback($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_motor_disable_force_feedback :: WbDeviceTag -> IO () 
wb_motor_disable_force_feedback tag =
   [C.exp| void { wb_motor_disable_force_feedback($(WbDeviceTag tag)) } |]

wb_motor_get_force_feedback_sampling_period :: WbDeviceTag -> IO CInt 
wb_motor_get_force_feedback_sampling_period tag =
   [C.exp| int { wb_motor_get_force_feedback_sampling_period($(WbDeviceTag tag)) } |]

wb_motor_get_force_feedback :: WbDeviceTag -> IO CDouble 
wb_motor_get_force_feedback tag =
   [C.exp| double { wb_motor_get_force_feedback($(WbDeviceTag tag)) } |]

wb_motor_enable_torque_feedback :: WbDeviceTag -> CInt -> IO () 
wb_motor_enable_torque_feedback tag sampling_period =
   [C.exp| void { wb_motor_enable_torque_feedback($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_motor_disable_torque_feedback :: WbDeviceTag -> IO () 
wb_motor_disable_torque_feedback tag =
   [C.exp| void { wb_motor_disable_torque_feedback($(WbDeviceTag tag)) } |]

wb_motor_get_torque_feedback_sampling_period :: WbDeviceTag -> IO CInt 
wb_motor_get_torque_feedback_sampling_period tag =
   [C.exp| int { wb_motor_get_torque_feedback_sampling_period($(WbDeviceTag tag)) } |]

wb_motor_get_torque_feedback :: WbDeviceTag -> IO CDouble 
wb_motor_get_torque_feedback tag =
   [C.exp| double { wb_motor_get_torque_feedback($(WbDeviceTag tag)) } |]

wb_motor_get_type :: WbDeviceTag -> IO WbJointType 
wb_motor_get_type tag =
   [C.exp| WbJointType { wb_motor_get_type($(WbDeviceTag tag)) } |]

wb_motor_get_target_position :: WbDeviceTag -> IO CDouble 
wb_motor_get_target_position tag =
   [C.exp| double { wb_motor_get_target_position($(WbDeviceTag tag)) } |]

wb_motor_get_min_position :: WbDeviceTag -> IO CDouble 
wb_motor_get_min_position tag =
   [C.exp| double { wb_motor_get_min_position($(WbDeviceTag tag)) } |]

wb_motor_get_max_position :: WbDeviceTag -> IO CDouble 
wb_motor_get_max_position tag =
   [C.exp| double { wb_motor_get_max_position($(WbDeviceTag tag)) } |]

wb_motor_get_velocity :: WbDeviceTag -> IO CDouble 
wb_motor_get_velocity tag =
   [C.exp| double { wb_motor_get_velocity($(WbDeviceTag tag)) } |]

wb_motor_get_max_velocity :: WbDeviceTag -> IO CDouble 
wb_motor_get_max_velocity tag =
   [C.exp| double { wb_motor_get_max_velocity($(WbDeviceTag tag)) } |]

wb_motor_get_acceleration :: WbDeviceTag -> IO CDouble 
wb_motor_get_acceleration tag =
   [C.exp| double { wb_motor_get_acceleration($(WbDeviceTag tag)) } |]

wb_motor_get_available_force :: WbDeviceTag -> IO CDouble 
wb_motor_get_available_force tag =
   [C.exp| double { wb_motor_get_available_force($(WbDeviceTag tag)) } |]

wb_motor_get_max_force :: WbDeviceTag -> IO CDouble 
wb_motor_get_max_force tag =
   [C.exp| double { wb_motor_get_max_force($(WbDeviceTag tag)) } |]

wb_motor_get_available_torque :: WbDeviceTag -> IO CDouble 
wb_motor_get_available_torque tag =
   [C.exp| double { wb_motor_get_available_torque($(WbDeviceTag tag)) } |]

wb_motor_get_max_torque :: WbDeviceTag -> IO CDouble 
wb_motor_get_max_torque tag =
   [C.exp| double { wb_motor_get_max_torque($(WbDeviceTag tag)) } |]

wb_motor_get_brake :: WbDeviceTag -> IO WbDeviceTag 
wb_motor_get_brake tag =
   [C.exp| WbDeviceTag { wb_motor_get_brake($(WbDeviceTag tag)) } |]

wb_motor_get_position_sensor :: WbDeviceTag -> IO WbDeviceTag 
wb_motor_get_position_sensor tag =
   [C.exp| WbDeviceTag { wb_motor_get_position_sensor($(WbDeviceTag tag)) } |]
