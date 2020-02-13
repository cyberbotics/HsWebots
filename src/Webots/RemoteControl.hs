{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.RemoteControl where
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
C.include "<webots/remote_control.h>"

wb_remote_control_custom_function :: Ptr CChar -> IO () 
wb_remote_control_custom_function xxx =
   [C.exp| void { wb_remote_control_custom_function($(char* xxx)) } |]

wbr_robot_battery_sensor_set_value :: CDouble -> IO () 
wbr_robot_battery_sensor_set_value value =
   [C.exp| void { wbr_robot_battery_sensor_set_value($(double value)) } |]

wbr_differential_wheels_set_encoders :: CDouble -> CDouble -> IO () 
wbr_differential_wheels_set_encoders left right =
   [C.exp| void { wbr_differential_wheels_set_encoders($(double left), $(double right)) } |]

wbr_accelerometer_set_values :: WbDeviceTag -> Ptr CDouble -> IO () 
wbr_accelerometer_set_values tag values =
   [C.exp| void { wbr_accelerometer_set_values($(WbDeviceTag tag), $(const double* values)) } |]

wbr_camera_recognition_set_object :: WbDeviceTag -> Ptr WbCameraRecognitionObject -> CInt -> IO () 
wbr_camera_recognition_set_object tag objects object_number =
   [C.exp| void { wbr_camera_recognition_set_object($(WbDeviceTag tag), $(const WbCameraRecognitionObject* objects), $(int object_number)) } |]

wbr_compass_set_values :: WbDeviceTag -> Ptr CDouble -> IO () 
wbr_compass_set_values tag values =
   [C.exp| void { wbr_compass_set_values($(WbDeviceTag tag), $(const double* values)) } |]

wbr_distance_sensor_set_value :: WbDeviceTag -> CDouble -> IO () 
wbr_distance_sensor_set_value tag value =
   [C.exp| void { wbr_distance_sensor_set_value($(WbDeviceTag tag), $(double value)) } |]

wbr_gps_set_values :: WbDeviceTag -> Ptr CDouble -> IO () 
wbr_gps_set_values tag values =
   [C.exp| void { wbr_gps_set_values($(WbDeviceTag tag), $(const double* values)) } |]

wbr_gps_set_speed :: WbDeviceTag -> CDouble -> IO () 
wbr_gps_set_speed tag speed =
   [C.exp| void { wbr_gps_set_speed($(WbDeviceTag tag), $(double speed)) } |]

wbr_gyro_set_values :: WbDeviceTag -> Ptr CDouble -> IO () 
wbr_gyro_set_values tag values =
   [C.exp| void { wbr_gyro_set_values($(WbDeviceTag tag), $(const double* values)) } |]

wbr_inertial_unit_set_value :: WbDeviceTag -> CDouble -> IO () 
wbr_inertial_unit_set_value tag value =
   [C.exp| void { wbr_inertial_unit_set_value($(WbDeviceTag tag), $(double value)) } |]

wbr_light_sensor_set_value :: WbDeviceTag -> CDouble -> IO () 
wbr_light_sensor_set_value tag value =
   [C.exp| void { wbr_light_sensor_set_value($(WbDeviceTag tag), $(double value)) } |]

wbr_microphone_set_buffer :: WbDeviceTag -> Ptr CChar -> CInt -> IO () 
wbr_microphone_set_buffer tag buffer size =
   [C.exp| void { wbr_microphone_set_buffer($(WbDeviceTag tag), $(const char* buffer), $(int size)) } |]

wbr_motor_set_position_feedback :: WbDeviceTag -> CDouble -> IO () 
wbr_motor_set_position_feedback tag value =
   [C.exp| void { wbr_motor_set_position_feedback($(WbDeviceTag tag), $(double value)) } |]

wbr_motor_set_force_feedback :: WbDeviceTag -> CDouble -> IO () 
wbr_motor_set_force_feedback tag value =
   [C.exp| void { wbr_motor_set_force_feedback($(WbDeviceTag tag), $(double value)) } |]

wbr_motor_set_torque_feedback :: WbDeviceTag -> CDouble -> IO () 
wbr_motor_set_torque_feedback tag value =
   [C.exp| void { wbr_motor_set_torque_feedback($(WbDeviceTag tag), $(double value)) } |]

wbr_position_sensor_set_value :: WbDeviceTag -> CDouble -> IO () 
wbr_position_sensor_set_value tag value =
   [C.exp| void { wbr_position_sensor_set_value($(WbDeviceTag tag), $(double value)) } |]

wbr_radar_set_targets :: WbDeviceTag -> Ptr WbRadarTarget -> CInt -> IO () 
wbr_radar_set_targets tag targets target_number =
   [C.exp| void { wbr_radar_set_targets($(WbDeviceTag tag), $(const WbRadarTarget* targets), $(int target_number)) } |]

wbr_touch_sensor_set_value :: WbDeviceTag -> CDouble -> IO () 
wbr_touch_sensor_set_value tag value =
   [C.exp| void { wbr_touch_sensor_set_value($(WbDeviceTag tag), $(double value)) } |]

wbr_touch_sensor_set_values :: WbDeviceTag -> Ptr CDouble -> IO () 
wbr_touch_sensor_set_values tag values =
   [C.exp| void { wbr_touch_sensor_set_values($(WbDeviceTag tag), $(const double* values)) } |]

wbr_display_save_image :: WbDeviceTag -> CInt -> CInt -> CInt -> Ptr CChar -> IO () 
wbr_display_save_image tag id width height image =
   [C.exp| void { wbr_display_save_image($(WbDeviceTag tag), $(int id), $(int width), $(int height), $(char* image)) } |]

wbr_camera_set_image :: WbDeviceTag -> Ptr CChar -> IO () 
wbr_camera_set_image tag image =
   [C.exp| void { wbr_camera_set_image($(WbDeviceTag tag), $(char* image)) } |]
