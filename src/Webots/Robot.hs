{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.Robot where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Language.C.Inline.Cpp (cppTypePairs)
import Foreign.C.Types
import Control.Monad (forM_,forM)

import Webots.Types

C.context $ C.baseCtx `mappend` cppTypePairs typeMaps
  
C.include "<math.h>"
C.include "<stdio.h>"
C.include "<stdlib.h>"
C.include "<webots/robot.h>"
C.include "<webots/supervisor.h>"

wb_robot_init :: IO CInt 
wb_robot_init  =
   [C.exp| int { wb_robot_init() } |]

wb_robot_step :: CInt -> IO CInt 
wb_robot_step duration =
   [C.exp| int { wb_robot_step($(int duration)) } |]

wb_robot_wait_for_user_input_event :: WbUserInputEvent -> CInt -> IO WbUserInputEvent 
wb_robot_wait_for_user_input_event event_type timeout =
   [C.exp| WbUserInputEvent { wb_robot_wait_for_user_input_event($(WbUserInputEvent event_type), $(int timeout)) } |]

wb_robot_cleanup :: IO () 
wb_robot_cleanup  =
   [C.exp| void { wb_robot_cleanup() } |]

wb_robot_get_time :: IO CDouble 
wb_robot_get_time  =
   [C.exp| double { wb_robot_get_time() } |]

wb_robot_get_name :: IO String 
wb_robot_get_name  =
   peekCString =<< [C.exp| const char* { wb_robot_get_name() } |]

wb_robot_get_model :: IO String 
wb_robot_get_model  =
   peekCString =<< [C.exp| const char* { wb_robot_get_model() } |]

wb_robot_get_custom_data :: IO String 
wb_robot_get_custom_data  =
   peekCString =<< [C.exp| const char* { wb_robot_get_custom_data() } |]

wb_robot_set_custom_data :: String -> IO () 
wb_robot_set_custom_data dat =
  withCString dat $ \dat' ->
  [C.exp| void { wb_robot_set_custom_data($(const char* dat')) } |]

wb_robot_get_mode :: IO WbRobotMode 
wb_robot_get_mode  =
   [C.exp| WbRobotMode { wb_robot_get_mode() } |]

wb_robot_set_mode :: WbRobotMode -> Ptr CChar -> IO () 
wb_robot_set_mode mode args =
   [C.exp| void { wb_robot_set_mode($(WbRobotMode mode), $(char* args)) } |]

wb_robot_get_synchronization :: IO CBool 
wb_robot_get_synchronization  =
   [C.exp| bool { wb_robot_get_synchronization() } |]

wb_robot_get_supervisor :: IO CBool 
wb_robot_get_supervisor  =
   [C.exp| bool { wb_robot_get_supervisor() } |]

wb_robot_get_project_path :: IO String 
wb_robot_get_project_path  =
   peekCString =<< [C.exp| const char* { wb_robot_get_project_path() } |]

wb_robot_get_world_path :: IO String 
wb_robot_get_world_path  =
   peekCString =<< [C.exp| const char* { wb_robot_get_world_path() } |]

wb_robot_get_basic_time_step :: IO CDouble 
wb_robot_get_basic_time_step  =
   [C.exp| double { wb_robot_get_basic_time_step() } |]

wb_robot_get_device :: String -> IO WbDeviceTag 
wb_robot_get_device name =
  withCString name $ \name' ->
  [C.exp| WbDeviceTag { wb_robot_get_device($(const char* name')) } |]

wb_robot_get_controller_name :: IO String 
wb_robot_get_controller_name  =
   peekCString =<< [C.exp| const char* { wb_robot_get_controller_name() } |]

wb_robot_get_controller_arguments :: IO String 
wb_robot_get_controller_arguments  =
   peekCString =<< [C.exp| const char* { wb_robot_get_controller_arguments() } |]

wb_robot_get_number_of_devices :: IO CInt 
wb_robot_get_number_of_devices  =
   [C.exp| int { wb_robot_get_number_of_devices() } |]

wb_robot_get_device_by_index :: CInt -> IO WbDeviceTag 
wb_robot_get_device_by_index index =
   [C.exp| WbDeviceTag { wb_robot_get_device_by_index($(int index)) } |]

wb_robot_get_type :: IO WbNodeType 
wb_robot_get_type  =
   [C.exp| WbNodeType { wb_robot_get_type() } |]

wb_robot_battery_sensor_enable :: CInt -> IO () 
wb_robot_battery_sensor_enable sampling_period =
   [C.exp| void { wb_robot_battery_sensor_enable($(int sampling_period)) } |]

wb_robot_battery_sensor_disable :: IO () 
wb_robot_battery_sensor_disable  =
   [C.exp| void { wb_robot_battery_sensor_disable() } |]

wb_robot_battery_sensor_get_sampling_period :: IO CInt 
wb_robot_battery_sensor_get_sampling_period  =
   [C.exp| int { wb_robot_battery_sensor_get_sampling_period() } |]

wb_robot_battery_sensor_get_value :: IO CDouble 
wb_robot_battery_sensor_get_value  =
   [C.exp| double { wb_robot_battery_sensor_get_value() } |]

--wb_robot_task_new :: () -> () -> IO () 
--wb_robot_task_new task param =
--   [C.exp| void { wb_robot_task_new($(void task), $(void param)) } |]

wb_robot_mutex_new :: IO WbMutexRef 
wb_robot_mutex_new  =
   [C.exp| WbMutexRef { wb_robot_mutex_new() } |]

wb_robot_mutex_lock :: WbMutexRef -> IO () 
wb_robot_mutex_lock mutex =
   [C.exp| void { wb_robot_mutex_lock($(WbMutexRef mutex)) } |]

wb_robot_mutex_unlock :: WbMutexRef -> IO () 
wb_robot_mutex_unlock mutex =
   [C.exp| void { wb_robot_mutex_unlock($(WbMutexRef mutex)) } |]

wb_robot_mutex_delete :: WbMutexRef -> IO () 
wb_robot_mutex_delete mutex =
   [C.exp| void { wb_robot_mutex_delete($(WbMutexRef mutex)) } |]

wb_robot_pin_to_static_environment :: CBool -> IO () 
wb_robot_pin_to_static_environment pin =
   [C.exp| void { wb_robot_pin_to_static_environment($(bool pin)) } |]

