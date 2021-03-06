{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Webots.Types where

import Foreign.Ptr
import Data.Word
import Foreign.C.Types
--import qualified Language.C.Inline as C
--import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Language.Haskell.TH

data WbField
data WbNode
data WbMutex
data WbImage
data WbRadarTarget
type WbRadioEvent = Ptr ()
type WbRadioMessage = Ptr ()

data WbSimulationMode'
  = WB_SUPERVISOR_SIMULATION_MODE_PAUSE
  | WB_SUPERVISOR_SIMULATION_MODE_REAL_TIME
  | WB_SUPERVISOR_SIMULATION_MODE_RUN
  | WB_SUPERVISOR_SIMULATION_MODE_FAST
  deriving (Show,Eq)


data WbCameraRecognitionObject = WbCameraRecognitionObject
  { obj_id :: CInt
  , obj_position :: (CDouble,CDouble,CDouble)
  , obj_orientation :: (CDouble,CDouble,CDouble,CDouble)
  , obj_size :: (CDouble,CDouble)
  , obj_position_on_image :: (CInt,CInt)
  , obj_size_on_image :: (CInt,CInt)
  , obj_number_of_colors :: CInt
  , obj_colors :: [CDouble]
  , obj_model :: String
  } deriving (Show,Eq)

data WbMouseState = WbMouseState
  { mouse_left :: CBool
  , mouse_middle :: CBool
  , mouse_right :: CBool
  , mouse_u :: CDouble
  , mouse_v :: CDouble
  , mouse_x :: CDouble
  , mouse_y :: CDouble
  , mouse_z :: CDouble
  } deriving (Show,Eq)


type WbFieldRef = Ptr WbField
type WbNodeRef = Ptr WbNode
type WbMutexRef = Ptr WbMutex
type WbImageRef = Ptr WbImage
type WbDeviceTag = CUShort
type WbSimulationMode = CInt
type WbNodeType = CInt
type WbFieldType = CInt
type WbUserInputEvent = CInt
type WbRobotMode = CInt
type WbJointType = CInt

type WbuDriverIndicatorState = CInt
type WbuDriverControlMode = CInt
type WbuDriverWiperMode = CInt

type WbTouchSensorType = CInt
type WbGpsCoordinateSystem = CInt
type WbDistanceSensorType = CInt

typeMaps :: [(C.CIdentifier,TypeQ)]
typeMaps =
  [ ("WbFieldRef", [t|Ptr WbField|])
  , ("WbNodeRef", [t|Ptr WbNode|])
  , ("WbMutexRef", [t|Ptr WbMutex|])
  , ("WbDeviceTag", [t|CUShort|])
  , ("WbSimulationMode", [t|CInt|])
  , ("WbNodeType", [t|CInt|])
  , ("WbFieldType", [t|CInt|])
  , ("WbUserInputEvent", [t|CInt|])
  , ("WbRobotMode", [t|CInt|])
  , ("WbuDriverIndicatorState", [t|CInt|])
  , ("WbuDriverControlMode",    [t|CInt|])
  , ("WbuDriverWiperMode",      [t|CInt|])
  , ("WbCameraRecognitionObject",      [t|WbCameraRecognitionObject|])
  , ("WbTouchSensorType",      [t|CInt|])
  , ("WbRadarTarget",      [t|WbRadarTarget|])
  , ("WbRadioEvent",      [t|Ptr ()|])
  , ("WbRadioMessage",      [t|Ptr ()|])
  , ("WbJointType",      [t|CInt|])
  , ("WbMouseState",      [t|WbMouseState|])
  , ("WbGpsCoordinateSystem",      [t|CInt|])
  , ("WbDistanceSensorType",       [t|CInt|])
  , ("WbImageRef",       [t|Ptr WbImage|])
  ]
