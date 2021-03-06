{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.Joystick where
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
C.include "<webots/joystick.h>"

wb_joystick_enable :: CInt -> IO () 
wb_joystick_enable sampling_period =
   [C.exp| void { wb_joystick_enable($(int sampling_period)) } |]

wb_joystick_disable :: IO () 
wb_joystick_disable  =
   [C.exp| void { wb_joystick_disable() } |]

wb_joystick_get_sampling_period :: IO CInt 
wb_joystick_get_sampling_period  =
   [C.exp| int { wb_joystick_get_sampling_period() } |]

wb_joystick_is_connected :: IO CBool 
wb_joystick_is_connected  =
   [C.exp| bool { wb_joystick_is_connected() } |]

wb_joystick_get_model :: IO String 
wb_joystick_get_model  =
   peekCString =<< [C.exp| const char* { wb_joystick_get_model() } |]

wb_joystick_get_number_of_axes :: IO CInt 
wb_joystick_get_number_of_axes  =
   [C.exp| int { wb_joystick_get_number_of_axes() } |]

wb_joystick_get_axis_value :: CInt -> IO CInt 
wb_joystick_get_axis_value axis =
   [C.exp| int { wb_joystick_get_axis_value($(int axis)) } |]

wb_joystick_get_number_of_povs :: IO CInt 
wb_joystick_get_number_of_povs  =
   [C.exp| int { wb_joystick_get_number_of_povs() } |]

wb_joystick_get_pov_value :: CInt -> IO CInt 
wb_joystick_get_pov_value pov =
   [C.exp| int { wb_joystick_get_pov_value($(int pov)) } |]

wb_joystick_get_pressed_button :: IO CInt 
wb_joystick_get_pressed_button  =
   [C.exp| int { wb_joystick_get_pressed_button() } |]

wb_joystick_set_constant_force :: CInt -> IO () 
wb_joystick_set_constant_force level =
   [C.exp| void { wb_joystick_set_constant_force($(int level)) } |]

wb_joystick_set_constant_force_duration :: CDouble -> IO () 
wb_joystick_set_constant_force_duration duration =
   [C.exp| void { wb_joystick_set_constant_force_duration($(double duration)) } |]

wb_joystick_set_auto_centering_gain :: CDouble -> IO () 
wb_joystick_set_auto_centering_gain gain =
   [C.exp| void { wb_joystick_set_auto_centering_gain($(double gain)) } |]

wb_joystick_set_resistance_gain :: CDouble -> IO () 
wb_joystick_set_resistance_gain gain =
   [C.exp| void { wb_joystick_set_resistance_gain($(double gain)) } |]

wb_joystick_set_force_axis :: CInt -> IO () 
wb_joystick_set_force_axis axis =
   [C.exp| void { wb_joystick_set_force_axis($(int axis)) } |]
