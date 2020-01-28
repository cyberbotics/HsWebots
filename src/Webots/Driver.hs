{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.Driver where

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
C.include "<webots/vehicle/driver.h>"

wbu_driver_init :: IO () 
wbu_driver_init  =
   [C.exp| void { wbu_driver_init() } |]

wbu_driver_cleanup :: IO () 
wbu_driver_cleanup  =
   [C.exp| void { wbu_driver_cleanup() } |]

wbu_driver_step :: IO CInt 
wbu_driver_step  =
   [C.exp| int { wbu_driver_step() } |]

wbu_driver_set_steering_angle :: CDouble -> IO () 
wbu_driver_set_steering_angle steering_angle =
   [C.exp| void { wbu_driver_set_steering_angle($(double steering_angle)) } |]

wbu_driver_get_steering_angle :: IO CDouble 
wbu_driver_get_steering_angle  =
   [C.exp| double { wbu_driver_get_steering_angle() } |]

wbu_driver_set_cruising_speed :: CDouble -> IO () 
wbu_driver_set_cruising_speed speed =
   [C.exp| void { wbu_driver_set_cruising_speed($(double speed)) } |]

wbu_driver_get_target_cruising_speed :: IO CDouble 
wbu_driver_get_target_cruising_speed  =
   [C.exp| double { wbu_driver_get_target_cruising_speed() } |]

wbu_driver_get_current_speed :: IO CDouble 
wbu_driver_get_current_speed  =
   [C.exp| double { wbu_driver_get_current_speed() } |]

wbu_driver_set_throttle :: CDouble -> IO () 
wbu_driver_set_throttle throttle =
   [C.exp| void { wbu_driver_set_throttle($(double throttle)) } |]

wbu_driver_get_throttle :: IO CDouble 
wbu_driver_get_throttle  =
   [C.exp| double { wbu_driver_get_throttle() } |]

wbu_driver_set_brake_intensity :: CDouble -> IO () 
wbu_driver_set_brake_intensity intensity =
   [C.exp| void { wbu_driver_set_brake_intensity($(double intensity)) } |]

wbu_driver_get_brake_intensity :: IO CDouble 
wbu_driver_get_brake_intensity  =
   [C.exp| double { wbu_driver_get_brake_intensity() } |]

wbu_driver_set_indicator :: WbuDriverIndicatorState -> IO () 
wbu_driver_set_indicator state =
   [C.exp| void { wbu_driver_set_indicator($(WbuDriverIndicatorState state)) } |]

wbu_driver_set_hazard_flashers :: CBool -> IO () 
wbu_driver_set_hazard_flashers state =
   [C.exp| void { wbu_driver_set_hazard_flashers($(bool state)) } |]

wbu_driver_get_indicator :: IO WbuDriverIndicatorState 
wbu_driver_get_indicator  =
   [C.exp| WbuDriverIndicatorState { wbu_driver_get_indicator() } |]

wbu_driver_get_hazard_flashers :: IO CBool 
wbu_driver_get_hazard_flashers  =
   [C.exp| bool { wbu_driver_get_hazard_flashers() } |]

wbu_driver_set_dipped_beams :: CBool -> IO () 
wbu_driver_set_dipped_beams state =
   [C.exp| void { wbu_driver_set_dipped_beams($(bool state)) } |]

wbu_driver_set_antifog_lights :: CBool -> IO () 
wbu_driver_set_antifog_lights state =
   [C.exp| void { wbu_driver_set_antifog_lights($(bool state)) } |]

wbu_driver_get_dipped_beams :: IO CBool 
wbu_driver_get_dipped_beams  =
   [C.exp| bool { wbu_driver_get_dipped_beams() } |]

wbu_driver_get_antifog_lights :: IO CBool 
wbu_driver_get_antifog_lights  =
   [C.exp| bool { wbu_driver_get_antifog_lights() } |]

wbu_driver_get_rpm :: IO CDouble 
wbu_driver_get_rpm  =
   [C.exp| double { wbu_driver_get_rpm() } |]

wbu_driver_get_gear :: IO CInt 
wbu_driver_get_gear  =
   [C.exp| int { wbu_driver_get_gear() } |]

wbu_driver_set_gear :: CInt -> IO () 
wbu_driver_set_gear gear =
   [C.exp| void { wbu_driver_set_gear($(int gear)) } |]

wbu_driver_get_gear_number :: IO CInt 
wbu_driver_get_gear_number  =
   [C.exp| int { wbu_driver_get_gear_number() } |]

wbu_driver_get_control_mode :: IO WbuDriverControlMode 
wbu_driver_get_control_mode  =
   [C.exp| WbuDriverControlMode { wbu_driver_get_control_mode() } |]

wbu_driver_set_wiper_mode :: WbuDriverWiperMode -> IO () 
wbu_driver_set_wiper_mode mode =
   [C.exp| void { wbu_driver_set_wiper_mode($(WbuDriverWiperMode mode)) } |]

wbu_driver_get_wiper_mode :: IO WbuDriverWiperMode 
wbu_driver_get_wiper_mode  =
   [C.exp| WbuDriverWiperMode { wbu_driver_get_wiper_mode() } |]

