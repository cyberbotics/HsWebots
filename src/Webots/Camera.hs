{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.Camera where

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
C.include "<webots/camera.h>"

wb_camera_enable :: WbDeviceTag -> CInt -> IO () 
wb_camera_enable tag sampling_period =
   [C.exp| void { wb_camera_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_camera_disable :: WbDeviceTag -> IO () 
wb_camera_disable tag =
   [C.exp| void { wb_camera_disable($(WbDeviceTag tag)) } |]

wb_camera_get_sampling_period :: WbDeviceTag -> IO CInt 
wb_camera_get_sampling_period tag =
   [C.exp| int { wb_camera_get_sampling_period($(WbDeviceTag tag)) } |]


-- |
-- ToDo: This function should return bytestring
wb_camera_get_image :: WbDeviceTag -> IO String 
wb_camera_get_image tag =
   peekCString =<< [C.exp| const char* { wb_camera_get_image($(WbDeviceTag tag)) } |]

wb_camera_get_width :: WbDeviceTag -> IO CInt 
wb_camera_get_width tag =
   [C.exp| int { wb_camera_get_width($(WbDeviceTag tag)) } |]

wb_camera_get_height :: WbDeviceTag -> IO CInt 
wb_camera_get_height tag =
   [C.exp| int { wb_camera_get_height($(WbDeviceTag tag)) } |]

wb_camera_get_fov :: WbDeviceTag -> IO CDouble 
wb_camera_get_fov tag =
   [C.exp| double { wb_camera_get_fov($(WbDeviceTag tag)) } |]

wb_camera_get_max_fov :: WbDeviceTag -> IO CDouble 
wb_camera_get_max_fov tag =
   [C.exp| double { wb_camera_get_max_fov($(WbDeviceTag tag)) } |]

wb_camera_get_min_fov :: WbDeviceTag -> IO CDouble 
wb_camera_get_min_fov tag =
   [C.exp| double { wb_camera_get_min_fov($(WbDeviceTag tag)) } |]

wb_camera_set_fov :: WbDeviceTag -> CDouble -> IO () 
wb_camera_set_fov tag fov =
   [C.exp| void { wb_camera_set_fov($(WbDeviceTag tag), $(double fov)) } |]

wb_camera_get_focal_length :: WbDeviceTag -> IO CDouble 
wb_camera_get_focal_length tag =
   [C.exp| double { wb_camera_get_focal_length($(WbDeviceTag tag)) } |]

wb_camera_get_focal_distance :: WbDeviceTag -> IO CDouble 
wb_camera_get_focal_distance tag =
   [C.exp| double { wb_camera_get_focal_distance($(WbDeviceTag tag)) } |]

wb_camera_get_max_focal_distance :: WbDeviceTag -> IO CDouble 
wb_camera_get_max_focal_distance tag =
   [C.exp| double { wb_camera_get_max_focal_distance($(WbDeviceTag tag)) } |]

wb_camera_get_min_focal_distance :: WbDeviceTag -> IO CDouble 
wb_camera_get_min_focal_distance tag =
   [C.exp| double { wb_camera_get_min_focal_distance($(WbDeviceTag tag)) } |]

wb_camera_set_focal_distance :: WbDeviceTag -> CDouble -> IO () 
wb_camera_set_focal_distance tag focal_distance =
   [C.exp| void { wb_camera_set_focal_distance($(WbDeviceTag tag), $(double focal_distance)) } |]

wb_camera_get_near :: WbDeviceTag -> IO CDouble 
wb_camera_get_near tag =
   [C.exp| double { wb_camera_get_near($(WbDeviceTag tag)) } |]

wb_camera_save_image :: WbDeviceTag -> String -> CInt -> IO CInt 
wb_camera_save_image tag filename quality =
  withCString filename $ \filename' ->
   [C.exp| int { wb_camera_save_image($(WbDeviceTag tag), $(const char* filename'), $(int quality)) } |]

wb_camera_has_recognition :: WbDeviceTag -> IO CBool 
wb_camera_has_recognition tag =
   [C.exp| bool { wb_camera_has_recognition($(WbDeviceTag tag)) } |]

wb_camera_recognition_enable :: WbDeviceTag -> CInt -> IO () 
wb_camera_recognition_enable tag sampling_period =
   [C.exp| void { wb_camera_recognition_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_camera_recognition_disable :: WbDeviceTag -> IO () 
wb_camera_recognition_disable tag =
   [C.exp| void { wb_camera_recognition_disable($(WbDeviceTag tag)) } |]

wb_camera_recognition_get_sampling_period :: WbDeviceTag -> IO CInt 
wb_camera_recognition_get_sampling_period tag =
   [C.exp| int { wb_camera_recognition_get_sampling_period($(WbDeviceTag tag)) } |]

wb_camera_recognition_get_number_of_objects :: WbDeviceTag -> IO CInt 
wb_camera_recognition_get_number_of_objects tag =
   [C.exp| int { wb_camera_recognition_get_number_of_objects($(WbDeviceTag tag)) } |]
