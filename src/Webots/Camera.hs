{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.Camera where

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

wb_camera_get_image :: WbDeviceTag -> IO (I.Image I.PixelRGBA8)
wb_camera_get_image tag = do
  let channel = 4
      zero = I.PixelRGBA8 0 0 0 0
  width <- fromIntegral <$> wb_camera_get_width tag
  height <- fromIntegral <$> wb_camera_get_height tag
  ptr1 <- [C.exp| const char* { wb_camera_get_image($(WbDeviceTag tag)) } |]
  let img@(I.Image w h vec) = I.generateImage (\_ _ -> zero) width height
  let (fptr,len) = V.unsafeToForeignPtr0 vec
      whc = width * height * channel
  if (len /= whc) then
    throwIO $ userError  $ "vector's length(" ++ show len ++ ") is not the same as image' one."
  else do
    F.withForeignPtr fptr $ \ptr2 -> do
      BSI.memcpy (F.castPtr ptr2) (F.castPtr ptr1) len
--      return $ I.pixelMap bgr2rgb img
      return $ img
  where
    bgr2rgb (I.PixelRGBA8 b g r a) = I.PixelRGBA8 r g b a
  
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

wb_camera_recognition_get_objects :: WbDeviceTag -> IO [WbCameraRecognitionObject]
wb_camera_recognition_get_objects tag = do
   num <- wb_camera_recognition_get_number_of_objects tag
   ptr <- [C.exp| const WbCameraRecognitionObject* { wb_camera_recognition_get_objects($(WbDeviceTag tag)) } |]
   forM [0..(num-1)] $ \i -> do
     obj_id <- [C.exp| int { $(WbCameraRecognitionObject* ptr)[$(int i)].id } |]
     obj_position <- (,,)
       <$> [C.exp| double { $(WbCameraRecognitionObject* ptr)[$(int i)].position[0] } |]
       <*> [C.exp| double { $(WbCameraRecognitionObject* ptr)[$(int i)].position[1] } |]
       <*> [C.exp| double { $(WbCameraRecognitionObject* ptr)[$(int i)].position[2] } |]
     obj_orientation <- (,,,)
       <$> [C.exp| double { $(WbCameraRecognitionObject* ptr)[$(int i)].orientation[0] } |]
       <*> [C.exp| double { $(WbCameraRecognitionObject* ptr)[$(int i)].orientation[1] } |]
       <*> [C.exp| double { $(WbCameraRecognitionObject* ptr)[$(int i)].orientation[2] } |]
       <*> [C.exp| double { $(WbCameraRecognitionObject* ptr)[$(int i)].orientation[3] } |]
     obj_size <- (,)
       <$> [C.exp| double { $(WbCameraRecognitionObject* ptr)[$(int i)].size[0] } |]
       <*> [C.exp| double { $(WbCameraRecognitionObject* ptr)[$(int i)].size[1] } |]
     obj_position_on_image <- (,)
       <$> [C.exp| int { $(WbCameraRecognitionObject* ptr)[$(int i)].position_on_image[0] } |]
       <*> [C.exp| int { $(WbCameraRecognitionObject* ptr)[$(int i)].position_on_image[1] } |]
     obj_size_on_image <- (,)
       <$> [C.exp| int { $(WbCameraRecognitionObject* ptr)[$(int i)].size_on_image[0] } |]
       <*> [C.exp| int { $(WbCameraRecognitionObject* ptr)[$(int i)].size_on_image[1] } |]
     obj_number_of_colors  <- [C.exp| int { $(WbCameraRecognitionObject* ptr)[$(int i)].number_of_colors } |]
     obj_colors <- forM [0..(obj_number_of_colors-1)] $ \j ->
       [C.exp| double { $(WbCameraRecognitionObject* ptr)[$(int i)].colors[$(int j)] } |]
     obj_model <- peekCString =<< [C.exp| const char* { $(WbCameraRecognitionObject* ptr)[$(int i)].model } |]
     return $ WbCameraRecognitionObject{..}
   

