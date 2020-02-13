{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.Lidar where
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
C.include "<webots/lidar.h>"

wb_lidar_enable :: WbDeviceTag -> CInt -> IO () 
wb_lidar_enable tag sampling_period =
   [C.exp| void { wb_lidar_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_lidar_enable_point_cloud :: WbDeviceTag -> IO () 
wb_lidar_enable_point_cloud tag =
   [C.exp| void { wb_lidar_enable_point_cloud($(WbDeviceTag tag)) } |]

wb_lidar_disable :: WbDeviceTag -> IO () 
wb_lidar_disable tag =
   [C.exp| void { wb_lidar_disable($(WbDeviceTag tag)) } |]

wb_lidar_disable_point_cloud :: WbDeviceTag -> IO () 
wb_lidar_disable_point_cloud tag =
   [C.exp| void { wb_lidar_disable_point_cloud($(WbDeviceTag tag)) } |]

wb_lidar_get_sampling_period :: WbDeviceTag -> IO CInt 
wb_lidar_get_sampling_period tag =
   [C.exp| int { wb_lidar_get_sampling_period($(WbDeviceTag tag)) } |]

wb_lidar_is_point_cloud_enabled :: WbDeviceTag -> IO CBool 
wb_lidar_is_point_cloud_enabled tag =
   [C.exp| bool { wb_lidar_is_point_cloud_enabled($(WbDeviceTag tag)) } |]
