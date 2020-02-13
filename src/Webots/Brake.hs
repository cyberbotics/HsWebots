{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.Brake where
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
C.include "<webots/brake.h>"

wb_brake_set_damping_constant :: WbDeviceTag -> CDouble -> IO () 
wb_brake_set_damping_constant tag damping_constant =
   [C.exp| void { wb_brake_set_damping_constant($(WbDeviceTag tag), $(double damping_constant)) } |]

wb_brake_get_type :: WbDeviceTag -> IO WbJointType 
wb_brake_get_type tag =
   [C.exp| WbJointType { wb_brake_get_type($(WbDeviceTag tag)) } |]

wb_brake_get_motor :: WbDeviceTag -> IO WbDeviceTag 
wb_brake_get_motor tag =
   [C.exp| WbDeviceTag { wb_brake_get_motor($(WbDeviceTag tag)) } |]

wb_brake_get_position_sensor :: WbDeviceTag -> IO WbDeviceTag 
wb_brake_get_position_sensor tag =
   [C.exp| WbDeviceTag { wb_brake_get_position_sensor($(WbDeviceTag tag)) } |]
