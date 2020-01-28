{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.Device where

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
C.include "<webots/device.h>"

wb_device_get_name :: WbDeviceTag -> IO String 
wb_device_get_name dt =
   peekCString =<< [C.exp| const char*  { wb_device_get_name($(WbDeviceTag dt)) } |]

wb_device_get_model :: WbDeviceTag -> IO String 
wb_device_get_model dt =
   peekCString =<< [C.exp| const char*  { wb_device_get_model($(WbDeviceTag dt)) } |]

wb_device_get_node_type :: WbDeviceTag -> IO WbNodeType 
wb_device_get_node_type dt =
   [C.exp| WbNodeType { wb_device_get_node_type($(WbDeviceTag dt)) } |]

