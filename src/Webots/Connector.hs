{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.Connector where
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
C.include "<webots/connector.h>"

wb_connector_enable_presence :: WbDeviceTag -> CInt -> IO () 
wb_connector_enable_presence tag sampling_period =
   [C.exp| void { wb_connector_enable_presence($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_connector_disable_presence :: WbDeviceTag -> IO () 
wb_connector_disable_presence tag =
   [C.exp| void { wb_connector_disable_presence($(WbDeviceTag tag)) } |]

wb_connector_get_presence_sampling_period :: WbDeviceTag -> IO CInt 
wb_connector_get_presence_sampling_period tag =
   [C.exp| int { wb_connector_get_presence_sampling_period($(WbDeviceTag tag)) } |]

wb_connector_get_presence :: WbDeviceTag -> IO CInt 
wb_connector_get_presence tag =
   [C.exp| int { wb_connector_get_presence($(WbDeviceTag tag)) } |]

wb_connector_lock :: WbDeviceTag -> IO () 
wb_connector_lock tag =
   [C.exp| void { wb_connector_lock($(WbDeviceTag tag)) } |]

wb_connector_unlock :: WbDeviceTag -> IO () 
wb_connector_unlock tag =
   [C.exp| void { wb_connector_unlock($(WbDeviceTag tag)) } |]
