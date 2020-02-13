{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.DifferentialWheels where
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
C.include "<webots/differential_wheels.h>"

wb_differential_wheels_set_speed :: CDouble -> CDouble -> IO () 
wb_differential_wheels_set_speed left right =
   [C.exp| void { wb_differential_wheels_set_speed($(double left), $(double right)) } |]

wb_differential_wheels_get_left_speed :: IO CDouble 
wb_differential_wheels_get_left_speed  =
   [C.exp| double { wb_differential_wheels_get_left_speed() } |]

wb_differential_wheels_get_right_speed :: IO CDouble 
wb_differential_wheels_get_right_speed  =
   [C.exp| double { wb_differential_wheels_get_right_speed() } |]

wb_differential_wheels_get_max_speed :: IO CDouble 
wb_differential_wheels_get_max_speed  =
   [C.exp| double { wb_differential_wheels_get_max_speed() } |]

wb_differential_wheels_get_speed_unit :: IO CDouble 
wb_differential_wheels_get_speed_unit  =
   [C.exp| double { wb_differential_wheels_get_speed_unit() } |]

wb_differential_wheels_enable_encoders :: CInt -> IO () 
wb_differential_wheels_enable_encoders sampling_period =
   [C.exp| void { wb_differential_wheels_enable_encoders($(int sampling_period)) } |]

wb_differential_wheels_disable_encoders :: IO () 
wb_differential_wheels_disable_encoders  =
   [C.exp| void { wb_differential_wheels_disable_encoders() } |]

wb_differential_wheels_get_encoders_sampling_period :: IO CInt 
wb_differential_wheels_get_encoders_sampling_period  =
   [C.exp| int { wb_differential_wheels_get_encoders_sampling_period() } |]

wb_differential_wheels_get_left_encoder :: IO CDouble 
wb_differential_wheels_get_left_encoder  =
   [C.exp| double { wb_differential_wheels_get_left_encoder() } |]

wb_differential_wheels_get_right_encoder :: IO CDouble 
wb_differential_wheels_get_right_encoder  =
   [C.exp| double { wb_differential_wheels_get_right_encoder() } |]

wb_differential_wheels_set_encoders :: CDouble -> CDouble -> IO () 
wb_differential_wheels_set_encoders left right =
   [C.exp| void { wb_differential_wheels_set_encoders($(double left), $(double right)) } |]
