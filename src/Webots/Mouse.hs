{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.Mouse where
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
C.include "<webots/mouse.h>"

wb_mouse_enable :: CInt -> IO () 
wb_mouse_enable sampling_period =
   [C.exp| void { wb_mouse_enable($(int sampling_period)) } |]

wb_mouse_disable :: IO () 
wb_mouse_disable  =
   [C.exp| void { wb_mouse_disable() } |]

wb_mouse_get_sampling_period :: IO CInt 
wb_mouse_get_sampling_period  =
   [C.exp| int { wb_mouse_get_sampling_period() } |]

wb_mouse_enable_3d_position :: IO () 
wb_mouse_enable_3d_position  =
   [C.exp| void { wb_mouse_enable_3d_position() } |]

wb_mouse_disable_3d_position :: IO () 
wb_mouse_disable_3d_position  =
   [C.exp| void { wb_mouse_disable_3d_position() } |]

wb_mouse_is_3d_position_enabled :: IO CBool 
wb_mouse_is_3d_position_enabled  =
   [C.exp| bool { wb_mouse_is_3d_position_enabled() } |]

wb_mouse_get_state :: IO WbMouseState 
wb_mouse_get_state  = do
  mouse_left <- [C.exp| bool { wb_mouse_get_state().left } |]
  mouse_middle <- [C.exp| bool { wb_mouse_get_state().middle } |]
  mouse_right <- [C.exp| bool { wb_mouse_get_state().right } |]
  mouse_u <- [C.exp| double { wb_mouse_get_state().u } |]
  mouse_v <- [C.exp| double { wb_mouse_get_state().v } |]
  mouse_x <- [C.exp| double { wb_mouse_get_state().x } |]
  mouse_y <- [C.exp| double { wb_mouse_get_state().y } |]
  mouse_z <- [C.exp| double { wb_mouse_get_state().z } |]
  return $ WbMouseState {..}
