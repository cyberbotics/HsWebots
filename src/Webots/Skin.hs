{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.Skin where
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
C.include "<webots/skin.h>"

wb_skin_set_bone_orientation :: WbDeviceTag -> CInt -> Ptr CDouble -> CBool -> IO () 
wb_skin_set_bone_orientation tag index orientation absolute =
   [C.exp| void { wb_skin_set_bone_orientation($(WbDeviceTag tag), $(int index), $(const double* orientation), $(bool absolute)) } |]

wb_skin_set_bone_position :: WbDeviceTag -> CInt -> Ptr CDouble -> CBool -> IO () 
wb_skin_set_bone_position tag index position absolute =
   [C.exp| void { wb_skin_set_bone_position($(WbDeviceTag tag), $(int index), $(const double* position), $(bool absolute)) } |]

wb_skin_get_bone_count :: WbDeviceTag -> IO CInt 
wb_skin_get_bone_count tag =
   [C.exp| int { wb_skin_get_bone_count($(WbDeviceTag tag)) } |]

wb_skin_get_bone_name :: WbDeviceTag -> CInt -> IO String 
wb_skin_get_bone_name tag index =
   peekCString =<< [C.exp| const char* { wb_skin_get_bone_name($(WbDeviceTag tag), $(int index)) } |]

wb_skin_get_bone_orientation :: WbDeviceTag -> CInt -> CBool -> IO (Ptr CDouble)
wb_skin_get_bone_orientation tag index absolute =
   [C.exp| const double* { wb_skin_get_bone_orientation($(WbDeviceTag tag), $(int index), $(bool absolute)) } |]

wb_skin_get_bone_position :: WbDeviceTag -> CInt -> CBool -> IO (Ptr CDouble)
wb_skin_get_bone_position tag index absolute =
   [C.exp| const double* { wb_skin_get_bone_position($(WbDeviceTag tag), $(int index), $(bool absolute)) } |]
