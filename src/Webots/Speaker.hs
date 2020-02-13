{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.Speaker where
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
C.include "<webots/speaker.h>"

wb_speaker_play_sound :: WbDeviceTag -> WbDeviceTag -> String -> CDouble -> CDouble -> CDouble -> CBool -> IO () 
wb_speaker_play_sound left right sound volume pitch balance loop =
   withCString sound $ \sound' -> [C.exp| void { wb_speaker_play_sound($(WbDeviceTag left), $(WbDeviceTag right), $(const char* sound'), $(double volume), $(double pitch), $(double balance), $(bool loop)) } |]

wb_speaker_stop :: WbDeviceTag -> String -> IO () 
wb_speaker_stop tag sound =
   withCString sound $ \sound' -> [C.exp| void { wb_speaker_stop($(WbDeviceTag tag), $(const char* sound')) } |]

wb_speaker_is_sound_playing :: WbDeviceTag -> String -> IO CBool 
wb_speaker_is_sound_playing tag sound =
   withCString sound $ \sound' -> [C.exp| bool { wb_speaker_is_sound_playing($(WbDeviceTag tag), $(const char* sound')) } |]

wb_speaker_set_engine :: WbDeviceTag -> String -> IO CBool 
wb_speaker_set_engine tag engine =
   withCString engine $ \engine' -> [C.exp| bool { wb_speaker_set_engine($(WbDeviceTag tag), $(const char* engine')) } |]

wb_speaker_set_language :: WbDeviceTag -> String -> IO CBool 
wb_speaker_set_language tag language =
   withCString language $ \language' -> [C.exp| bool { wb_speaker_set_language($(WbDeviceTag tag), $(const char* language')) } |]

wb_speaker_get_engine :: WbDeviceTag -> IO String 
wb_speaker_get_engine tag =
   peekCString =<< [C.exp| const char* { wb_speaker_get_engine($(WbDeviceTag tag)) } |]

wb_speaker_get_language :: WbDeviceTag -> IO String 
wb_speaker_get_language tag =
   peekCString =<< [C.exp| const char* { wb_speaker_get_language($(WbDeviceTag tag)) } |]

wb_speaker_speak :: WbDeviceTag -> String -> CDouble -> IO () 
wb_speaker_speak tag text volume =
   withCString text $ \text' -> [C.exp| void { wb_speaker_speak($(WbDeviceTag tag), $(const char* text'), $(double volume)) } |]

wb_speaker_is_speaking :: WbDeviceTag -> IO CBool 
wb_speaker_is_speaking tag =
   [C.exp| bool { wb_speaker_is_speaking($(WbDeviceTag tag)) } |]
