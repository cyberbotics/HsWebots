{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Webots.Display where
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
C.include "<webots/display.h>"

wb_display_get_width :: WbDeviceTag -> IO CInt 
wb_display_get_width tag =
   [C.exp| int { wb_display_get_width($(WbDeviceTag tag)) } |]

wb_display_get_height :: WbDeviceTag -> IO CInt 
wb_display_get_height tag =
   [C.exp| int { wb_display_get_height($(WbDeviceTag tag)) } |]

wb_display_set_color :: WbDeviceTag -> CInt -> IO () 
wb_display_set_color tag color =
   [C.exp| void { wb_display_set_color($(WbDeviceTag tag), $(int color)) } |]

wb_display_set_alpha :: WbDeviceTag -> CDouble -> IO () 
wb_display_set_alpha tag alpha =
   [C.exp| void { wb_display_set_alpha($(WbDeviceTag tag), $(double alpha)) } |]

wb_display_set_opacity :: WbDeviceTag -> CDouble -> IO () 
wb_display_set_opacity tag opacity =
   [C.exp| void { wb_display_set_opacity($(WbDeviceTag tag), $(double opacity)) } |]

wb_display_set_font :: WbDeviceTag -> String -> CInt -> CBool -> IO () 
wb_display_set_font tag font size anti_aliasing =
   withCString font $ \font' -> [C.exp| void { wb_display_set_font($(WbDeviceTag tag), $(const char* font'), $(int size), $(bool anti_aliasing)) } |]

wb_display_attach_camera :: WbDeviceTag -> WbDeviceTag -> IO () 
wb_display_attach_camera tag camera_tag =
   [C.exp| void { wb_display_attach_camera($(WbDeviceTag tag), $(WbDeviceTag camera_tag)) } |]

wb_display_detach_camera :: WbDeviceTag -> IO () 
wb_display_detach_camera tag =
   [C.exp| void { wb_display_detach_camera($(WbDeviceTag tag)) } |]

wb_display_draw_pixel :: WbDeviceTag -> CInt -> CInt -> IO () 
wb_display_draw_pixel tag x y =
   [C.exp| void { wb_display_draw_pixel($(WbDeviceTag tag), $(int x), $(int y)) } |]

wb_display_draw_line :: WbDeviceTag -> CInt -> CInt -> CInt -> CInt -> IO () 
wb_display_draw_line tag x1 y1 x2 y2 =
   [C.exp| void { wb_display_draw_line($(WbDeviceTag tag), $(int x1), $(int y1), $(int x2), $(int y2)) } |]

wb_display_draw_rectangle :: WbDeviceTag -> CInt -> CInt -> CInt -> CInt -> IO () 
wb_display_draw_rectangle tag x y width height =
   [C.exp| void { wb_display_draw_rectangle($(WbDeviceTag tag), $(int x), $(int y), $(int width), $(int height)) } |]

wb_display_draw_oval :: WbDeviceTag -> CInt -> CInt -> CInt -> CInt -> IO () 
wb_display_draw_oval tag cx cy a b =
   [C.exp| void { wb_display_draw_oval($(WbDeviceTag tag), $(int cx), $(int cy), $(int a), $(int b)) } |]

wb_display_draw_polygon :: WbDeviceTag -> Ptr CInt -> Ptr CInt -> CInt -> IO () 
wb_display_draw_polygon tag x y size =
   [C.exp| void { wb_display_draw_polygon($(WbDeviceTag tag), $(int* x), $(int* y), $(int size)) } |]

wb_display_draw_text :: WbDeviceTag -> String -> CInt -> CInt -> IO () 
wb_display_draw_text tag text x y =
   withCString text $ \text' -> [C.exp| void { wb_display_draw_text($(WbDeviceTag tag), $(const char* text'), $(int x), $(int y)) } |]

wb_display_fill_rectangle :: WbDeviceTag -> CInt -> CInt -> CInt -> CInt -> IO () 
wb_display_fill_rectangle tag x y width height =
   [C.exp| void { wb_display_fill_rectangle($(WbDeviceTag tag), $(int x), $(int y), $(int width), $(int height)) } |]

wb_display_fill_oval :: WbDeviceTag -> CInt -> CInt -> CInt -> CInt -> IO () 
wb_display_fill_oval tag cx cy a b =
   [C.exp| void { wb_display_fill_oval($(WbDeviceTag tag), $(int cx), $(int cy), $(int a), $(int b)) } |]

wb_display_fill_polygon :: WbDeviceTag -> Ptr CInt -> Ptr CInt -> CInt -> IO () 
wb_display_fill_polygon tag x y size =
   [C.exp| void { wb_display_fill_polygon($(WbDeviceTag tag), $(int* x), $(int* y), $(int size)) } |]

wb_display_image_new :: WbDeviceTag -> CInt -> CInt -> Ptr () -> CInt -> IO WbImageRef 
wb_display_image_new tag width height dat format =
   [C.exp| WbImageRef { wb_display_image_new($(WbDeviceTag tag), $(int width), $(int height), $(void* dat), $(int format)) } |]

wb_display_image_copy :: WbDeviceTag -> CInt -> CInt -> CInt -> CInt -> IO WbImageRef 
wb_display_image_copy tag x y width height =
   [C.exp| WbImageRef { wb_display_image_copy($(WbDeviceTag tag), $(int x), $(int y), $(int width), $(int height)) } |]

wb_display_image_load :: WbDeviceTag -> String -> IO WbImageRef 
wb_display_image_load tag filename =
   withCString filename $ \filename' -> [C.exp| WbImageRef { wb_display_image_load($(WbDeviceTag tag), $(const char* filename')) } |]

wb_display_image_delete :: WbDeviceTag -> WbImageRef -> IO () 
wb_display_image_delete tag ir =
   [C.exp| void { wb_display_image_delete($(WbDeviceTag tag), $(WbImageRef ir)) } |]

wb_display_image_paste :: WbDeviceTag -> WbImageRef -> CInt -> CInt -> CBool -> IO () 
wb_display_image_paste tag ir x y blend =
   [C.exp| void { wb_display_image_paste($(WbDeviceTag tag), $(WbImageRef ir), $(int x), $(int y), $(bool blend)) } |]

wb_display_image_save :: WbDeviceTag -> WbImageRef -> String -> IO () 
wb_display_image_save tag ir filename =
   withCString filename $ \filename' -> [C.exp| void { wb_display_image_save($(WbDeviceTag tag), $(WbImageRef ir), $(const char* filename')) } |]
