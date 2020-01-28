
wb_pen_write :: WbDeviceTag -> CBool -> IO () 
wb_pen_write tag write =
   [C.exp| void { wb_pen_write($(WbDeviceTag tag), $(bool write)) } |]

wb_pen_set_ink_color :: WbDeviceTag -> CInt -> CDouble -> IO () 
wb_pen_set_ink_color tag color density =
   [C.exp| void { wb_pen_set_ink_color($(WbDeviceTag tag), $(int color), $(double density)) } |]
