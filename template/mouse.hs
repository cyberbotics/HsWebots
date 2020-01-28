
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
wb_mouse_get_state  =
   [C.exp| WbMouseState { wb_mouse_get_state() } |]
