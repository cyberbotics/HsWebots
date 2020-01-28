
wb_keyboard_enable :: CInt -> IO () 
wb_keyboard_enable sampling_period =
   [C.exp| void { wb_keyboard_enable($(int sampling_period)) } |]

wb_keyboard_disable :: IO () 
wb_keyboard_disable  =
   [C.exp| void { wb_keyboard_disable() } |]

wb_keyboard_get_sampling_period :: IO CInt 
wb_keyboard_get_sampling_period  =
   [C.exp| int { wb_keyboard_get_sampling_period() } |]

wb_keyboard_get_key :: IO CInt 
wb_keyboard_get_key  =
   [C.exp| int { wb_keyboard_get_key() } |]
