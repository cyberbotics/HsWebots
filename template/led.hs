
wb_led_set :: WbDeviceTag -> CInt -> IO () 
wb_led_set tag value =
   [C.exp| void { wb_led_set($(WbDeviceTag tag), $(int value)) } |]

wb_led_get :: WbDeviceTag -> IO CInt 
wb_led_get tag =
   [C.exp| int { wb_led_get($(WbDeviceTag tag)) } |]
