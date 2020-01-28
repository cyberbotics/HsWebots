
wb_microphone_enable :: WbDeviceTag -> CInt -> IO () 
wb_microphone_enable tag sampling_period =
   [C.exp| void { wb_microphone_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_microphone_disable :: WbDeviceTag -> IO () 
wb_microphone_disable tag =
   [C.exp| void { wb_microphone_disable($(WbDeviceTag tag)) } |]

wb_microphone_get_sampling_period :: WbDeviceTag -> IO CInt 
wb_microphone_get_sampling_period tag =
   [C.exp| int { wb_microphone_get_sampling_period($(WbDeviceTag tag)) } |]
