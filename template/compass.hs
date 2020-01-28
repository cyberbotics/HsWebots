
wb_compass_enable :: WbDeviceTag -> CInt -> IO () 
wb_compass_enable tag sampling_period =
   [C.exp| void { wb_compass_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_compass_disable :: WbDeviceTag -> IO () 
wb_compass_disable tag =
   [C.exp| void { wb_compass_disable($(WbDeviceTag tag)) } |]

wb_compass_get_sampling_period :: WbDeviceTag -> IO CInt 
wb_compass_get_sampling_period tag =
   [C.exp| int { wb_compass_get_sampling_period($(WbDeviceTag tag)) } |]

wb_compass_get_values :: WbDeviceTag -> IO Ptr CDouble 
wb_compass_get_values tag =
   [C.exp| const double* { wb_compass_get_values($(WbDeviceTag tag)) } |]
