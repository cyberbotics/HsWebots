
wb_accelerometer_enable :: WbDeviceTag -> CInt -> IO () 
wb_accelerometer_enable tag sampling_period =
   [C.exp| void { wb_accelerometer_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_accelerometer_disable :: WbDeviceTag -> IO () 
wb_accelerometer_disable tag =
   [C.exp| void { wb_accelerometer_disable($(WbDeviceTag tag)) } |]

wb_accelerometer_get_sampling_period :: WbDeviceTag -> IO CInt 
wb_accelerometer_get_sampling_period tag =
   [C.exp| int { wb_accelerometer_get_sampling_period($(WbDeviceTag tag)) } |]

wb_accelerometer_get_values :: WbDeviceTag -> IO Ptr CDouble 
wb_accelerometer_get_values tag =
   [C.exp| const double* { wb_accelerometer_get_values($(WbDeviceTag tag)) } |]
