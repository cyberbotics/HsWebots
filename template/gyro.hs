
wb_gyro_enable :: WbDeviceTag -> CInt -> IO () 
wb_gyro_enable tag sampling_period =
   [C.exp| void { wb_gyro_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_gyro_disable :: WbDeviceTag -> IO () 
wb_gyro_disable tag =
   [C.exp| void { wb_gyro_disable($(WbDeviceTag tag)) } |]

wb_gyro_get_sampling_period :: WbDeviceTag -> IO CInt 
wb_gyro_get_sampling_period tag =
   [C.exp| int { wb_gyro_get_sampling_period($(WbDeviceTag tag)) } |]

wb_gyro_get_values :: WbDeviceTag -> IO Ptr CDouble 
wb_gyro_get_values tag =
   [C.exp| const double* { wb_gyro_get_values($(WbDeviceTag tag)) } |]
