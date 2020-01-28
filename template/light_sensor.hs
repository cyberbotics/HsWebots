
wb_light_sensor_enable :: WbDeviceTag -> CInt -> IO () 
wb_light_sensor_enable tag sampling_period =
   [C.exp| void { wb_light_sensor_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_light_sensor_disable :: WbDeviceTag -> IO () 
wb_light_sensor_disable tag =
   [C.exp| void { wb_light_sensor_disable($(WbDeviceTag tag)) } |]

wb_light_sensor_get_sampling_period :: WbDeviceTag -> IO CInt 
wb_light_sensor_get_sampling_period tag =
   [C.exp| int { wb_light_sensor_get_sampling_period($(WbDeviceTag tag)) } |]

wb_light_sensor_get_value :: WbDeviceTag -> IO CDouble 
wb_light_sensor_get_value tag =
   [C.exp| double { wb_light_sensor_get_value($(WbDeviceTag tag)) } |]
