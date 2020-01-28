
wb_distance_sensor_enable :: WbDeviceTag -> CInt -> IO () 
wb_distance_sensor_enable tag sampling_period =
   [C.exp| void { wb_distance_sensor_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_distance_sensor_disable :: WbDeviceTag -> IO () 
wb_distance_sensor_disable tag =
   [C.exp| void { wb_distance_sensor_disable($(WbDeviceTag tag)) } |]

wb_distance_sensor_get_sampling_period :: WbDeviceTag -> IO CInt 
wb_distance_sensor_get_sampling_period tag =
   [C.exp| int { wb_distance_sensor_get_sampling_period($(WbDeviceTag tag)) } |]

wb_distance_sensor_get_value :: WbDeviceTag -> IO CDouble 
wb_distance_sensor_get_value tag =
   [C.exp| double { wb_distance_sensor_get_value($(WbDeviceTag tag)) } |]

wb_distance_sensor_get_max_value :: WbDeviceTag -> IO CDouble 
wb_distance_sensor_get_max_value tag =
   [C.exp| double { wb_distance_sensor_get_max_value($(WbDeviceTag tag)) } |]

wb_distance_sensor_get_min_value :: WbDeviceTag -> IO CDouble 
wb_distance_sensor_get_min_value tag =
   [C.exp| double { wb_distance_sensor_get_min_value($(WbDeviceTag tag)) } |]

wb_distance_sensor_get_aperture :: WbDeviceTag -> IO CDouble 
wb_distance_sensor_get_aperture tag =
   [C.exp| double { wb_distance_sensor_get_aperture($(WbDeviceTag tag)) } |]

wb_distance_sensor_get_type :: WbDeviceTag -> IO WbDistanceSensorType 
wb_distance_sensor_get_type tag =
   [C.exp| WbDistanceSensorType { wb_distance_sensor_get_type($(WbDeviceTag tag)) } |]
