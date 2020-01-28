
wb_touch_sensor_enable :: WbDeviceTag -> CInt -> IO () 
wb_touch_sensor_enable tag sampling_period =
   [C.exp| void { wb_touch_sensor_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_touch_sensor_disable :: WbDeviceTag -> IO () 
wb_touch_sensor_disable tag =
   [C.exp| void { wb_touch_sensor_disable($(WbDeviceTag tag)) } |]

wb_touch_sensor_get_sampling_period :: WbDeviceTag -> IO CInt 
wb_touch_sensor_get_sampling_period tag =
   [C.exp| int { wb_touch_sensor_get_sampling_period($(WbDeviceTag tag)) } |]

wb_touch_sensor_get_value :: WbDeviceTag -> IO CDouble 
wb_touch_sensor_get_value tag =
   [C.exp| double { wb_touch_sensor_get_value($(WbDeviceTag tag)) } |]

wb_touch_sensor_get_values :: WbDeviceTag -> IO Ptr CDouble 
wb_touch_sensor_get_values tag =
   [C.exp| const double* { wb_touch_sensor_get_values($(WbDeviceTag tag)) } |]

wb_touch_sensor_get_type :: WbDeviceTag -> IO WbTouchSensorType 
wb_touch_sensor_get_type tag =
   [C.exp| WbTouchSensorType { wb_touch_sensor_get_type($(WbDeviceTag tag)) } |]
