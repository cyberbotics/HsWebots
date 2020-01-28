
wb_position_sensor_enable :: WbDeviceTag -> CInt -> IO () 
wb_position_sensor_enable tag sampling_period =
   [C.exp| void { wb_position_sensor_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_position_sensor_disable :: WbDeviceTag -> IO () 
wb_position_sensor_disable tag =
   [C.exp| void { wb_position_sensor_disable($(WbDeviceTag tag)) } |]

wb_position_sensor_get_sampling_period :: WbDeviceTag -> IO CInt 
wb_position_sensor_get_sampling_period tag =
   [C.exp| int { wb_position_sensor_get_sampling_period($(WbDeviceTag tag)) } |]

wb_position_sensor_get_value :: WbDeviceTag -> IO CDouble 
wb_position_sensor_get_value tag =
   [C.exp| double { wb_position_sensor_get_value($(WbDeviceTag tag)) } |]

wb_position_sensor_get_type :: WbDeviceTag -> IO WbJointType 
wb_position_sensor_get_type tag =
   [C.exp| WbJointType { wb_position_sensor_get_type($(WbDeviceTag tag)) } |]

wb_position_sensor_get_motor :: WbDeviceTag -> IO WbDeviceTag 
wb_position_sensor_get_motor tag =
   [C.exp| WbDeviceTag { wb_position_sensor_get_motor($(WbDeviceTag tag)) } |]

wb_position_sensor_get_brake :: WbDeviceTag -> IO WbDeviceTag 
wb_position_sensor_get_brake tag =
   [C.exp| WbDeviceTag { wb_position_sensor_get_brake($(WbDeviceTag tag)) } |]
