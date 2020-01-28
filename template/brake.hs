
wb_brake_set_damping_constant :: WbDeviceTag -> CDouble -> IO () 
wb_brake_set_damping_constant tag damping_constant =
   [C.exp| void { wb_brake_set_damping_constant($(WbDeviceTag tag), $(double damping_constant)) } |]

wb_brake_get_type :: WbDeviceTag -> IO WbJointType 
wb_brake_get_type tag =
   [C.exp| WbJointType { wb_brake_get_type($(WbDeviceTag tag)) } |]

wb_brake_get_motor :: WbDeviceTag -> IO WbDeviceTag 
wb_brake_get_motor tag =
   [C.exp| WbDeviceTag { wb_brake_get_motor($(WbDeviceTag tag)) } |]

wb_brake_get_position_sensor :: WbDeviceTag -> IO WbDeviceTag 
wb_brake_get_position_sensor tag =
   [C.exp| WbDeviceTag { wb_brake_get_position_sensor($(WbDeviceTag tag)) } |]
