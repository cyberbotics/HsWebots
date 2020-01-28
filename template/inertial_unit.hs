
wb_inertial_unit_enable :: WbDeviceTag -> CInt -> IO () 
wb_inertial_unit_enable tag sampling_period =
   [C.exp| void { wb_inertial_unit_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_inertial_unit_disable :: WbDeviceTag -> IO () 
wb_inertial_unit_disable tag =
   [C.exp| void { wb_inertial_unit_disable($(WbDeviceTag tag)) } |]

wb_inertial_unit_get_sampling_period :: WbDeviceTag -> IO CInt 
wb_inertial_unit_get_sampling_period tag =
   [C.exp| int { wb_inertial_unit_get_sampling_period($(WbDeviceTag tag)) } |]

wb_inertial_unit_get_roll_pitch_yaw :: WbDeviceTag -> IO Ptr CDouble 
wb_inertial_unit_get_roll_pitch_yaw tag =
   [C.exp| const double* { wb_inertial_unit_get_roll_pitch_yaw($(WbDeviceTag tag)) } |]
