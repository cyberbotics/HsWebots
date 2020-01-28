
wb_gps_enable :: WbDeviceTag -> CInt -> IO () 
wb_gps_enable tag sampling_period =
   [C.exp| void { wb_gps_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_gps_disable :: WbDeviceTag -> IO () 
wb_gps_disable tag =
   [C.exp| void { wb_gps_disable($(WbDeviceTag tag)) } |]

wb_gps_get_sampling_period :: WbDeviceTag -> IO CInt 
wb_gps_get_sampling_period tag =
   [C.exp| int { wb_gps_get_sampling_period($(WbDeviceTag tag)) } |]

wb_gps_get_speed :: WbDeviceTag -> IO Ptr CDouble 
wb_gps_get_speed tag =
   [C.exp| const double* { wb_gps_get_speed($(WbDeviceTag tag)) } |]

wb_gps_get_values :: WbDeviceTag -> IO Ptr CDouble 
wb_gps_get_values tag =
   [C.exp| const double* { wb_gps_get_values($(WbDeviceTag tag)) } |]

wb_gps_convert_to_degrees_minutes_seconds :: CDouble -> IO String 
wb_gps_convert_to_degrees_minutes_seconds decimal_degrees =
   [C.exp| const char* { wb_gps_convert_to_degrees_minutes_seconds($(double decimal_degrees)) } |]

wb_gps_get_coordinate_system :: WbDeviceTag -> IO WbGpsCoordinateSystem 
wb_gps_get_coordinate_system tag =
   [C.exp| WbGpsCoordinateSystem { wb_gps_get_coordinate_system($(WbDeviceTag tag)) } |]
