
wb_radar_enable :: WbDeviceTag -> CInt -> IO () 
wb_radar_enable tag sampling_period =
   [C.exp| void { wb_radar_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_radar_disable :: WbDeviceTag -> IO () 
wb_radar_disable tag =
   [C.exp| void { wb_radar_disable($(WbDeviceTag tag)) } |]

wb_radar_get_sampling_period :: WbDeviceTag -> IO CInt 
wb_radar_get_sampling_period tag =
   [C.exp| int { wb_radar_get_sampling_period($(WbDeviceTag tag)) } |]

wb_radar_get_number_of_targets :: WbDeviceTag -> IO CInt 
wb_radar_get_number_of_targets tag =
   [C.exp| int { wb_radar_get_number_of_targets($(WbDeviceTag tag)) } |]
