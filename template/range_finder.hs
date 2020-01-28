
wb_range_finder_enable :: WbDeviceTag -> CInt -> IO () 
wb_range_finder_enable tag sampling_period =
   [C.exp| void { wb_range_finder_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_range_finder_disable :: WbDeviceTag -> IO () 
wb_range_finder_disable tag =
   [C.exp| void { wb_range_finder_disable($(WbDeviceTag tag)) } |]

wb_range_finder_get_sampling_period :: WbDeviceTag -> IO CInt 
wb_range_finder_get_sampling_period tag =
   [C.exp| int { wb_range_finder_get_sampling_period($(WbDeviceTag tag)) } |]
