
wb_camera_enable :: WbDeviceTag -> CInt -> IO () 
wb_camera_enable tag sampling_period =
   [C.exp| void { wb_camera_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_camera_disable :: WbDeviceTag -> IO () 
wb_camera_disable tag =
   [C.exp| void { wb_camera_disable($(WbDeviceTag tag)) } |]

wb_camera_get_sampling_period :: WbDeviceTag -> IO CInt 
wb_camera_get_sampling_period tag =
   [C.exp| int { wb_camera_get_sampling_period($(WbDeviceTag tag)) } |]
