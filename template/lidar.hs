
wb_lidar_enable :: WbDeviceTag -> CInt -> IO () 
wb_lidar_enable tag sampling_period =
   [C.exp| void { wb_lidar_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_lidar_enable_point_cloud :: WbDeviceTag -> IO () 
wb_lidar_enable_point_cloud tag =
   [C.exp| void { wb_lidar_enable_point_cloud($(WbDeviceTag tag)) } |]

wb_lidar_disable :: WbDeviceTag -> IO () 
wb_lidar_disable tag =
   [C.exp| void { wb_lidar_disable($(WbDeviceTag tag)) } |]

wb_lidar_disable_point_cloud :: WbDeviceTag -> IO () 
wb_lidar_disable_point_cloud tag =
   [C.exp| void { wb_lidar_disable_point_cloud($(WbDeviceTag tag)) } |]

wb_lidar_get_sampling_period :: WbDeviceTag -> IO CInt 
wb_lidar_get_sampling_period tag =
   [C.exp| int { wb_lidar_get_sampling_period($(WbDeviceTag tag)) } |]

wb_lidar_is_point_cloud_enabled :: WbDeviceTag -> IO CBool 
wb_lidar_is_point_cloud_enabled tag =
   [C.exp| bool { wb_lidar_is_point_cloud_enabled($(WbDeviceTag tag)) } |]
