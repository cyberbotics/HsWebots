
wb_device_get_name :: WbDeviceTag -> IO String 
wb_device_get_name dt =
   [C.exp| const char* { wb_device_get_name($(WbDeviceTag dt)) } |]

wb_device_get_model :: WbDeviceTag -> IO String 
wb_device_get_model dt =
   [C.exp| const char* { wb_device_get_model($(WbDeviceTag dt)) } |]

wb_device_get_node_type :: WbDeviceTag -> IO WbNodeType 
wb_device_get_node_type dt =
   [C.exp| WbNodeType { wb_device_get_node_type($(WbDeviceTag dt)) } |]

wb_device_get_type :: WbDeviceTag -> IO WbNodeType 
wb_device_get_type dt =
   [C.exp| WbNodeType { wb_device_get_type($(WbDeviceTag dt)) } |]
