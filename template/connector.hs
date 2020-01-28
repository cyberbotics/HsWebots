
wb_connector_enable_presence :: WbDeviceTag -> CInt -> IO () 
wb_connector_enable_presence tag sampling_period =
   [C.exp| void { wb_connector_enable_presence($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_connector_disable_presence :: WbDeviceTag -> IO () 
wb_connector_disable_presence tag =
   [C.exp| void { wb_connector_disable_presence($(WbDeviceTag tag)) } |]

wb_connector_get_presence_sampling_period :: WbDeviceTag -> IO CInt 
wb_connector_get_presence_sampling_period tag =
   [C.exp| int { wb_connector_get_presence_sampling_period($(WbDeviceTag tag)) } |]

wb_connector_get_presence :: WbDeviceTag -> IO CInt 
wb_connector_get_presence tag =
   [C.exp| int { wb_connector_get_presence($(WbDeviceTag tag)) } |]

wb_connector_lock :: WbDeviceTag -> IO () 
wb_connector_lock tag =
   [C.exp| void { wb_connector_lock($(WbDeviceTag tag)) } |]

wb_connector_unlock :: WbDeviceTag -> IO () 
wb_connector_unlock tag =
   [C.exp| void { wb_connector_unlock($(WbDeviceTag tag)) } |]
