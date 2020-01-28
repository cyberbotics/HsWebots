
wb_emitter_send :: WbDeviceTag -> const,void -> CInt -> IO CInt 
wb_emitter_send tag data size =
   [C.exp| int { wb_emitter_send($(WbDeviceTag tag), $(const,void data), $(int size)) } |]

wb_emitter_get_buffer_size :: WbDeviceTag -> IO CInt 
wb_emitter_get_buffer_size tag =
   [C.exp| int { wb_emitter_get_buffer_size($(WbDeviceTag tag)) } |]

wb_emitter_set_channel :: WbDeviceTag -> CInt -> IO () 
wb_emitter_set_channel tag channel =
   [C.exp| void { wb_emitter_set_channel($(WbDeviceTag tag), $(int channel)) } |]

wb_emitter_get_channel :: WbDeviceTag -> IO CInt 
wb_emitter_get_channel tag =
   [C.exp| int { wb_emitter_get_channel($(WbDeviceTag tag)) } |]

wb_emitter_get_range :: WbDeviceTag -> IO CDouble 
wb_emitter_get_range tag =
   [C.exp| double { wb_emitter_get_range($(WbDeviceTag tag)) } |]

wb_emitter_set_range :: WbDeviceTag -> CDouble -> IO () 
wb_emitter_set_range tag range =
   [C.exp| void { wb_emitter_set_range($(WbDeviceTag tag), $(double range)) } |]
