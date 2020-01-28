
wb_receiver_enable :: WbDeviceTag -> CInt -> IO () 
wb_receiver_enable tag sampling_period =
   [C.exp| void { wb_receiver_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_receiver_disable :: WbDeviceTag -> IO () 
wb_receiver_disable tag =
   [C.exp| void { wb_receiver_disable($(WbDeviceTag tag)) } |]

wb_receiver_get_sampling_period :: WbDeviceTag -> IO CInt 
wb_receiver_get_sampling_period tag =
   [C.exp| int { wb_receiver_get_sampling_period($(WbDeviceTag tag)) } |]

wb_receiver_set_channel :: WbDeviceTag -> CInt -> IO () 
wb_receiver_set_channel tag channel =
   [C.exp| void { wb_receiver_set_channel($(WbDeviceTag tag), $(int channel)) } |]

wb_receiver_get_channel :: WbDeviceTag -> IO CInt 
wb_receiver_get_channel tag =
   [C.exp| int { wb_receiver_get_channel($(WbDeviceTag tag)) } |]

wb_receiver_get_queue_length :: WbDeviceTag -> IO CInt 
wb_receiver_get_queue_length tag =
   [C.exp| int { wb_receiver_get_queue_length($(WbDeviceTag tag)) } |]

wb_receiver_next_packet :: WbDeviceTag -> IO () 
wb_receiver_next_packet tag =
   [C.exp| void { wb_receiver_next_packet($(WbDeviceTag tag)) } |]

wb_receiver_get_data_size :: WbDeviceTag -> IO CInt 
wb_receiver_get_data_size tag =
   [C.exp| int { wb_receiver_get_data_size($(WbDeviceTag tag)) } |]
