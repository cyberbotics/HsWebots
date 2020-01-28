
wb_radio_message_new :: CInt -> String -> String -> IO WbRadioMessage 
wb_radio_message_new length body destination =
   [C.exp| WbRadioMessage { wb_radio_message_new($(int length), $(const char* body), $(const char* destination)) } |]

wb_radio_message_delete :: WbRadioMessage -> IO () 
wb_radio_message_delete  =
   [C.exp| void { wb_radio_message_delete() } |]

wb_radio_message_get_destination :: WbRadioMessage -> IO String 
wb_radio_message_get_destination  =
   [C.exp| const char* { wb_radio_message_get_destination() } |]

wb_radio_message_get_length :: WbRadioMessage -> IO CInt 
wb_radio_message_get_length  =
   [C.exp| int { wb_radio_message_get_length() } |]

wb_radio_message_get_body :: WbRadioMessage -> IO String 
wb_radio_message_get_body  =
   [C.exp| const char* { wb_radio_message_get_body() } |]

wb_radio_enable :: WbDeviceTag -> CInt -> IO () 
wb_radio_enable tag sampling_period =
   [C.exp| void { wb_radio_enable($(WbDeviceTag tag), $(int sampling_period)) } |]

wb_radio_disable :: WbDeviceTag -> IO () 
wb_radio_disable tag =
   [C.exp| void { wb_radio_disable($(WbDeviceTag tag)) } |]

wb_radio_set_address :: WbDeviceTag -> String -> IO () 
wb_radio_set_address tag address =
   [C.exp| void { wb_radio_set_address($(WbDeviceTag tag), $(const char* address)) } |]

wb_radio_get_address :: WbDeviceTag -> IO String 
wb_radio_get_address tag =
   [C.exp| const char* { wb_radio_get_address($(WbDeviceTag tag)) } |]

wb_radio_set_frequency :: WbDeviceTag -> CDouble -> IO () 
wb_radio_set_frequency tag hz =
   [C.exp| void { wb_radio_set_frequency($(WbDeviceTag tag), $(double hz)) } |]

wb_radio_get_frequency :: WbDeviceTag -> IO CDouble 
wb_radio_get_frequency tag =
   [C.exp| double { wb_radio_get_frequency($(WbDeviceTag tag)) } |]

wb_radio_set_channel :: WbDeviceTag -> CInt -> IO () 
wb_radio_set_channel tag channel =
   [C.exp| void { wb_radio_set_channel($(WbDeviceTag tag), $(int channel)) } |]

wb_radio_get_channel :: WbDeviceTag -> IO CInt 
wb_radio_get_channel tag =
   [C.exp| int { wb_radio_get_channel($(WbDeviceTag tag)) } |]

wb_radio_set_bitrate :: WbDeviceTag -> CInt -> IO () 
wb_radio_set_bitrate tag bits_per_second =
   [C.exp| void { wb_radio_set_bitrate($(WbDeviceTag tag), $(int bits_per_second)) } |]

wb_radio_get_bitrate :: WbDeviceTag -> IO CInt 
wb_radio_get_bitrate tag =
   [C.exp| int { wb_radio_get_bitrate($(WbDeviceTag tag)) } |]

wb_radio_set_rx_sensitivity :: WbDeviceTag -> CDouble -> IO () 
wb_radio_set_rx_sensitivity tag dBm =
   [C.exp| void { wb_radio_set_rx_sensitivity($(WbDeviceTag tag), $(double dBm)) } |]

wb_radio_get_rx_sensitivity :: WbDeviceTag -> IO CDouble 
wb_radio_get_rx_sensitivity tag =
   [C.exp| double { wb_radio_get_rx_sensitivity($(WbDeviceTag tag)) } |]

wb_radio_set_tx_power :: WbDeviceTag -> CDouble -> IO () 
wb_radio_set_tx_power tag dBm =
   [C.exp| void { wb_radio_set_tx_power($(WbDeviceTag tag), $(double dBm)) } |]

wb_radio_get_tx_power :: WbDeviceTag -> IO CDouble 
wb_radio_get_tx_power tag =
   [C.exp| double { wb_radio_get_tx_power($(WbDeviceTag tag)) } |]

wb_radio_set_callback :: WbDeviceTag -> () -> IO () 
wb_radio_set_callback tag xxx =
   [C.exp| void { wb_radio_set_callback($(WbDeviceTag tag), $(void xxx)) } |]

wb_radio_event_get_radio :: const,WbRadioEvent -> IO WbDeviceTag 
wb_radio_event_get_radio  =
   [C.exp| WbDeviceTag { wb_radio_event_get_radio() } |]

wb_radio_event_get_data :: const,WbRadioEvent -> IO CBool 
wb_radio_event_get_data  =
   [C.exp| bool { wb_radio_event_get_data() } |]

wb_radio_event_get_data_size :: const,WbRadioEvent -> IO CInt 
wb_radio_event_get_data_size  =
   [C.exp| int { wb_radio_event_get_data_size() } |]

wb_radio_event_get_emitter :: const,WbRadioEvent -> IO CBool 
wb_radio_event_get_emitter  =
   [C.exp| bool { wb_radio_event_get_emitter() } |]

wb_radio_event_get_rssi :: const,WbRadioEvent -> IO CDouble 
wb_radio_event_get_rssi  =
   [C.exp| double { wb_radio_event_get_rssi() } |]

wb_radio_send :: WbDeviceTag -> const,WbRadioMessage -> CDouble -> IO () 
wb_radio_send tag delay =
   [C.exp| void { wb_radio_send($(WbDeviceTag tag), $(const,WbRadioMessage delay)) } |]
