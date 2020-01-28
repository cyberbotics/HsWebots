
wb_differential_wheels_set_speed :: CDouble -> CDouble -> IO () 
wb_differential_wheels_set_speed left right =
   [C.exp| void { wb_differential_wheels_set_speed($(double left), $(double right)) } |]

wb_differential_wheels_get_left_speed :: IO CDouble 
wb_differential_wheels_get_left_speed  =
   [C.exp| double { wb_differential_wheels_get_left_speed() } |]

wb_differential_wheels_get_right_speed :: IO CDouble 
wb_differential_wheels_get_right_speed  =
   [C.exp| double { wb_differential_wheels_get_right_speed() } |]

wb_differential_wheels_get_max_speed :: IO CDouble 
wb_differential_wheels_get_max_speed  =
   [C.exp| double { wb_differential_wheels_get_max_speed() } |]

wb_differential_wheels_get_speed_unit :: IO CDouble 
wb_differential_wheels_get_speed_unit  =
   [C.exp| double { wb_differential_wheels_get_speed_unit() } |]

wb_differential_wheels_enable_encoders :: CInt -> IO () 
wb_differential_wheels_enable_encoders sampling_period =
   [C.exp| void { wb_differential_wheels_enable_encoders($(int sampling_period)) } |]

wb_differential_wheels_disable_encoders :: IO () 
wb_differential_wheels_disable_encoders  =
   [C.exp| void { wb_differential_wheels_disable_encoders() } |]

wb_differential_wheels_get_encoders_sampling_period :: IO CInt 
wb_differential_wheels_get_encoders_sampling_period  =
   [C.exp| int { wb_differential_wheels_get_encoders_sampling_period() } |]

wb_differential_wheels_get_left_encoder :: IO CDouble 
wb_differential_wheels_get_left_encoder  =
   [C.exp| double { wb_differential_wheels_get_left_encoder() } |]

wb_differential_wheels_get_right_encoder :: IO CDouble 
wb_differential_wheels_get_right_encoder  =
   [C.exp| double { wb_differential_wheels_get_right_encoder() } |]

wb_differential_wheels_set_encoders :: CDouble -> CDouble -> IO () 
wb_differential_wheels_set_encoders left right =
   [C.exp| void { wb_differential_wheels_set_encoders($(double left), $(double right)) } |]
