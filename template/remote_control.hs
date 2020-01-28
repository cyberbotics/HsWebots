
wb_remote_control_custom_function :: () -> IO () 
wb_remote_control_custom_function xxx =
   [C.exp| void { wb_remote_control_custom_function($(void xxx)) } |]

wbr_robot_battery_sensor_set_value :: CDouble -> IO () 
wbr_robot_battery_sensor_set_value value =
   [C.exp| void { wbr_robot_battery_sensor_set_value($(double value)) } |]

wbr_differential_wheels_set_encoders :: CDouble -> CDouble -> IO () 
wbr_differential_wheels_set_encoders left right =
   [C.exp| void { wbr_differential_wheels_set_encoders($(double left), $(double right)) } |]

wbr_accelerometer_set_values :: WbDeviceTag -> Ptr CDouble -> IO () 
wbr_accelerometer_set_values tag values =
   [C.exp| void { wbr_accelerometer_set_values($(WbDeviceTag tag), $(const double* values)) } |]

wbr_camera_recognition_set_object :: WbDeviceTag -> const,WbCameraRecognitionObject -> CInt -> IO () 
wbr_camera_recognition_set_object tag objects object_number =
   [C.exp| void { wbr_camera_recognition_set_object($(WbDeviceTag tag), $(const,WbCameraRecognitionObject objects), $(int object_number)) } |]

wbr_compass_set_values :: WbDeviceTag -> Ptr CDouble -> IO () 
wbr_compass_set_values tag values =
   [C.exp| void { wbr_compass_set_values($(WbDeviceTag tag), $(const double* values)) } |]

wbr_distance_sensor_set_value :: WbDeviceTag -> CDouble -> IO () 
wbr_distance_sensor_set_value tag value =
   [C.exp| void { wbr_distance_sensor_set_value($(WbDeviceTag tag), $(double value)) } |]

wbr_gps_set_values :: WbDeviceTag -> Ptr CDouble -> IO () 
wbr_gps_set_values tag values =
   [C.exp| void { wbr_gps_set_values($(WbDeviceTag tag), $(const double* values)) } |]

wbr_gps_set_speed :: WbDeviceTag -> Ptr CDouble -> IO () 
wbr_gps_set_speed tag speed =
   [C.exp| void { wbr_gps_set_speed($(WbDeviceTag tag), $(const double* speed)) } |]

wbr_gyro_set_values :: WbDeviceTag -> Ptr CDouble -> IO () 
wbr_gyro_set_values tag values =
   [C.exp| void { wbr_gyro_set_values($(WbDeviceTag tag), $(const double* values)) } |]

wbr_inertial_unit_set_value :: WbDeviceTag -> CDouble -> IO () 
wbr_inertial_unit_set_value tag value =
   [C.exp| void { wbr_inertial_unit_set_value($(WbDeviceTag tag), $(double value)) } |]

wbr_light_sensor_set_value :: WbDeviceTag -> CDouble -> IO () 
wbr_light_sensor_set_value tag value =
   [C.exp| void { wbr_light_sensor_set_value($(WbDeviceTag tag), $(double value)) } |]

wbr_microphone_set_buffer :: WbDeviceTag -> const,unsigned,char -> CInt -> IO () 
wbr_microphone_set_buffer tag buffer size =
   [C.exp| void { wbr_microphone_set_buffer($(WbDeviceTag tag), $(const,unsigned,char buffer), $(int size)) } |]

wbr_motor_set_position_feedback :: WbDeviceTag -> CDouble -> IO () 
wbr_motor_set_position_feedback tag value =
   [C.exp| void { wbr_motor_set_position_feedback($(WbDeviceTag tag), $(double value)) } |]

wbr_motor_set_force_feedback :: WbDeviceTag -> CDouble -> IO () 
wbr_motor_set_force_feedback tag value =
   [C.exp| void { wbr_motor_set_force_feedback($(WbDeviceTag tag), $(double value)) } |]

wbr_motor_set_torque_feedback :: WbDeviceTag -> CDouble -> IO () 
wbr_motor_set_torque_feedback tag value =
   [C.exp| void { wbr_motor_set_torque_feedback($(WbDeviceTag tag), $(double value)) } |]

wbr_position_sensor_set_value :: WbDeviceTag -> CDouble -> IO () 
wbr_position_sensor_set_value tag value =
   [C.exp| void { wbr_position_sensor_set_value($(WbDeviceTag tag), $(double value)) } |]

wbr_radar_set_targets :: WbDeviceTag -> const,WbRadarTarget -> CInt -> IO () 
wbr_radar_set_targets tag targets target_number =
   [C.exp| void { wbr_radar_set_targets($(WbDeviceTag tag), $(const,WbRadarTarget targets), $(int target_number)) } |]

wbr_touch_sensor_set_value :: WbDeviceTag -> CDouble -> IO () 
wbr_touch_sensor_set_value tag value =
   [C.exp| void { wbr_touch_sensor_set_value($(WbDeviceTag tag), $(double value)) } |]

wbr_touch_sensor_set_values :: WbDeviceTag -> Ptr CDouble -> IO () 
wbr_touch_sensor_set_values tag values =
   [C.exp| void { wbr_touch_sensor_set_values($(WbDeviceTag tag), $(const double* values)) } |]

wbr_display_save_image :: WbDeviceTag -> CInt -> CInt -> CInt -> unsigned,char -> IO () 
wbr_display_save_image tag id width height image =
   [C.exp| void { wbr_display_save_image($(WbDeviceTag tag), $(int id), $(int width), $(int height), $(unsigned,char image)) } |]

wbr_camera_set_image :: WbDeviceTag -> const,unsigned,char -> IO () 
wbr_camera_set_image tag image =
   [C.exp| void { wbr_camera_set_image($(WbDeviceTag tag), $(const,unsigned,char image)) } |]
