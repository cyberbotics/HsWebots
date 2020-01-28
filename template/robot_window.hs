
wb_robot_window_custom_function :: () -> IO () 
wb_robot_window_custom_function xxx =
   [C.exp| void { wb_robot_window_custom_function($(void xxx)) } |]
