
wb_robot_wwi_send :: String -> CInt -> IO () 
wb_robot_wwi_send data size =
   [C.exp| void { wb_robot_wwi_send($(const char* data), $(int size)) } |]

wb_robot_wwi_receive :: CInt -> IO String 
wb_robot_wwi_receive size =
   [C.exp| const char* { wb_robot_wwi_receive($(int size)) } |]
