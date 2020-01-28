
wb_console_print :: String -> CInt -> IO () 
wb_console_print text stream =
   [C.exp| void { wb_console_print($(const char* text), $(int stream)) } |]
