
wb_node_get_name :: WbNodeType -> IO String 
wb_node_get_name t =
   [C.exp| const char* { wb_node_get_name($(WbNodeType t)) } |]
