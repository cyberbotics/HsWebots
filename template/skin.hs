
wb_skin_set_bone_orientation :: WbDeviceTag -> CInt -> Ptr CDouble -> CBool -> IO () 
wb_skin_set_bone_orientation tag index orientation absolute =
   [C.exp| void { wb_skin_set_bone_orientation($(WbDeviceTag tag), $(int index), $(const double* orientation), $(bool absolute)) } |]

wb_skin_set_bone_position :: WbDeviceTag -> CInt -> Ptr CDouble -> CBool -> IO () 
wb_skin_set_bone_position tag index position absolute =
   [C.exp| void { wb_skin_set_bone_position($(WbDeviceTag tag), $(int index), $(const double* position), $(bool absolute)) } |]

wb_skin_get_bone_count :: WbDeviceTag -> IO CInt 
wb_skin_get_bone_count tag =
   [C.exp| int { wb_skin_get_bone_count($(WbDeviceTag tag)) } |]

wb_skin_get_bone_name :: WbDeviceTag -> CInt -> IO String 
wb_skin_get_bone_name tag index =
   [C.exp| const char* { wb_skin_get_bone_name($(WbDeviceTag tag), $(int index)) } |]

wb_skin_get_bone_orientation :: WbDeviceTag -> CInt -> CBool -> IO Ptr CDouble 
wb_skin_get_bone_orientation tag index absolute =
   [C.exp| const double* { wb_skin_get_bone_orientation($(WbDeviceTag tag), $(int index), $(bool absolute)) } |]

wb_skin_get_bone_position :: WbDeviceTag -> CInt -> CBool -> IO Ptr CDouble 
wb_skin_get_bone_position tag index absolute =
   [C.exp| const double* { wb_skin_get_bone_position($(WbDeviceTag tag), $(int index), $(bool absolute)) } |]
