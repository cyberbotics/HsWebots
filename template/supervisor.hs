
wb_supervisor_world_load :: String -> IO () 
wb_supervisor_world_load filename =
   [C.exp| void { wb_supervisor_world_load($(const char* filename)) } |]

wb_supervisor_world_save :: String -> IO CBool 
wb_supervisor_world_save filename =
   [C.exp| bool { wb_supervisor_world_save($(const char* filename)) } |]

wb_supervisor_world_reload :: IO () 
wb_supervisor_world_reload  =
   [C.exp| void { wb_supervisor_world_reload() } |]

wb_supervisor_simulation_quit :: CInt -> IO () 
wb_supervisor_simulation_quit status =
   [C.exp| void { wb_supervisor_simulation_quit($(int status)) } |]

wb_supervisor_simulation_reset :: IO () 
wb_supervisor_simulation_reset  =
   [C.exp| void { wb_supervisor_simulation_reset() } |]

wb_supervisor_simulation_reset_physics :: IO () 
wb_supervisor_simulation_reset_physics  =
   [C.exp| void { wb_supervisor_simulation_reset_physics() } |]

wb_supervisor_simulation_get_mode :: IO WbSimulationMode 
wb_supervisor_simulation_get_mode  =
   [C.exp| WbSimulationMode { wb_supervisor_simulation_get_mode() } |]

wb_supervisor_simulation_set_mode :: WbSimulationMode -> IO () 
wb_supervisor_simulation_set_mode mode =
   [C.exp| void { wb_supervisor_simulation_set_mode($(WbSimulationMode mode)) } |]

wb_supervisor_set_label :: CInt -> String -> CDouble -> CDouble -> CDouble -> CInt -> CDouble -> String -> IO () 
wb_supervisor_set_label id text x y size color transparency font =
   [C.exp| void { wb_supervisor_set_label($(int id), $(const char* text), $(double x), $(double y), $(double size), $(int color), $(double transparency), $(const char* font)) } |]

wb_supervisor_export_image :: String -> CInt -> IO () 
wb_supervisor_export_image filename quality =
   [C.exp| void { wb_supervisor_export_image($(const char* filename), $(int quality)) } |]

wb_supervisor_movie_start_recording :: String -> CInt -> CInt -> CInt -> CInt -> CInt -> CBool -> IO () 
wb_supervisor_movie_start_recording filename width height codec quality acceleration caption =
   [C.exp| void { wb_supervisor_movie_start_recording($(const char* filename), $(int width), $(int height), $(int codec), $(int quality), $(int acceleration), $(bool caption)) } |]

wb_supervisor_movie_stop_recording :: IO () 
wb_supervisor_movie_stop_recording  =
   [C.exp| void { wb_supervisor_movie_stop_recording() } |]

wb_supervisor_movie_is_ready :: IO CBool 
wb_supervisor_movie_is_ready  =
   [C.exp| bool { wb_supervisor_movie_is_ready() } |]

wb_supervisor_movie_failed :: IO CBool 
wb_supervisor_movie_failed  =
   [C.exp| bool { wb_supervisor_movie_failed() } |]

wb_supervisor_animation_start_recording :: String -> IO CBool 
wb_supervisor_animation_start_recording filename =
   [C.exp| bool { wb_supervisor_animation_start_recording($(const char* filename)) } |]

wb_supervisor_animation_stop_recording :: IO CBool 
wb_supervisor_animation_stop_recording  =
   [C.exp| bool { wb_supervisor_animation_stop_recording() } |]

wb_supervisor_node_get_root :: IO WbNodeRef 
wb_supervisor_node_get_root  =
   [C.exp| WbNodeRef { wb_supervisor_node_get_root() } |]

wb_supervisor_node_get_self :: IO WbNodeRef 
wb_supervisor_node_get_self  =
   [C.exp| WbNodeRef { wb_supervisor_node_get_self() } |]

wb_supervisor_node_get_id :: WbNodeRef -> IO CInt 
wb_supervisor_node_get_id node =
   [C.exp| int { wb_supervisor_node_get_id($(WbNodeRef node)) } |]

wb_supervisor_node_get_from_id :: CInt -> IO WbNodeRef 
wb_supervisor_node_get_from_id id =
   [C.exp| WbNodeRef { wb_supervisor_node_get_from_id($(int id)) } |]

wb_supervisor_node_get_from_def :: String -> IO WbNodeRef 
wb_supervisor_node_get_from_def def =
   [C.exp| WbNodeRef { wb_supervisor_node_get_from_def($(const char* def)) } |]

wb_supervisor_node_get_parent_node :: WbNodeRef -> IO WbNodeRef 
wb_supervisor_node_get_parent_node node =
   [C.exp| WbNodeRef { wb_supervisor_node_get_parent_node($(WbNodeRef node)) } |]

wb_supervisor_node_get_selected :: IO WbNodeRef 
wb_supervisor_node_get_selected  =
   [C.exp| WbNodeRef { wb_supervisor_node_get_selected() } |]

wb_supervisor_node_get_type :: WbNodeRef -> IO WbNodeType 
wb_supervisor_node_get_type node =
   [C.exp| WbNodeType { wb_supervisor_node_get_type($(WbNodeRef node)) } |]

wb_supervisor_node_get_field :: WbNodeRef -> String -> IO WbFieldRef 
wb_supervisor_node_get_field node field_name =
   [C.exp| WbFieldRef { wb_supervisor_node_get_field($(WbNodeRef node), $(const char* field_name)) } |]

wb_supervisor_node_remove :: WbNodeRef -> IO () 
wb_supervisor_node_remove node =
   [C.exp| void { wb_supervisor_node_remove($(WbNodeRef node)) } |]

wb_supervisor_node_get_def :: WbNodeRef -> IO String 
wb_supervisor_node_get_def node =
   [C.exp| const char* { wb_supervisor_node_get_def($(WbNodeRef node)) } |]

wb_supervisor_node_get_type_name :: WbNodeRef -> IO String 
wb_supervisor_node_get_type_name node =
   [C.exp| const char* { wb_supervisor_node_get_type_name($(WbNodeRef node)) } |]

wb_supervisor_node_get_base_type_name :: WbNodeRef -> IO String 
wb_supervisor_node_get_base_type_name node =
   [C.exp| const char* { wb_supervisor_node_get_base_type_name($(WbNodeRef node)) } |]

wb_supervisor_node_get_center_of_mass :: WbNodeRef -> IO Ptr CDouble 
wb_supervisor_node_get_center_of_mass node =
   [C.exp| const double* { wb_supervisor_node_get_center_of_mass($(WbNodeRef node)) } |]

wb_supervisor_node_get_contact_point :: WbNodeRef -> CInt -> IO Ptr CDouble 
wb_supervisor_node_get_contact_point node index =
   [C.exp| const double* { wb_supervisor_node_get_contact_point($(WbNodeRef node), $(int index)) } |]

wb_supervisor_node_get_number_of_contact_points :: WbNodeRef -> IO CInt 
wb_supervisor_node_get_number_of_contact_points node =
   [C.exp| int { wb_supervisor_node_get_number_of_contact_points($(WbNodeRef node)) } |]

wb_supervisor_node_get_orientation :: WbNodeRef -> IO Ptr CDouble 
wb_supervisor_node_get_orientation node =
   [C.exp| const double* { wb_supervisor_node_get_orientation($(WbNodeRef node)) } |]

wb_supervisor_node_get_position :: WbNodeRef -> IO Ptr CDouble 
wb_supervisor_node_get_position node =
   [C.exp| const double* { wb_supervisor_node_get_position($(WbNodeRef node)) } |]

wb_supervisor_node_get_static_balance :: WbNodeRef -> IO CBool 
wb_supervisor_node_get_static_balance node =
   [C.exp| bool { wb_supervisor_node_get_static_balance($(WbNodeRef node)) } |]

wb_supervisor_node_get_velocity :: WbNodeRef -> IO Ptr CDouble 
wb_supervisor_node_get_velocity node =
   [C.exp| const double* { wb_supervisor_node_get_velocity($(WbNodeRef node)) } |]

wb_supervisor_node_set_velocity :: WbNodeRef -> Ptr CDouble -> IO () 
wb_supervisor_node_set_velocity node velocity =
   [C.exp| void { wb_supervisor_node_set_velocity($(WbNodeRef node), $(const double* velocity)) } |]

wb_supervisor_node_reset_physics :: WbNodeRef -> IO () 
wb_supervisor_node_reset_physics node =
   [C.exp| void { wb_supervisor_node_reset_physics($(WbNodeRef node)) } |]

wb_supervisor_node_restart_controller :: WbNodeRef -> IO () 
wb_supervisor_node_restart_controller node =
   [C.exp| void { wb_supervisor_node_restart_controller($(WbNodeRef node)) } |]

wb_supervisor_node_move_viewpoint :: WbNodeRef -> IO () 
wb_supervisor_node_move_viewpoint node =
   [C.exp| void { wb_supervisor_node_move_viewpoint($(WbNodeRef node)) } |]

wb_supervisor_node_set_visibility :: WbNodeRef -> WbNodeRef -> CBool -> IO () 
wb_supervisor_node_set_visibility node from visible =
   [C.exp| void { wb_supervisor_node_set_visibility($(WbNodeRef node), $(WbNodeRef from), $(bool visible)) } |]

wb_supervisor_field_get_type :: WbFieldRef -> IO WbFieldType 
wb_supervisor_field_get_type field =
   [C.exp| WbFieldType { wb_supervisor_field_get_type($(WbFieldRef field)) } |]

wb_supervisor_field_get_type_name :: WbFieldRef -> IO String 
wb_supervisor_field_get_type_name field =
   [C.exp| const char* { wb_supervisor_field_get_type_name($(WbFieldRef field)) } |]

wb_supervisor_field_get_count :: WbFieldRef -> IO CInt 
wb_supervisor_field_get_count field =
   [C.exp| int { wb_supervisor_field_get_count($(WbFieldRef field)) } |]

wb_supervisor_field_get_sf_bool :: WbFieldRef -> IO CBool 
wb_supervisor_field_get_sf_bool field =
   [C.exp| bool { wb_supervisor_field_get_sf_bool($(WbFieldRef field)) } |]

wb_supervisor_field_get_sf_int32 :: WbFieldRef -> IO CInt 
wb_supervisor_field_get_sf_int32 field =
   [C.exp| int { wb_supervisor_field_get_sf_int32($(WbFieldRef field)) } |]

wb_supervisor_field_get_sf_float :: WbFieldRef -> IO CDouble 
wb_supervisor_field_get_sf_float field =
   [C.exp| double { wb_supervisor_field_get_sf_float($(WbFieldRef field)) } |]

wb_supervisor_field_get_sf_vec2f :: WbFieldRef -> IO Ptr CDouble 
wb_supervisor_field_get_sf_vec2f field =
   [C.exp| const double* { wb_supervisor_field_get_sf_vec2f($(WbFieldRef field)) } |]

wb_supervisor_field_get_sf_vec3f :: WbFieldRef -> IO Ptr CDouble 
wb_supervisor_field_get_sf_vec3f field =
   [C.exp| const double* { wb_supervisor_field_get_sf_vec3f($(WbFieldRef field)) } |]

wb_supervisor_field_get_sf_rotation :: WbFieldRef -> IO Ptr CDouble 
wb_supervisor_field_get_sf_rotation field =
   [C.exp| const double* { wb_supervisor_field_get_sf_rotation($(WbFieldRef field)) } |]

wb_supervisor_field_get_sf_color :: WbFieldRef -> IO Ptr CDouble 
wb_supervisor_field_get_sf_color field =
   [C.exp| const double* { wb_supervisor_field_get_sf_color($(WbFieldRef field)) } |]

wb_supervisor_field_get_sf_string :: WbFieldRef -> IO String 
wb_supervisor_field_get_sf_string field =
   [C.exp| const char* { wb_supervisor_field_get_sf_string($(WbFieldRef field)) } |]

wb_supervisor_field_get_sf_node :: WbFieldRef -> IO WbNodeRef 
wb_supervisor_field_get_sf_node field =
   [C.exp| WbNodeRef { wb_supervisor_field_get_sf_node($(WbFieldRef field)) } |]

wb_supervisor_field_get_mf_bool :: WbFieldRef -> CInt -> IO CBool 
wb_supervisor_field_get_mf_bool field index =
   [C.exp| bool { wb_supervisor_field_get_mf_bool($(WbFieldRef field), $(int index)) } |]

wb_supervisor_field_get_mf_int32 :: WbFieldRef -> CInt -> IO CInt 
wb_supervisor_field_get_mf_int32 field index =
   [C.exp| int { wb_supervisor_field_get_mf_int32($(WbFieldRef field), $(int index)) } |]

wb_supervisor_field_get_mf_float :: WbFieldRef -> CInt -> IO CDouble 
wb_supervisor_field_get_mf_float field index =
   [C.exp| double { wb_supervisor_field_get_mf_float($(WbFieldRef field), $(int index)) } |]

wb_supervisor_field_get_mf_vec2f :: WbFieldRef -> CInt -> IO Ptr CDouble 
wb_supervisor_field_get_mf_vec2f field index =
   [C.exp| const double* { wb_supervisor_field_get_mf_vec2f($(WbFieldRef field), $(int index)) } |]

wb_supervisor_field_get_mf_vec3f :: WbFieldRef -> CInt -> IO Ptr CDouble 
wb_supervisor_field_get_mf_vec3f field index =
   [C.exp| const double* { wb_supervisor_field_get_mf_vec3f($(WbFieldRef field), $(int index)) } |]

wb_supervisor_field_get_mf_color :: WbFieldRef -> CInt -> IO Ptr CDouble 
wb_supervisor_field_get_mf_color field index =
   [C.exp| const double* { wb_supervisor_field_get_mf_color($(WbFieldRef field), $(int index)) } |]

wb_supervisor_field_get_mf_rotation :: WbFieldRef -> CInt -> IO Ptr CDouble 
wb_supervisor_field_get_mf_rotation field index =
   [C.exp| const double* { wb_supervisor_field_get_mf_rotation($(WbFieldRef field), $(int index)) } |]

wb_supervisor_field_get_mf_string :: WbFieldRef -> CInt -> IO String 
wb_supervisor_field_get_mf_string field index =
   [C.exp| const char* { wb_supervisor_field_get_mf_string($(WbFieldRef field), $(int index)) } |]

wb_supervisor_field_get_mf_node :: WbFieldRef -> CInt -> IO WbNodeRef 
wb_supervisor_field_get_mf_node field index =
   [C.exp| WbNodeRef { wb_supervisor_field_get_mf_node($(WbFieldRef field), $(int index)) } |]

wb_supervisor_field_set_sf_bool :: WbFieldRef -> CBool -> IO () 
wb_supervisor_field_set_sf_bool field value =
   [C.exp| void { wb_supervisor_field_set_sf_bool($(WbFieldRef field), $(bool value)) } |]

wb_supervisor_field_set_sf_int32 :: WbFieldRef -> CInt -> IO () 
wb_supervisor_field_set_sf_int32 field value =
   [C.exp| void { wb_supervisor_field_set_sf_int32($(WbFieldRef field), $(int value)) } |]

wb_supervisor_field_set_sf_float :: WbFieldRef -> CDouble -> IO () 
wb_supervisor_field_set_sf_float field value =
   [C.exp| void { wb_supervisor_field_set_sf_float($(WbFieldRef field), $(double value)) } |]

wb_supervisor_field_set_sf_vec2f :: WbFieldRef -> Ptr CDouble -> IO () 
wb_supervisor_field_set_sf_vec2f field values =
   [C.exp| void { wb_supervisor_field_set_sf_vec2f($(WbFieldRef field), $(const double* values)) } |]

wb_supervisor_field_set_sf_vec3f :: WbFieldRef -> Ptr CDouble -> IO () 
wb_supervisor_field_set_sf_vec3f field values =
   [C.exp| void { wb_supervisor_field_set_sf_vec3f($(WbFieldRef field), $(const double* values)) } |]

wb_supervisor_field_set_sf_rotation :: WbFieldRef -> Ptr CDouble -> IO () 
wb_supervisor_field_set_sf_rotation field values =
   [C.exp| void { wb_supervisor_field_set_sf_rotation($(WbFieldRef field), $(const double* values)) } |]

wb_supervisor_field_set_sf_color :: WbFieldRef -> Ptr CDouble -> IO () 
wb_supervisor_field_set_sf_color field values =
   [C.exp| void { wb_supervisor_field_set_sf_color($(WbFieldRef field), $(const double* values)) } |]

wb_supervisor_field_set_sf_string :: WbFieldRef -> String -> IO () 
wb_supervisor_field_set_sf_string field value =
   [C.exp| void { wb_supervisor_field_set_sf_string($(WbFieldRef field), $(const char* value)) } |]

wb_supervisor_field_set_mf_bool :: WbFieldRef -> CInt -> CBool -> IO () 
wb_supervisor_field_set_mf_bool field index value =
   [C.exp| void { wb_supervisor_field_set_mf_bool($(WbFieldRef field), $(int index), $(bool value)) } |]

wb_supervisor_field_set_mf_int32 :: WbFieldRef -> CInt -> CInt -> IO () 
wb_supervisor_field_set_mf_int32 field index value =
   [C.exp| void { wb_supervisor_field_set_mf_int32($(WbFieldRef field), $(int index), $(int value)) } |]

wb_supervisor_field_set_mf_float :: WbFieldRef -> CInt -> CDouble -> IO () 
wb_supervisor_field_set_mf_float field index value =
   [C.exp| void { wb_supervisor_field_set_mf_float($(WbFieldRef field), $(int index), $(double value)) } |]

wb_supervisor_field_set_mf_vec2f :: WbFieldRef -> CInt -> Ptr CDouble -> IO () 
wb_supervisor_field_set_mf_vec2f field index values =
   [C.exp| void { wb_supervisor_field_set_mf_vec2f($(WbFieldRef field), $(int index), $(const double* values)) } |]

wb_supervisor_field_set_mf_vec3f :: WbFieldRef -> CInt -> Ptr CDouble -> IO () 
wb_supervisor_field_set_mf_vec3f field index values =
   [C.exp| void { wb_supervisor_field_set_mf_vec3f($(WbFieldRef field), $(int index), $(const double* values)) } |]

wb_supervisor_field_set_mf_rotation :: WbFieldRef -> CInt -> Ptr CDouble -> IO () 
wb_supervisor_field_set_mf_rotation field index values =
   [C.exp| void { wb_supervisor_field_set_mf_rotation($(WbFieldRef field), $(int index), $(const double* values)) } |]

wb_supervisor_field_set_mf_color :: WbFieldRef -> CInt -> Ptr CDouble -> IO () 
wb_supervisor_field_set_mf_color field index values =
   [C.exp| void { wb_supervisor_field_set_mf_color($(WbFieldRef field), $(int index), $(const double* values)) } |]

wb_supervisor_field_set_mf_string :: WbFieldRef -> CInt -> String -> IO () 
wb_supervisor_field_set_mf_string field index value =
   [C.exp| void { wb_supervisor_field_set_mf_string($(WbFieldRef field), $(int index), $(const char* value)) } |]

wb_supervisor_field_insert_mf_bool :: WbFieldRef -> CInt -> CBool -> IO () 
wb_supervisor_field_insert_mf_bool field index value =
   [C.exp| void { wb_supervisor_field_insert_mf_bool($(WbFieldRef field), $(int index), $(bool value)) } |]

wb_supervisor_field_insert_mf_int32 :: WbFieldRef -> CInt -> CInt -> IO () 
wb_supervisor_field_insert_mf_int32 field index value =
   [C.exp| void { wb_supervisor_field_insert_mf_int32($(WbFieldRef field), $(int index), $(int value)) } |]

wb_supervisor_field_insert_mf_float :: WbFieldRef -> CInt -> CDouble -> IO () 
wb_supervisor_field_insert_mf_float field index value =
   [C.exp| void { wb_supervisor_field_insert_mf_float($(WbFieldRef field), $(int index), $(double value)) } |]

wb_supervisor_field_insert_mf_vec2f :: WbFieldRef -> CInt -> Ptr CDouble -> IO () 
wb_supervisor_field_insert_mf_vec2f field index values =
   [C.exp| void { wb_supervisor_field_insert_mf_vec2f($(WbFieldRef field), $(int index), $(const double* values)) } |]

wb_supervisor_field_insert_mf_vec3f :: WbFieldRef -> CInt -> Ptr CDouble -> IO () 
wb_supervisor_field_insert_mf_vec3f field index values =
   [C.exp| void { wb_supervisor_field_insert_mf_vec3f($(WbFieldRef field), $(int index), $(const double* values)) } |]

wb_supervisor_field_insert_mf_rotation :: WbFieldRef -> CInt -> Ptr CDouble -> IO () 
wb_supervisor_field_insert_mf_rotation field index values =
   [C.exp| void { wb_supervisor_field_insert_mf_rotation($(WbFieldRef field), $(int index), $(const double* values)) } |]

wb_supervisor_field_insert_mf_color :: WbFieldRef -> CInt -> Ptr CDouble -> IO () 
wb_supervisor_field_insert_mf_color field index values =
   [C.exp| void { wb_supervisor_field_insert_mf_color($(WbFieldRef field), $(int index), $(const double* values)) } |]

wb_supervisor_field_insert_mf_string :: WbFieldRef -> CInt -> String -> IO () 
wb_supervisor_field_insert_mf_string field index value =
   [C.exp| void { wb_supervisor_field_insert_mf_string($(WbFieldRef field), $(int index), $(const char* value)) } |]

wb_supervisor_field_remove_mf :: WbFieldRef -> CInt -> IO () 
wb_supervisor_field_remove_mf field index =
   [C.exp| void { wb_supervisor_field_remove_mf($(WbFieldRef field), $(int index)) } |]

wb_supervisor_field_import_mf_node :: WbFieldRef -> CInt -> String -> IO () 
wb_supervisor_field_import_mf_node field position filename =
   [C.exp| void { wb_supervisor_field_import_mf_node($(WbFieldRef field), $(int position), $(const char* filename)) } |]

wb_supervisor_field_import_mf_node_from_string :: WbFieldRef -> CInt -> String -> IO () 
wb_supervisor_field_import_mf_node_from_string field position node_string =
   [C.exp| void { wb_supervisor_field_import_mf_node_from_string($(WbFieldRef field), $(int position), $(const char* node_string)) } |]

wb_supervisor_virtual_reality_headset_is_used :: IO CBool 
wb_supervisor_virtual_reality_headset_is_used  =
   [C.exp| bool { wb_supervisor_virtual_reality_headset_is_used() } |]

wb_supervisor_virtual_reality_headset_get_position :: IO Ptr CDouble 
wb_supervisor_virtual_reality_headset_get_position  =
   [C.exp| const double* { wb_supervisor_virtual_reality_headset_get_position() } |]

wb_supervisor_virtual_reality_headset_get_orientation :: IO Ptr CDouble 
wb_supervisor_virtual_reality_headset_get_orientation  =
   [C.exp| const double* { wb_supervisor_virtual_reality_headset_get_orientation() } |]

wb_supervisor_simulation_revert :: IO () 
wb_supervisor_simulation_revert  =
   [C.exp| void { wb_supervisor_simulation_revert() } |]

wb_supervisor_load_world :: String -> IO () 
wb_supervisor_load_world filename =
   [C.exp| void { wb_supervisor_load_world($(const char* filename)) } |]

wb_supervisor_save_world :: String -> IO CBool 
wb_supervisor_save_world filename =
   [C.exp| bool { wb_supervisor_save_world($(const char* filename)) } |]

wb_supervisor_field_remove_mf_node :: WbFieldRef -> CInt -> IO () 
wb_supervisor_field_remove_mf_node field position =
   [C.exp| void { wb_supervisor_field_remove_mf_node($(WbFieldRef field), $(int position)) } |]

wb_supervisor_simulation_physics_reset :: IO () 
wb_supervisor_simulation_physics_reset  =
   [C.exp| void { wb_supervisor_simulation_physics_reset() } |]

wb_supervisor_movie_get_status :: IO CInt 
wb_supervisor_movie_get_status  =
   [C.exp| int { wb_supervisor_movie_get_status() } |]

wb_supervisor_start_movie :: String -> CInt -> CInt -> CInt -> CInt -> CInt -> CBool -> IO () 
wb_supervisor_start_movie file width height codec quality acceleration caption =
   [C.exp| void { wb_supervisor_start_movie($(const char* file), $(int width), $(int height), $(int codec), $(int quality), $(int acceleration), $(bool caption)) } |]

wb_supervisor_stop_movie :: IO () 
wb_supervisor_stop_movie  =
   [C.exp| void { wb_supervisor_stop_movie() } |]

wb_supervisor_get_movie_status :: IO CInt 
wb_supervisor_get_movie_status  =
   [C.exp| int { wb_supervisor_get_movie_status() } |]
