
wb_speaker_play_sound :: WbDeviceTag -> WbDeviceTag -> String -> CDouble -> CDouble -> CDouble -> CBool -> IO () 
wb_speaker_play_sound left right sound volume pitch balance loop =
   [C.exp| void { wb_speaker_play_sound($(WbDeviceTag left), $(WbDeviceTag right), $(const char* sound), $(double volume), $(double pitch), $(double balance), $(bool loop)) } |]

wb_speaker_stop :: WbDeviceTag -> String -> IO () 
wb_speaker_stop tag sound =
   [C.exp| void { wb_speaker_stop($(WbDeviceTag tag), $(const char* sound)) } |]

wb_speaker_is_sound_playing :: WbDeviceTag -> String -> IO CBool 
wb_speaker_is_sound_playing tag sound =
   [C.exp| bool { wb_speaker_is_sound_playing($(WbDeviceTag tag), $(const char* sound)) } |]

wb_speaker_set_engine :: WbDeviceTag -> String -> IO CBool 
wb_speaker_set_engine tag engine =
   [C.exp| bool { wb_speaker_set_engine($(WbDeviceTag tag), $(const char* engine)) } |]

wb_speaker_set_language :: WbDeviceTag -> String -> IO CBool 
wb_speaker_set_language tag language =
   [C.exp| bool { wb_speaker_set_language($(WbDeviceTag tag), $(const char* language)) } |]

wb_speaker_get_engine :: WbDeviceTag -> IO String 
wb_speaker_get_engine tag =
   [C.exp| const char* { wb_speaker_get_engine($(WbDeviceTag tag)) } |]

wb_speaker_get_language :: WbDeviceTag -> IO String 
wb_speaker_get_language tag =
   [C.exp| const char* { wb_speaker_get_language($(WbDeviceTag tag)) } |]

wb_speaker_speak :: WbDeviceTag -> String -> CDouble -> IO () 
wb_speaker_speak tag text volume =
   [C.exp| void { wb_speaker_speak($(WbDeviceTag tag), $(const char* text), $(double volume)) } |]

wb_speaker_is_speaking :: WbDeviceTag -> IO CBool 
wb_speaker_is_speaking tag =
   [C.exp| bool { wb_speaker_is_speaking($(WbDeviceTag tag)) } |]
