;; ============================================================================
;; IDENTIFIERS
;; ============================================================================
(identifier) @variable

;; ============================================================================
;; CONSTANTS
;; ============================================================================
((identifier) @constant
 (#match? @constant "^[A-Z][A-Z\\d_]*$"))

(system_constant) @constant.builtin

;; ============================================================================
;; VARIABLES
;; ============================================================================
; (system_variable) @variable.builtin
(compiler_variable) @variable.builtin

;; ============================================================================
;; KEYWORDS
;; ============================================================================
[
    (break_keyword)
    (case_keyword)
    (continue_keyword)
    (default_keyword)
    (else_keyword)
    (for_keyword)
    (if_keyword)
    (return_keyword)
    (switch_keyword)
    (while_keyword)
    (select_keyword)
    (active_keyword)

    (struct_keyword)
    (structure_keyword)

    (program_name_keyword)
    (module_name_keyword)

    (define_device_keyword)
    (define_constant_keyword)
    (define_type_keyword)
    (define_variable_keyword)
    (define_system_variable_keyword)
    (define_start_keyword)
    (define_event_keyword)
    (define_mutually_exclusive_keyword)
    (define_function_keyword)
    (define_library_function_keyword)
    (define_combine_keyword)
    (define_connect_level_keyword)
    (define_latching_keyword)
    (define_toggling_keyword)
    (define_program_keyword)
    (define_call_keyword)
    (define_module_keyword)

    (push_keyword)
    (release_keyword)
    (hold_keyword)
    (repeat_keyword)

    (on_keyword)
    (off_keyword)

    (online_keyword)
    (offline_keyword)
    (onerror_keyword)
    (string_keyword)
    (command_keyword)
    (awake_keyword)
    (standby_keyword)

    (send_level_keyword)
    (send_string_keyword)
    (send_command_keyword)
    (clear_buffer_keyword)
    (create_buffer_keyword)
    (create_multi_buffer_keyword)
    (call_keyword)
    (system_call_keyword)

    (devchan_on_keyword)
    (devchan_off_keyword)
    (devchan_to_keyword)
    (devchan_min_to_keyword)
    (devchan_total_off_keyword)
    (devchan_pulse_keyword)

    (wait_keyword)
    (wait_until_keyword)
    (cancel_wait_keyword)
    (cancel_wait_until_keyword)
    (cancel_all_wait_keyword)
    (cancel_all_wait_until_keyword)

    (button_event_keyword)
    (channel_event_keyword)
    (data_event_keyword)
    (level_event_keyword)
    (timeline_event_keyword)
    (custom_event_keyword)

    (band)
    (bor)
    (bxor)
    (bnot)
    (lshift)
    (rshift)

    (and)
    (or)
    (not)
    (xor)
] @keyword

;; ============================================================================
;; QUALIFIERS
;; ============================================================================
[
    (constant_keyword)
    (volatile_keyword)
    (non_volatile_keyword)
    (persistent_keyword)
] @keyword.qualifier

;; ============================================================================
;; STORAGE CLASSES
;; ============================================================================
[
    (local_var_keyword)
    (stack_var_keyword)
] @keyword.storage

;; ============================================================================
;; PREPROCESSOR
;; ============================================================================
[
    (preproc_include_keyword)
    (preproc_define_keyword)
    (preproc_warn_keyword)
    (preproc_disable_warning_keyword)
    (preproc_if_defined_keyword)
    (preproc_if_not_defined_keyword)
    (preproc_else_keyword)
    (preproc_end_if_keyword)
] @keyword.directive

;; Preproc Arguments
;; Highlight all preproc_args initially as constants
(preproc_arg) @constant

;; Then specifically override numeric values
((preproc_arg) @number
 (#match? @number "^[ \t]*[0-9]+[ \t]*$"))

;; And string values with quotes
((preproc_arg) @string
 (#match? @string "'"))

;; ============================================================================
;; OPERATORS
;; ============================================================================
[
    "="
    "+"
    "-"
    "*"
    "/"
    "%"
    ">"
    "<"
    "&"
    "|"
    "^"
    "!"
    "~"
    "&&"
    "||"
    "=="
    "!="
    "<="
    ">="
    "<<"
    ">>"
    "++"
    "--"
    "<>"
    (range_operator)
] @operator

;; ============================================================================
;; PUNCTUATION
;; ============================================================================
[
    "("
    ")"
    "{"
    "}"
    "["
    "]"
] @punctuation.bracket

[
    "."
    ";"
    ","
    ":"
] @punctuation.delimiter

;; ============================================================================
;; LITERALS
;; ============================================================================
(string_literal) @string
(number_literal) @number
(device_literal) @number
"\"" @string.special
((string_literal) @string.regex
  (#match? @string.regex "^'/.*/'$"))

[
  (true)
  (false)
] @boolean

;; ============================================================================
;; TYPES
;; ============================================================================
(type_identifier) @type.custom
(primitive_type) @type
(structured_type) @type.builtin

;; ============================================================================
;; PROPERTIES
;; ============================================================================
(field_identifier) @property

;; ============================================================================
;; FUNCTIONS
;; ============================================================================
(call_expression
  function: (system_function) @function.builtin)
(call_expression
  function: (identifier) @function)
; (call_expression
;   function: (field_expression
;     field: (field_identifier) @function))
(function_definition
  name: (identifier) @function)
; (function_declarator
;   declarator: (identifier) @function)

;; ============================================================================
;; FUNCTION PARAMETERS
;; May remove this if can't get local scoping to work properly
;; ============================================================================
; (parameter_declaration
;   declarator: [
;     (identifier)
;     (array_declarator declarator: (identifier))
;     (array_declarator declarator: (array_declarator declarator: (identifier)))
;     (array_declarator declarator: (array_declarator declarator: (array_declarator declarator: (identifier))))
;     (array_declarator declarator: (array_declarator declarator: (array_declarator declarator: (array_declarator declarator: (identifier)))))
;   ] @parameter)

;; ============================================================================
;; LIBRARY FUNCTIONS
;; ============================================================================
(function_declaration
  name: (identifier) @function)

;; ============================================================================
;; COMMENTS
;; ============================================================================
(comment) @comment

;; ============================================================================
;; SNAPI HIGHLIGHTS
;; ============================================================================
((call_expression
  function: (identifier) @function.builtin)
  (#match? @function.builtin "(?i)^(duetpackcmdheader|duetpackcmdparam|duetpackcmdparamarray|duetpackcmdsimple|duetparsecmdheader|duetparsecmdparam)$"))

((identifier) @constant.builtin
  (#match? @constant.builtin "(?i)^(snapi_axi_version|invalid_number|invalid_string|invalid_enum|duet_min_level_value|duet_max_level_value|power|digit_0|digit_1|digit_2|digit_3|digit_4|digit_5|digit_6|digit_7|digit_8|digit_9|menu_plus_10|menu_enter|vol_up|vol_up_fb|vol_dn|vol_dn_fb|vol_mute|pwr_on|pwr_off|vol_preset|vol_mute_on|vol_mute_fb|device_communicating|data_initialized|power_on|power_fb|menu_cancel|menu_func|menu_up|menu_dn|menu_lt|menu_rt|menu_select|menu_exit|menu_up_lt|menu_up_rt|menu_dn_lt|menu_dn_rt|menu_video|menu_thumbs_dn|menu_thumbs_up|menu_accept|menu_reject|menu_live_tv|menu_sleep|menu_ppv|menu_function|menu_setup|menu_xm|menu_fm|menu_am|menu_clear|menu_back|menu_forward|menu_advance|menu_dimmer|menu_hold|menu_list|menu_lt_paren|menu_rt_paren|menu_underscore|menu_dash|menu_asterisk|menu_dot|menu_pound|menu_comma|menu_dial|menu_conference|menu_plus_100|menu_plus_1000|menu_display|menu_subtitle|menu_info|menu_favorites|menu_continue|menu_return|menu_guide|menu_page_up|menu_page_dn|menu_deck_a_b|menu_tv_vcr|menu_record_speed|menu_program|menu_ab_repeat|menu_help|menu_title|menu_top_menu|menu_zoom|menu_angle|menu_audio|menu_preview_input|menu_send_input|menu_send_graphics|menu_flash|menu_reset|menu_instant_replay|audioproc_level_up|audioproc_level_dn|audioproc_state|audioproc_state_on|audioproc_state_fb|audioproc_preset|audioproc_lvl|play|stop|pause|ffwd|rew|sfwd|srev|record|play_fb|stop_fb|pause_fb|ffwd_fb|rew_fb|sfwd_fb|srev_fb|record_fb|gain_up|gain_dn|gain_up_fb|gain_dn_fb|gain_mute|gain_mute_on|gain_mute_fb|cass_reverse_play|cass_tape_side|cass_tape_side_a|cass_tape_side_a_fb|cass_tape_side_b|cass_tape_side_b_fb|cass_record_mute|cass_record_mute_on|cass_record_mute_fb|search_speed|eject|reset_counter|tape_loaded_fb|record_lock_fb|slow_fwd|slow_rev|slow_fwd_fb|slow_rev_fb|aconf_privacy|aconf_privacy_on|aconf_privacy_fb|aconf_train|dial_redial|dial_off_hook|dial_auto_answer|dial_audible_ring|dial_flash_hook|dial_off_hook_on|dial_off_hook_fb|dial_auto_answer_on|dial_auto_answer_fb|dial_audible_ring_on|dial_audible_ring_fb|vol_lvl|gain_lvl|chan_up|chan_dn|tuner_band|tuner_preset_group|tuner_station_up|tuner_station_dn|tuner_scan_fwd|tuner_scan_rev|tuner_seek_fwd|tuner_seek_rev|tuner_osd|tuner_prev|tilt_up|tilt_up_fb|tilt_dn|tilt_dn_fb|pan_lt|pan_lt_fb|pan_rt|pan_rt_fb|zoom_out|zoom_out_fb|zoom_in|zoom_in_fb|focus_near|focus_near_fb|focus_far|focus_far_fb|auto_focus_on|auto_focus_fb|auto_iris_on|auto_iris_fb|auto_focus|auto_iris|iris_open|iris_open_fb|iris_close|iris_close_fb|cam_preset|zoom_lvl|focus_lvl|iris_lvl|zoom_speed_lvl|focus_speed_lvl|iris_speed_lvl|pan_lvl|tilt_lvl|pan_speed_lvl|tilt_speed_lvl|media_random|media_repeat|media_random_disc_on|media_random_disc_fb|media_random_all_on|media_random_all_fb|media_random_off_on|media_random_off_fb|media_repeat_disc_on|media_repeat_disc_fb|media_repeat_track_on|media_repeat_track_fb|media_repeat_all_on|media_repeat_all_fb|media_repeat_off_on|media_repeat_off_fb|frame_fwd|frame_rev|scan_speed|source_tv1|source_video1|source_video2|source_video3|source_tape1|source_tape2|source_cd1|source_tuner1|source_phono1|source_aux1|source_cycle|disc_next|disc_prev|disc_tray|disc_random|disc_repeat|disc_random_disc_on|disc_random_disc_fb|disc_random_all_on|disc_random_all_fb|disc_random_off_on|disc_random_off_fb|disc_repeat_disc_on|disc_repeat_disc_fb|disc_repeat_track_on|disc_repeat_track_fb|disc_repeat_all_on|disc_repeat_all_fb|disc_repeat_off_on|disc_repeat_off_fb|doccam_light|doccam_lower_light_on|doccam_lower_light_fb|doccam_upper_light_on|doccam_upper_light_fb|aspect_ratio|bright_up|bright_dn|color_up|color_dn|contrast_up|contrast_dn|sharp_up|sharp_dn|tint_up|tint_dn|pic_mute|pic_mute_on|pic_mute_fb|pic_freeze|pic_freeze_on|pic_freeze_fb|pip_pos|pip_swap|pip|pip_on|pip_fb|balance_up|balance_dn|bass_up|bass_dn|treble_up|treble_dn|loudness|loudness_on|loudness_fb|hvac_cool_up|hvac_cool_dn|hvac_heat_up|hvac_heat_dn|hvac_humidify_up|hvac_humidify_dn|hvac_dehumidify_up|hvac_dehumidify_dn|hvac_hold_on|hvac_hold_fb|hvac_lock_on|hvac_lock_fb|hvac_fan|hvac_fan_on|hvac_fan_on_fb|hvac_fan_auto|hvac_fan_auto_fb|hvac_fan_status_fb|hvac_humidify_state|hvac_state|hvac_auto|hvac_auto_fb|hvac_cool|hvac_cool_fb|hvac_heat|hvac_heat_fb|hvac_off|hvac_off_fb|hvac_eheat|hvac_eheat_fb|hvac_cooling_fb|hvac_heating_fb|hvac_cooling2_fb|hvac_eheating_fb|hvac_humidify_auto|hvac_humidify_auto_fb|hvac_dehumidify|hvac_dehumidify_fb|hvac_humidify|hvac_humidify_fb|hvac_humidify_off|hvac_humidify_off_fb|hvac_dehumidifing_fb|hvac_humidifing_fb|hvac_cool_lvl|hvac_heat_lvl|indoor_temp_lvl|outdoor_temp_lvl|indoor_humid_lvl|outdoor_humid_lvl|hvac_humidify_lvl|hvac_dehumidify_lvl|bright_lvl|color_lvl|contrast_lvl|sharp_lvl|tint_lvl|balance_lvl|bass_lvl|treble_lvl|surround_next|surround_prev|cable_ab|cable_b_on|cable_b_fb|lamp_warming_fb|lamp_cooling_fb|lamp_power_fb|motor_stop|motor_stop_fb|motor_open|motor_open_fb|motor_close|motor_close_fb|motor_preset|motor_pos_lvl|keypad_btn1|keypad_btn1_fb|keypad_btn2|keypad_btn2_fb|keypad_btn3|keypad_btn3_fb|keypad_btn4|keypad_btn4_fb|keypad_btn5|keypad_btn5_fb|keypad_btn6|keypad_btn6_fb|keypad_btn7|keypad_btn7_fb|keypad_btn8|keypad_btn8_fb|keypad_btn9|keypad_btn9_fb|keypad_btn10|keypad_btn10_fb|keypad_btn1_blink|keypad_btn1_blink_fb|keypad_btn2_blink|keypad_btn2_blink_fb|keypad_btn3_blink|keypad_btn3_blink_fb|keypad_btn4_blink|keypad_btn4_blink_fb|keypad_btn5_blink|keypad_btn5_blink_fb|keypad_btn6_blink|keypad_btn6_blink_fb|keypad_btn7_blink|keypad_btn7_blink_fb|keypad_btn8_blink|keypad_btn8_blink_fb|keypad_btn9_blink|keypad_btn9_blink_fb|keypad_btn10_blink|keypad_btn10_blink_fb|pool_heat|spa_heat|spa_jets|pool_heat_up|pool_heat_dn|spa_heat_up|spa_heat_dn|pool_pump_on|pool_pump_fb|spa_pump_on|spa_pump_fb|pool_light_on|pool_light_fb|spa_light_on|spa_light_fb|pool_heat_off|pool_heat_off_fb|pool_heater|pool_heater_fb|pool_solar|pool_solar_fb|pool_solar_pref|pool_solar_pref_fb|spa_heat_off|spa_heat_off_fb|spa_heater|spa_heater_fb|spa_solar|spa_solar_fb|spa_solar_pref|spa_solar_pref_fb|spa_jets_off|spa_jets_off_fb|spa_jets_lo|spa_jets_lo_fb|spa_jets_med|spa_jets_med_fb|spa_jets_hi|spa_jets_hi_fb|spa_blower_on|spa_blower_fb|pool_heating|pool_heating_solar|spa_heating|spa_heating_solar|pool_heat_lvl|spa_heat_lvl|pool_temp_lvl|spa_temp_lvl|vconf_privacy|vconf_privacy_on|vconf_privacy_fb|vconf_train|slide_next|slide_prev|multiwin_preset|pan_up|pan_dn|sensor_fb|sensor_value|vproc_preset|weather_force_reading|weather_raining|weather_freezing|weather_bar_rising|weather_bar_falling|weather_hi_temp_lvl|weather_lo_temp_lvl|weather_wind_chill_lvl|weather_heat_index_lvl|weather_dewpoint_lvl|weather_bar_lvl|menu_audio_menu|menu_advanced|hvac_hold|hvac_lock|duet_max_cmd_len|duet_max_param_len|duet_max_hdr_len|duet_ramping_repeat)$"))

;; ============================================================================
;; G4API HIGHLIGHTS
;; ============================================================================
((identifier) @constant.builtin
  (#match? @constant.builtin "(?i)^(g4api_axi_version|btn_power|btn_vol_up|btn_vol_down|btn_vol_mute|lvl_vol|btn_play|btn_stop|btn_pause|btn_ffwd|btn_rew|btn_sfwd|btn_srev|btn_record|btn_eject|btn_slow_fwd|btn_slow_rev|btn_reset_counter|btn_frame_fwd|btn_frame_rev|btn_scan_speed|btn_search_speed|btn_digit_0|btn_digit_1|btn_digit_2|btn_digit_3|btn_digit_4|btn_digit_5|btn_digit_6|btn_digit_7|btn_digit_8|btn_digit_9|btn_menu_plus_10|btn_menu_plus_100|btn_menu_plus_1000|btn_chan_up|btn_chan_dn|btn_menu_enter|btn_menu_cancel|btn_menu_func|btn_menu_up|btn_menu_dn|btn_menu_lt|btn_menu_rt|btn_menu_select|btn_menu_exit|btn_menu_clear|btn_menu_return|btn_menu_help|btn_menu_display|btn_menu_info|btn_menu_guide|btn_menu_page_up|btn_menu_page_dn|btn_menu_favorites|btn_menu_audio|btn_menu_subtitle|btn_menu_title|btn_menu_top_menu|btn_menu_angle|btn_menu_back|btn_menu_setup|btn_menu_continue|btn_menu_reset|btn_menu_accept|btn_menu_reject|btn_menu_zoom|btn_menu_sleep|btn_menu_video|btn_menu_live_tv|btn_menu_advance|btn_menu_list|btn_menu_dot|btn_menu_pound|btn_menu_asterisk|btn_menu_dash|btn_menu_underscore|btn_menu_lt_paren|btn_menu_rt_paren|btn_menu_comma|btn_menu_hold|btn_menu_dial|btn_menu_flash|btn_menu_conference|btn_menu_ab_repeat|btn_menu_thumbs_up|btn_menu_thumbs_dn|btn_menu_instant_replay|btn_menu_record_speed|btn_menu_program|btn_menu_tv_vcr|btn_menu_xm|btn_menu_fm|btn_menu_am|btn_menu_dimmer|btn_menu_ppv|btn_menu_function|btn_source_tv1|btn_source_video1|btn_source_video2|btn_source_video3|btn_source_tape1|btn_source_tape2|btn_source_cd1|btn_source_tuner1|btn_source_phono1|btn_source_aux1|btn_source_cycle|btn_balance_up|btn_balance_dn|btn_bass_up|btn_bass_dn|btn_treble_up|btn_treble_dn|btn_gain_up|btn_gain_dn|btn_gain_mute|btn_loudness|btn_surround_next|btn_surround_prev|btn_aspect_ratio|btn_bright_up|btn_bright_dn|btn_color_up|btn_color_dn|btn_contrast_up|btn_contrast_dn|btn_sharp_up|btn_sharp_dn|btn_tint_up|btn_tint_dn|btn_pic_mute|btn_pic_freeze|btn_pip|btn_pip_pos|btn_pip_swap|btn_tuner_band|btn_tuner_preset_group|btn_tuner_station_up|btn_tuner_station_dn|btn_tuner_scan_fwd|btn_tuner_scan_rev|btn_tuner_seek_fwd|btn_tuner_seek_rev|btn_tuner_prev|btn_cass_tape_side|btn_tilt_up|btn_tilt_dn|btn_pan_up|btn_pan_dn|btn_pan_lt|btn_pan_rt|btn_zoom_in|btn_zoom_out|btn_focus_near|btn_focus_far|btn_auto_focus|btn_auto_iris|btn_iris_open|btn_iris_close|btn_vproj_preset_save|btn_cam_preset_save|btn_vol_preset_save|btn_swt_take|btn_swt_preset_save|btn_mixer_preset_save|btn_aproc_preset_save|btn_vproc_preset_save|btn_multiwin_preset_save|btn_vwall_config_save|btn_lamp_warming_fb|btn_lamp_cooling_fb|btn_lamp_power_fb|btn_sec_arm|btn_sec_arm_home|btn_sec_disarm|btn_sec_panic|btn_sec_police|btn_sec_fire|btn_sec_medical|btn_sec_arm_now|btn_sec_arm_home_now|btn_sec_pt_active|btn_sec_pt_bypass|btn_hvac_cool_up|btn_hvac_cool_dn|btn_hvac_heat_up|btn_hvac_heat_dn|btn_hvac_humidify_up|btn_hvac_humidify_dn|btn_hvac_dehumidify_up|btn_hvac_dehumidify_dn|btn_hvac_units|btn_hvac_fan|btn_hvac_fan_status|btn_hvac_humidify_state|btn_hvac_state|btn_pool_heat|btn_spa_heat|btn_spa_jets|btn_pool_heat_up|btn_pool_heat_dn|btn_spa_heat_up|btn_spa_heat_dn|btn_pool_pump_on|btn_spa_pump_on|btn_pool_light_on|btn_spa_light_on|btn_spa_blower_on|btn_pool_units|btn_window_front|btn_window_back|btn_window_forward|btn_window_backward|btn_vwall_config_next|btn_vwall_config_prev|btn_motor_stop|btn_motor_open|btn_motor_close|btn_aconf_privacy|btn_aconf_train|btn_dial_redial|btn_dial_off_hook|btn_dial_auto_answer|btn_dial_audible_ring|btn_phonebook_next|btn_phonebook_prev|btn_phonebook_dial|btn_vconf_privacy|btn_vconf_train|btn_mixer_xpoint_mute|btn_mixer_xpoint_up|btn_mixer_xpoint_dn|btn_aproc_xpoint_up|btn_aproc_xpoint_dn|btn_aproc_xpoint_mute|btn_swt_level_all|btn_swt_level_video|btn_swt_level_audio|btn_cable_ab|btn_cable_pc|btn_cable_timer|btn_cable_clock|btn_cable_alt|btn_cable_ippv|btn_cable_credit|btn_cable_scan|btn_cable_ce|btn_cable_buy|btn_cable_dcr|btn_disc_next|btn_disc_prev|btn_disc_chapter|btn_disc_dnr|btn_disc_memory|btn_disc_mode|btn_disc_play_mode|btn_disc_search_mode|btn_disc_time|btn_disc_tray|btn_disc_random|btn_disc_repeat|btn_slide_next|btn_slide_prev|btn_doccam_light|btn_weather_force_reading|btn_weather_units|btn_mediadb_next|btn_mediadb_prev|btn_mediadb_select|btn_mediadb_back|btn_mediadb_play|btn_mediadb_search|btn_mediadb_search_all|btn_mediadb_search_artist|btn_mediadb_search_genre|btn_mediadb_search_title|btn_mediadb_search_keywords|btn_mediadb_search_bookmark|btn_mediadb_search_text|btn_mediadb_return_all|btn_mediadb_return_picture|btn_mediadb_return_application|btn_mediadb_return_track|btn_mediadb_return_chapter|btn_mediadb_return_playlist|btn_mediadb_return_bookmark|btn_mediadb_return_disc|btn_mediadb_return_audio|btn_mediadb_return_video|btn_mediadb_return_genre|btn_mediadb_return_artist|btn_mediadb_return_station|btn_ups_outlet_state_1|btn_ups_outlet_state_2|btn_ups_outlet_state_3|btn_ups_outlet_state_4|btn_ups_outlet_state_5|btn_ups_outlet_state_6|btn_light_ramp_up|btn_light_ramp_dn|btn_light_element_1|btn_light_element_2|btn_light_element_3|btn_light_element_4|btn_light_element_5|btn_light_element_6|btn_light_element_7|btn_light_element_8|btn_light_element_9|btn_light_element_10|btn_light_element_11|btn_light_element_12|btn_light_element_13|btn_light_element_14|btn_light_element_15|btn_light_element_16|btn_light_element_17|btn_light_element_18|btn_light_element_19|btn_light_element_20|btn_lightsys_ramp_up|btn_lightsys_ramp_dn|btn_lightsys_element_1|btn_lightsys_element_2|btn_lightsys_element_3|btn_lightsys_element_4|btn_lightsys_element_5|btn_lightsys_element_6|btn_lightsys_element_7|btn_lightsys_element_8|btn_lightsys_element_9|btn_lightsys_element_10|btn_lightsys_element_11|btn_lightsys_element_12|btn_lightsys_element_13|btn_lightsys_element_14|btn_lightsys_element_15|btn_lightsys_element_16|btn_lightsys_element_17|btn_lightsys_element_18|btn_lightsys_element_19|btn_lightsys_element_20|lvl_balance|lvl_bass|lvl_treble|lvl_gain|lvl_bright|lvl_color|lvl_contrast|lvl_sharp|lvl_tint|lvl_light_level_x|lvl_light_level_y|lvl_lightsys_level_x|lvl_lightsys_level_y|lvl_mixer_xpoint_set|lvl_aproc_xpoint_set|txt_source_input|txt_tuner_station|txt_tuner_band|txt_aspect_ratio|txt_lamp_hours|txt_lamp_cooldown|txt_lamp_warmup|txt_media_counter|txt_tape_counter|txt_disc_counter|txt_disc_track_counter|txt_current_media_info|txt_dial_number|txt_dial_call_progress|txt_phonebook_name|txt_phonebook_number|txt_phonebook_list_status|txt_mediadb_search_cur_item|txt_mediadb_search_text|txt_mediadb_search|txt_mediadb_return|txt_mediadb_list_status|txt_sec_status|txt_sec_password|txt_hvac_state|txt_hvac_status|txt_hvac_humid_state|txt_hvac_humid_status|txt_hvac_cool|txt_hvac_heat|txt_hvac_temp|txt_hvac_outdoor_temp|txt_hvac_humid|txt_hvac_outdoor_humid|txt_hvac_humidify|txt_hvac_dehumidify|txt_indoor_temp|txt_outdoor_temp|txt_indoor_humid|txt_outdoor_humid|txt_pool_setpoint|txt_spa_setpoint|txt_pool_temp|txt_spa_temp|txt_pool_heater|txt_spa_heater|txt_spa_jets|txt_pool_heating|txt_spa_heating|txt_ups_status|txt_ups_backup_time|txt_ups_alarm|btn_menu_preview_input|btn_menu_send_input|btn_menu_send_graphics|txt_weather_hi_temp|txt_weather_lo_temp|txt_weather_wind_chill|txt_weather_heat_index|txt_weather_dewpoint|txt_weather_bar_press|txt_weather_bar_trend|txt_weather_condition|txt_weather_rain_today|txt_weather_rain_week|txt_weather_rain_month|txt_weather_rain_year|txt_weather_alert|txt_weather_wind_info|txt_weather_forecast_condition|txt_weather_forecast_hi|txt_weather_forecast_lo|txt_weather_forecast_cop|btn_vwall_config|txt_vwall_config|btn_tuner_preset|btn_source_input|btn_preamp_surround|btn_light_preset|btn_dial_flash_hook|txt_disc_number|txt_disc_duration|txt_disc_titles|txt_disc_tracks|txt_disc_type|txt_disc_title_number|txt_disc_title_duration|txt_disc_title_tracks|btn_doccam_lower_light_on|btn_doccam_upper_light_on|btn_hvac_hold_on|btn_hvac_lock_on|btn_mixer_take|btn_loudness_on|txt_weather_wind_speed|txt_weather_wind_dir|btn_vproj_preset|btn_input_source|btn_input_source_group|btn_disc_list|btn_dial_list|btn_phonebook_list|btn_cam_preset|btn_mixer_preset|btn_mixer_input|btn_mixer_output|btn_aproc_preset|btn_aproc_input|btn_aproc_output|btn_swt_preset|btn_swt_input|btn_swt_output|btn_mediadb_properties|btn_mediadb_list|btn_vol_preset|btn_multiwin_preset|btn_window_input|btn_light_element|btn_lightsys_element|btn_pool_aux|btn_ups_outlet_state|btn_io_state|btn_relay_state|btn_keypad_button|txt_disc_info|txt_title_info|txt_track_info|txt_track_properties|txt_track_values|txt_disc_properties|txt_disc_values|txt_disc_title_properties|txt_disc_title_values|txt_decode_properties|txt_decode_values|txt_encode_properties|txt_encode_values|txt_input_source|txt_input_source_group|txt_phonebook_list|txt_mediadb_properties|txt_mediadb_values|txt_mediadb_list|txt_light_element_name|txt_lightsys_element_name|btn_media_random|btn_media_repeat)$"))

;; ============================================================================
;; UNICODELIB HIGHLIGHTS
;; ============================================================================
((call_expression
  function: (identifier) @function.builtin
  (#match? @function.builtin "(?i)^(wc_encode|wc_decode|_wc|wc_to_ch|ch_to_wc|wc_find_string|wc_left_string|wc_length_string|wc_lower_string|wc_max_length_string|wc_mid_string|wc_remove_string|wc_right_string|wc_set_length_string|wc_upper_string|wc_compare_string|wc_get_buffer_char|wc_get_buffer_string|wc_concat_string|__wc_explode_file_handle|__wc_compose_file_handle|__wc_get_file_header|__wc_get_file_format|wc_file_open|wc_file_close|wc_file_read|wc_file_read_line|wc_file_write|wc_file_write_line|wc_tp_encode)$")))

((identifier) @constant.builtin
  (#match? @constant.builtin "(?i)^(__unicode_lib_version__|wc_format_ascii|wc_format_unicode|wc_format_unicode_be|wc_format_utf8|wc_format_tp|wc_max_string_size|wc_max_g3_str_length|wc_max_g4_str_length)$"))

((identifier) @variable.builtin
  (#match? @variable.builtin "(?i)^(cwc_upper_lookup|cwc_upper_result|cwc_lower_lookup|cwc_lower_result)$"))
