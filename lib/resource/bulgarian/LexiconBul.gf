--# -path=.:prelude

concrete LexiconBul of Lexicon = CatBul ** 
  open ParadigmsBul, ResBul, Prelude in {

flags 
  optimize=values ;

lin
  airplane_N = mkN007 "�������" ;
  answer_V2S = mkV2S (actionV (mkV187 "���������") (mkV173 "��������")) naP ;
  apartment_N = mkN007 "����������" ;
  apple_N = mkN041 "������" ;
  art_N = mkN054 "��������" ;
  ask_V2Q = mkV2Q (stateV (mkV186 "�����")) noPrep ;
  baby_N = mkN065 "����" ;
  bad_A = mkA076 "���" ;
  bank_N = mkN041 "�����" ;
  beautiful_A = mkA076 "������" ;
  become_VA = mkVA (actionV (mkV186 "������") (mkV152 "�����")) ;
  beer_N = mkN041 "����" ;
  beg_V2V = mkV2V (stateV (mkV173 "����")) noPrep zaP ;
  big_A = mkA081 "�����" ;
  bike_N = mkN061 "������" ;
  bird_N = mkN041 "�����" ;
  black_A = mkA079 "�����" ;
  blue_A = mkA086 "���" ;
  boat_N = mkN007 "�����" ;
  book_N = mkN041 "�����" ;
  boot_N = mkN041 "������" ;
  boss_N = mkN001 "���" ;
  boy_N = mkN065 "�����" ;
  bread_N = mkN001 "����" ;
  break_V2 = dirV2 (actionV (mkV173 "����") (mkV173 "�����")) ;
  broad_A = mkA079 "�������" ;
  brother_N2 = prepN2 (mkN025 "����") naP ;
  brown_A = mkA076 "�����" ;
  butter_N = mkN054 "�����" ;
  buy_V2 = dirV2 (actionV (mkV186 "�������") (mkV173 "����")) ;
  camera_N = mkN041 "������" ;
  cap_N = mkN041 "�����" ;
  car_N = mkN041 "����" ;
  carpet_N = mkN007 "�����" ;
  cat_N = mkN041 "�����" ;
  ceiling_N = mkN007 "�����" ;
  chair_N = mkN001 "����" ;
  cheese_N = mkN066 "������" ;
  child_N = mkN067 "����" ;
  church_N = mkN041 "������" ;
  city_N = mkN001 "����" ;
  clean_A = mkA076 "����" ;
  clever_A = mkA079 "����" ;
  close_V2 = dirV2 (actionV (mkV187 "��������") (mkV173 "�������")) ;
  coat_N = mkN054 "�����" ;
  cold_A = mkA076 "������" ;
  come_V = actionV (mkV165 "���") (mkV146a "�����") ;
  computer_N = mkN009 "��������" ;
  country_N = mkN041 "�������" ;
  cousin_N = mkN007a "���������" ;
  cow_N = mkN041 "�����" ;
  die_V = actionV (mkV186 "������") (mkV150a "����") ;
  dirty_A = mkA079 "������" ;
  distance_N3 = prepN3 (mkN072 "����������") otP doP ;
  doctor_N = mkN007a "������" ;
  dog_N = mkN065 "����" ;
  door_N = mkN041 "�����" ;
  drink_V2 = dirV2 (stateV (mkV163 "���")) ;
  easy_A2V = mkA2V (mkA079 "�����") zaP ;
  eat_V2 = dirV2 (stateV (mkV169 "��")) ;
  empty_A = mkA079 "������" ;
  enemy_N = mkN001 "����" ;
  factory_N = mkN041 "�������" ;
  father_N2 = prepN2 (mkN038 "����") naP ;
  fear_VS = mkVS (stateV (mkV186 "���������")) ;
  find_V2 = dirV2 (actionV (mkV186 "�������") (mkV173 "������")) ;
  fish_N = mkN041 "����" ;
  floor_N = mkN007 "����" ;
  fridge_N = mkN007 "������" ;
  friend_N = mkN031a "�������" ;
  fruit_N = mkN001 "����" ;
  fun_AV = mkAV (mkA079 "�������") ;
  forget_V2 = dirV2 (actionV (mkV187 "��������") (mkV173 "�������")) ;
  garden_N = mkN041 "�������" ;
  girl_N = mkN065 "������" ;
  glove_N = mkN041 "��������" ;
  gold_N = mkN054 "�����" ;
  good_A = mkA080 "�����" ;
  go_V = actionV (mkV186 "������") (mkV146 "�����") ;
  green_A = mkA076 "�����" ;
  harbour_N = mkN066 "����������" ;
  hate_V2 = dirV2 (stateV (mkV173 "�����")) ;
  hat_N = mkN041 "�����" ;
  have_V2 = dirV2 (stateV (mkV186 "����")) ;
  hear_V2 = dirV2 (actionV (mkV186 "�����") (mkV163 "���")) ;
  hill_N = mkN001 "����" ;
  hope_VS = mkVS (reflV (stateV (mkV186 "�������")) Acc) ;
  horse_N = mkN035 "���" ;
  hot_A = mkA076 "�����" ;
  house_N = mkN041 "����" ;
  important_A = mkA079 "�����" ;
  industry_N = mkN047 "���������" ;
  iron_N = mkN057 "������" ;
  king_N = mkN035a "���" ;
  know_V2 = dirV2 (stateV (mkV162 "����")) ;
  lake_N = mkN054 "�����" ;
  lamp_N = mkN041 "�����" ;
  learn_V2 = dirV2 (stateV (mkV176 "���")) ;
  leather_N = mkN041 "����" ;
  leave_V2 = dirV2 (actionV (mkV187 "�������") (mkV173 "������")) ;
  like_V2 = dirV2 (actionV (mkV186 "��������") (mkV186 "�������")) ;
  listen_V2 = dirV2 (stateV (mkV186 "������")) ;
  live_V = stateV (mkV160 "�����") ;
  long_A = mkA080 "�����" ;
  lose_V2 = dirV2 (actionV (mkV173 "����") (mkV173 "������")) ;
  love_N = mkN049 "�����" ;
  love_V2 = dirV2 (stateV (mkV186 "������")) ;
  man_N = mkN024 "���" ;
  married_A2 = mkA2 (mkA076 "�����") zaP ;
  meat_N = mkN054 "����" ;
  milk_N = mkN057 "�����" ;
  moon_N = mkN041 "����" ;
  mother_N2 = prepN2 (mkN041a "�����") naP ;
  mountain_N = mkN041 "�������" ;
  music_N = mkN041 "������" ;
  narrow_A = mkA084 "�����" ;
  new_A = mkA076 "���" ;
  newspaper_N = mkN014 "�������" ;
  oil_N = mkN065 "����" ;
  old_A = mkA076 "����" ;
  open_V2 = dirV2 (actionV (mkV187 "�������") (mkV173 "������")) ;
  paint_V2A = mkV2A (actionV (mkV186 "�������") (mkV186 "���������")) noPrep ;
  paper_N = mkN047 "������" ;
  paris_PN = mkPN "�����" Masc ;
  peace_N = mkN040a "���" ;
  pen_N = mkN041 "�������" ;
  planet_N = mkN041 "�������" ;
  plastic_N = mkN041 "���������" ;
  play_V2 = dirV2 (stateV (mkV161 "�����")) ;
  policeman_N = mkN032a "�������" ;
  priest_N = mkN014 "��������" ;
  probable_AS = mkAS (mkA079 "��������") ;
  queen_N = mkN041 "�������" ;
  radio_N = mkN054 "�����" ;
  rain_V0 = mkV0 (stateV (mkV174 "����")) ;
  read_V2 = dirV2 (stateV (mkV145 "����")) ;
  red_A = mkA076 "������" ;
  religion_N = mkN047 "�������" ;
  restaurant_N = mkN007 "���������" ;
  river_N = mkN041 "����" ;
  rock_N = mkN041 "�����" ;
  roof_N = mkN007 "������" ;
  rubber_N = mkN041 "����" ;
  run_V = stateV (mkV186 "�����") ;
  say_VS = mkVS (actionV (mkV186 "������") (mkV156 "����")) ;
  school_N = mkN066 "�������" ;
  science_N = mkN041 "�����" ;
  sea_N = mkN065 "����" ;
  seek_V2 = dirV2 (stateV (mkV173 "�����")) ;
  see_V2 = dirV2 (actionV (mkV186 "������") (mkV181 "����")) ;
  sell_V3 = dirV3 (stateV (mkV186 "��������")) naP ;
  send_V3 = dirV3 (actionV (mkV186 "������") (mkV173 "�����")) doP ;
  sheep_N = mkN044 "����" ;
  ship_N = mkN007 "�����" ;
  shirt_N = mkN041 "����" ;
  shoe_N = mkN041 "������" ;
  shop_N = mkN007 "�������" ;
  short_A = mkA076 "���" ;
  silver_N = mkN054 "������" ;
  sister_N = mkN041a "������" ;
  sleep_V = stateV (mkV182 "���") ;
  small_A = mkA080 "�����" ;
  snake_N = mkN047 "����" ;
  sock_N = mkN007 "�����" ;
  speak_V2 = dirV2 (stateV (mkV173 "������")) ;
  star_N = mkN041 "������" ;
  steel_N = mkN041 "�������" ;
  stone_N = mkN017 "�����" ;
  stove_N = mkN041 "�����" ;
  student_N = mkN007a "�������" ;
  stupid_A = mkA076 "������" ;
  sun_N = mkN066 "������" ;
  switch8off_V2 = dirV2 (actionV (mkV186 "���������") (mkV176 "�������")) ;
  switch8on_V2 = dirV2 (actionV (mkV186 "��������") (mkV176 "������")) ;
  table_N = mkN041 "����" ;
  talk_V3 = mkV3 (stateV (mkV173 "������")) naP zaP ;
  teacher_N = mkN031a "������" ;
  teach_V2 = dirV2 (actionV (mkV186 "����������") (mkV168 "��������")) ;
  television_N = mkN047 "���������" ;
  thick_A = mkA076 "�����" ;
  thin_A = mkA080 "�����" ;
  train_N = mkN001 "����" ;
  travel_V = stateV (mkV186 "�������") ;
  tree_N = mkN061 "�����" ;
  ugly_A = mkA076 "������" ;
  understand_V2 = dirV2 (actionV (mkV186 "��������") (mkV170 "�������")) ;
  university_N = mkN007 "�����������" ;
  village_N = mkN054 "����" ;
  wait_V2 = prepV2 (stateV (mkV186 "�����")) zaP ;
  walk_V = stateV (mkV173 "����") ;
  warm_A = mkA080 "�����" ;
  war_N = mkN041 "�����" ;
  watch_V2 = dirV2 (stateV (mkV186 "������")) ;
  water_N = mkN041 "����" ;
  white_A = mkA081 "���" ;
  window_N = mkN008 "��������" ;
  wine_N = mkN054 "����" ;
  win_V2 = dirV2 (actionV (mkV186 "����������") (mkV174 "������")) ;
  woman_N = mkN041a "����" ;
  wonder_VQ = mkVQ (reflV (actionV (mkV186 "�������") (mkV173 "����")) Acc) ;
  wood_N = mkN041 "���������" ;
  write_V2 = dirV2 (stateV (mkV159 "����")) ;
  yellow_A = mkA076 "����" ;
  young_A = mkA076 "����" ;
  do_V2 = dirV2 (stateV (mkV160a "�����")) ;
  now_Adv = mkAdv "����" ;
  already_Adv = mkAdv "����" ;
  song_N = mkN050 "�����" ;
  add_V3 = dirV3 (actionV (mkV186 "�������") (mkV170 "������")) sP ;
  number_N = mkN054 "�����" ;
  put_V2 = prepV2 (actionV (mkV186 "������") (mkV176 "�����")) noPrep ;
  stop_V = actionV (mkV186 "������") (mkV150 "����") ;
  jump_V = actionV (mkV186 "������") (mkV176 "�����") ;
  left_Ord = mkA081 "���" ** {nonEmpty=True} ;
  right_Ord = mkA084 "�����" ** {nonEmpty=True} ;
  far_Adv = mkAdv "������" ;
  correct_A = mkA079 "��������" ;
  dry_A = mkA076 "���" ;
  dull_A = mkA076 "���" ;
  full_A = mkA079 "�����" ;
  heavy_A = mkA080 "�����" ;
  near_A = mkA080 "������" ;
  rotten_A = mkA076 "�������" ;
  round_A = mkA080 "������" ;
  sharp_A = mkA080 "�����" ;
  smooth_A = mkA080 "������" ;
  straight_A = mkA081 "����" ;
  wet_A = mkA080 "�����" ; ----
  wide_A = mkA076 "�����" ;
  animal_N = mkN062 "�������" ;
  ashes_N = mkN049 "���e�" ;
  back_N = mkN003 "����" ;
  bark_N = mkN028 "���" ;
  belly_N = mkN007 "�����" ;
  blood_N = mkN053 "����" ;
  bone_N = mkN049 "����" ;
  breast_N = mkN041 "�����" ;
  cloud_N = mkN014 "�����" ;
  day_N = mkN033 "���" ;
  dust_N = mkN001 "����" ;
  ear_N = mkN064 "���" ;
  earth_N = mkN047 "����" ;
  egg_N = mkN066 "����" ;
  eye_N = mkN063 "���" ;
  fat_N = mkN041 "fat" ;
  feather_N = mkN038 "����" ;
  fingernail_N = mkN034 "�����" ;
  fire_N = mkN030 "����" ;
  flower_N = mkN068 "�����" ;
  fog_N = mkN041 "�����" ;
  foot_N = mkN041 "������" ;
  forest_N = mkN041 "����" ;
  grass_N = mkN041 "�����" ;
  guts_N = mkN054 "�����" ;
  hair_N = mkN041 "����" ;
  hand_N = mkN045 "����" ;
  head_N = mkN041 "�����" ;
  heart_N = mkN066 "�����" ;
  horn_N = mkN001 "���" ;
  husband_N = mkN015 "������" ;  -- personal
  ice_N = mkN001 "���" ;
  knee_N = mkN058 "������" ;
  leaf_N = mkN054 "�����" ;
  leg_N = mkN022 "����" ;
  liver_N = mkN001 "����" ;
  louse_N = mkN041 "�����" ;
  mouth_N = mkN042 "����" ;
  name_N = mkN069 "���" ;
  neck_N = mkN003 "����" ;
  night_N = mkN049 "���" ;
  nose_N = mkN001 "���" ;
  person_N = mkN014 "�����" ;
  rain_N = mkN001 "����" ;
  road_N = mkN037 "���" ;
  root_N = mkN007 "�����" ;
  rope_N = mkN065 "����" ;
  salt_N = mkN049 "���" ;
  sand_N = mkN014 "�����" ;
  seed_N = mkN069 "����" ;
  skin_N = mkN041 "����" ;
  sky_N = mkN070 "����" ;
  smoke_N = mkN014 "�����" ;
  snow_N = mkN002 "����" ;
  stick_N = mkN041 "������" ;
  tail_N = mkN041 "������" ;
  tongue_N = mkN014 "����" ;
  tooth_N = mkN007 "���" ;
  wife_N = mkN041 "�������" ;
  wind_N = mkN004 "�����" ;
  wing_N = mkN056 "�����" ;
  worm_N = mkN032 "������" ;
  year_N = mkN041 "������" ;
  blow_V = stateV (mkV186 "�����") ;
  breathe_V = dirV2 (stateV (mkV186 "�����")) ;
  burn_V = actionV (mkV187 "�������") (mkV177 "������") ;
  dig_V = stateV (mkV161 "�����") ;
  fall_V = actionV (mkV186 "�����") (mkV152 "�����") ;
  float_V = stateV (mkV186 "������") ;
  flow_V = stateV (mkV148 "����") ;
  fly_V = stateV (mkV177 "����") ;
  freeze_V = stateV (mkV186 "���������") ;
  give_V3 = dirV3 (actionV (mkV186 "�����") (mkV186 "���")) naP ;
  laugh_V = reflV (stateV (mkV160 "����")) Acc ;
  lie_V = stateV (mkV178 "����") ;
  play_V = stateV (mkV161 "�����") ;
  sew_V = stateV (mkV163 "���") ;
  sing_V = stateV (mkV164 "���") ;
  sit_V = stateV (mkV177 "����") ;
  smell_V = stateV (mkV159 "������") ;
  spit_V = stateV (mkV163 "����") ;
  stand_V = stateV (mkV180 "����") ;
  swell_V = actionV (mkV186 "�������") (mkV163 "�����") ;
  swim_V = stateV (mkV186 "������") ;
  think_V = stateV (mkV173 "�����") ;
  turn_V = actionV (mkV186 "�������") (mkV152 "������") ;
  vomit_V = actionV (mkV186 "��������") (mkV152 "�������") ;

  bite_V2 = dirV2 (stateV (mkV154 "����")) ;
  count_V2 = dirV2 (stateV (mkV175 "����")) ;
  cut_V2 = dirV2 (stateV (mkV157 "����"))  ;
  fear_V2 = dirV2 (reflV (stateV (mkV186 "���������")) Acc) ;
  fight_V2 = dirV2 (reflV (stateV (mkV173 "����")) Acc) ;
  hit_V2 = dirV2 (actionV (mkV187 "�����") (mkV173 "�����")) ;
  hold_V2 = dirV2 (stateV (mkV179 "�����")) ;
  hunt_V2 = dirV2 (stateV (mkV174 "����")) ;
  kill_V2 = dirV2 (actionV (mkV186 "������") (mkV163 "����")) ;
  pull_V2 = dirV2 (stateV (mkV186 "������")) ;
  push_V2 = dirV2 (stateV (mkV186 "�����")) ;
  rub_V2 = dirV2 (stateV (mkV163 "����")) ;
  scratch_V2 = dirV2 (actionV (mkV186 "�������") (mkV152 "������")) ;
  split_V2 = dirV2 (actionV (mkV187 "��������") (mkV174 "�������")) ;
  squeeze_V2 = dirV2 (actionV (mkV186 "�������") (mkV152 "������")) ;
  stab_V2 = dirV2 (actionV (mkV186 "���������") (mkV176 "�������")) ;
  suck_V2 = dirV2 (stateV (mkV155 "����")) ;
  throw_V2 = dirV2 (actionV (mkV187 "�������") (mkV173 "������")) ;
  tie_V2 = dirV2 (actionV (mkV186 "�������") (mkV156 "�����")) ;
  wash_V2 = dirV2 (stateV (mkV163 "���")) ;
  wipe_V2 = dirV2 (stateV (mkV159 "�����")) ;

  grammar_N = mkN041 "���������" ;
  language_N = mkN014 "����" ;
  rule_N = mkN054 "�������" ;
  
  john_PN = mkPN "����" Masc ;
  question_N = mkN007 "������" ;
  ready_A = mkA076 "�����" ;
  reason_N = mkN041 "�������" ;
  today_Adv = mkAdv "����" ;
  uncertain_A = mkA079 "������" ;
  
oper
  zaP = mkPrep "��" Acc ;
  naP = mkPrep [] Dat ;
  otP = mkPrep "��" Acc ;
  doP = mkPrep "��" Acc ;
  sP  = mkPrep (pre { "�" ; 
                      "���" / strs {"�" ; "�" ; "�" ; "�"}
                    })  Acc ;
} ;
