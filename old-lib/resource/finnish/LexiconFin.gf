--# -path=.:prelude

concrete LexiconFin of Lexicon = CatFin ** open MorphoFin, ParadigmsFin in {

flags 
  optimize=values ;


lin
  airplane_N = mkN "lentokone" ;
  answer_V2S = mkV2 (mkV "vastata") (casePrep allative) ;
  apartment_N = mkN "asunto" ;
  apple_N = mkN "omena" ; --- omenia, not omenoita
  art_N = mkN "taide" ;
  ask_V2Q = mkV2 (mkV "kysy�") (casePrep ablative) ;
  baby_N = mkN "vauva" ;
  bad_A = mkA (mkN "paha") "pahempi" "pahin" ;
  bank_N = mkN "pankki" ;
  beautiful_A = mkA (mkN "kaunis") "kauniimpi" "kaunein" ;
  become_VA = mkVA (mkV "tulla") (casePrep translative) ;
  beer_N = mkN "olut" "oluita" ;
  beg_V2V = mkV2V (mk2V "pyyt��" "pyysi") (casePrep partitive) ;
  big_A = mkA (mkN "suuri" "suuria") "suurempi" "suurin" ;
  bike_N = mkN "polkupy�r�" ; --- for correct vowel harmony
  bird_N = mkN "lintu" ;
  black_A = mkA (mkN "musta") "mustempi" "mustin" ;
  blue_A = mkA (mkN "sininen") "sinisempi" "sinisin" ;
  boat_N = mkN "vene" ;
  book_N = mkN "kirja" ;
  boot_N = mkN "saapas" ;
  boss_N = mkN "pomo" ;
  boy_N = mkN "poika" "pojan" "poikia" ;
  bread_N = mkN "leip�" ;
  break_V2 = mkV2 (mkV "rikkoa") ;
  broad_A = mkA (mkN "leve�") "leve�mpi" "levein" ;
  brother_N2 = mkN2 (
    mkN "veli" "veljen" "veljen�" "velje�" "veljeen" 
        "veljin�" "veljiss�" "veljien" "velji�" "veljiin") ;
  brown_A = mkA (mkN "ruskea") "ruskeampi" "ruskein" ;
  butter_N = mk3N "voi" "voin" "voita" ;  ---- errors in Part
  buy_V2 = mkV2 (mkV "ostaa") ;
  camera_N = mkN "kamera" ;
  cap_N = mkN "lakki" ;
  car_N = mkN "auto" "auton" "autoja" ; -- mkN: audon
  carpet_N = mkN "matto" ;
  cat_N = mkN "kissa" ;
  ceiling_N = mkN "katto" ;
  chair_N = mkN "tuoli" ;
  cheese_N = mkN "juusto" ;
  child_N = mkN "lapsi" "lapsen" "lasta" "lapsena" "lapseen" 
              "lasten" "lapsia" "lapsina" "lapsissa" "lapsiin" ;
  church_N = mkN "kirkko" ;
  city_N = mkN "kaupunki" ;
  clean_A = mkA (mkN "puhdas") ;
  clever_A = mkA (mkN "viisas") ;
  close_V2 = mkV2 (mkV "sulkea") ;
  coat_N = mkN "takki" ;
  cold_A = mkA (mkN "kylm�") "kylmempi" "kylmin" ;
  come_V = mkV "tulla" ;
  computer_N = mkN "tietokone" ;
  country_N = mkN "maa" ;
  cousin_N = mkN "serkku" ;
  cow_N = mkN "lehm�" ;
  die_V = mkV "kuolla" ;
  dirty_A = mkA (mkN "likainen") "likaisempi" "likaisin" ;
  distance_N3 = mkN3 (mkN "et�isyys") (casePrep elative) (casePrep illative) ;
  doctor_N = mk2N "tohtori" "tohtoreita" ;
  dog_N = mkN "koira" ;
  door_N = mkN "ovi" "ovia" ;
  drink_V2 = mkV2 (mkV "juoda") ;
  easy_A2V = mkA2 (mkA (mkN "helppo") "helpompi" "helpoin") 
    (casePrep allative) ;
  eat_V2 = mkV2 (mkV "sy�d�") ;
  empty_A = mkA (mkN "tyhj�") "tyhjempi" "tyhjin" ;
  enemy_N = mkN "vihollinen" ;
  factory_N = mkN "tehdas" ;
  father_N2 = mkN2 (mkN "is�") ;
  fear_VS = mkVS (mk2V "pel�t�" "pelk�si") ;
  find_V2 = mkV2 (mk2V "l�yt��" "l�ysi") ;
  fish_N = mkN "kala" ;
  floor_N = mk2N "lattia" "lattioita" ;
  forget_V2 = mkV2 (mkV "unohtaa") ;
  fridge_N = mkN "j��kaappi" ;
  friend_N = mkN "yst�v�" ;
  fruit_N = mkN "hedelm�" ;
  fun_AV = mkAV (mkA (mkN "hauska") "hauskempi" "hauskin") ;
  garden_N = mkN "puutarha" "puutarhan" "puutarhoja" ;
  girl_N = mkN "tytt�" ;
  glove_N = mkN "k�sine" ;
  gold_N = mkN "kulta" ;
  good_A = mkA (mkN "hyv�") "parempi" "parhain" ; --- paras
  go_V = mkV "menn�" ;
  green_A = mkA (mkN "vihre�") "vihre�mpi" "vihrein" ;
  harbour_N = mkN "satama" "sataman" "satamia" ;
  hate_V2 = mkV2 (mkV "vihata") cpartitive ;
  hat_N = mkN "hattu" ;
  have_V2 = mkV2 (caseV adessive vOlla) ;
  hear_V2 = mkV2 (mkV "kuulla") ;
  hill_N = mkN "kukkula" ;
  hope_VS = mkVS (mkV "toivoa") ;
  horse_N = mkN "hevonen" ;
  hot_A = mkA (mkN "kuuma") "kuumempi" "kuumin" ;
  house_N = mkN "talo" ;
  important_A = mkA (mkN "t�rke�") "t�rke�mpi" "t�rkein" ;
  industry_N = mkN "teollisuus" ;
  iron_N = mkN "rauta" ;
  king_N = mkN "kuningas" ;
  know_V2 = mkV2 (mk2V "tiet��" "tiesi") ; --- tuntea; gives tiet�nyt
  lake_N = mkN "j�rvi" "j�rvi�" ;
  lamp_N = mkN "lamppu" ;
  learn_V2 = 
    mkV2 (mk12V "oppia" "opin" "oppii" "oppivat" "oppikaa" "opitaan"
      "opin" "oppi" "oppisi" "oppinut" "opittu" "opitun") ;
  leather_N = mkN "nahka" ; --- nahan
  leave_V2 = mkV2 (mkV "j�tt��") ;
  like_V2 = mkV2 (mkV "pit��") elative ;
  listen_V2 = mkV2 (mkV "kuunnella" "kuunteli") partitive ;
  live_V = mkV "el��" ;
  long_A = mkA (mkN "pitk�") "pitempi" "pisin" ;
  lose_V2 = mkV2 (mkV "h�vit�" "h�visi") ; --- hukata
  love_N = mk3N "rakkaus" "rakkauden" "rakkauksia" ;
  love_V2 = mkV2 (mkV "rakastaa") partitive ;
  man_N = mkN "mies" "miehen" "miest�" "miehen�" "mieheen" 
              "miesten" "miehi�" "miehin�" "miehiss�" "miehiin" ; 
  married_A2 = mkA2 (mkA "avioitunut") (postPrep genitive "kanssa") ; ---- infl
  meat_N = mkN "liha" ;
  milk_N = mkN "maito" ;
  moon_N = mkN "kuu" ;
  mother_N2 = mkN2 (mkN "�iti") ;
  mountain_N = mkN "vuori" "vuoria" ;
  music_N = mkN "musiikki" ;
  narrow_A = mkA (mkN "kapea") "kapeampi" "kapein" ;
  new_A = mkA (mk3N "uusi" "uuden" "uusia") "uudempi" "uusin" ;
  newspaper_N = mkN "sanoma" (mkN "lehti" "lehti�") ; --- for correct vowel harmony
  oil_N = mkN "�ljy" ;
  old_A = mkA (mkN "vanha") "vanhempi" "vanhin" ;
  open_V2 = mkV2 (mkV "avata" "avasi") ;
  paint_V2A = mkV2A (mkV "maalata") accPrep (casePrep translative) ;
  paper_N = mk2N "paperi" "papereita" ;
  paris_PN = mkPN (mkN "Pariisi") ;
  peace_N = mkN "rauha" ;
  pen_N = mkN "kyn�" ;
  planet_N = mkN "planeetta" ;
  plastic_N = mkN "muovi" ;
  play_V2 = mkV2 (mkV "pelata") cpartitive ; --- leikki�, soittaa
  policeman_N = mkN "poliisi" ;
  priest_N = mkN "pappi" ;
  probable_AS = mkAS --- for vowel harmony
    (mkA (mkN "todenn�k�inen") "tonenn�k�isempi" "todenn�l�isin") ; ---- sta
  queen_N = mkN "kuningatar" ;
  radio_N = mk2N "radio" "radioita" ;
  rain_V0 = mkV0 (mk2V "sataa" "satoi") ;
  read_V2 = mkV2 (mkV "lukea") ;
  red_A = mkA "punainen" ;
  religion_N = mkN "uskonto" ;
  restaurant_N = mkN "ravintola" ;
  river_N = mkN "joki" "jokia" ;
  rock_N = mk2N "kallio" "kallioita" ;
  roof_N = mkN "katto" ;
  rubber_N = mkN "kumi" ;
  run_V = mk2V "juosta" "juoksi" ;
  say_VS = mkVS (mkV "sanoa") ;
  school_N = mkN "koulu" ;
  science_N = mkN "tiede" ;
  sea_N = mkN "meri" "meren" "meri�" "merta" ;
  seek_V2 = mkV2 (mkV "etsi�") cpartitive ;
  see_V2 = mkV2 (
    mk12V "n�hd�" "n�en" "n�kee" "n�kev�t" "n�hk��" "n�hd��n"
      "n�in" "n�ki" "n�kisi" "n�hnyt" "n�hty" "n�hdyn") ; 
  sell_V3 = mkV3 (mkV "myyd�") accPrep (casePrep allative) ;
  send_V3 = mkV3 (mkV "l�hett��") accPrep (casePrep allative) ;
  sheep_N = mkN "lammas" ;
  ship_N = mkN "laiva" ;
  shirt_N = mkN "paita" ;
  shoe_N = mkN "kenk�" ;
  shop_N = mkN "kauppa" ;
  short_A = mkA (mkN "lyhyt" "lyhyit�") ;
  silver_N = mkN "hopea" ;
  sister_N = mkN "sisko" ;
  sleep_V = mkV "nukkua" ;
  small_A = mkA (mk2N "pieni" "pieni�") "pienempi" "pienin" ;
  snake_N = mkN "k��rme" ;
  sock_N = mkN "sukka" ;
  speak_V2 = mkV2 (mkV "puhua") cpartitive ;
  star_N = mkN "t�hti" "t�hti�" ;
  steel_N = mkN "ter�s" ;
  stone_N = mkN "kivi" "kivi�" ;
  stove_N = mk3N "liesi" "lieden" "liesi�" ;
  student_N = mk2N "opiskelija" "opiskelijoita" ;
  stupid_A = mkA "tyhm�" ;
  sun_N = mkN "aurinko" ;
  switch8off_V2 = mkV2 (mkV "sammuttaa") ; ---
  switch8on_V2 = mkV2 (mkV "sytytt��") ; ---
  table_N = mkN "p�yt�" ;
  talk_V3 = mkV3 (mkV "puhua") (casePrep allative) (casePrep elative) ;
  teacher_N = mkN "opettaja" ;
  teach_V2 = mkV2 (mkV "opettaa") ;
  television_N = mk2N "televisio" "televisioita" ;
  thick_A = mkA "paksu" ;
  thin_A = mkA (mkN "ohut" "ohuita") ;
  train_N = mkN "juna" ;
  travel_V = mkV "matkustaa" ;
  tree_N = mkN "puu" ;
 ---- trousers_N = mkN "trousers" ;
  ugly_A = mkA (mkN "ruma") "rumempi" "rumin" ;
  understand_V2 = mkV2 (mkV "ymm�rt��" "ymm�rr�n" "ymm�rsi") ;
  university_N = mkN "yliopisto" ;
  village_N = mkN "kyl�" ;
  wait_V2 = mkV2 (mkV "odottaa") partitive ;
  walk_V = mkV "k�vell�" "k�veli" ;
  warm_A = mkA 
    (mkN "l�mmin" "l�mpim�n" "l�mpim�n�" "l�mmint�" "l�mpim��n" 
         "l�mpimin�" "l�mpimiss�" "l�mpimien" "l�mpimi�" "l�mpimiin"
	 ) 
    "l�mpim�mpi" "l�mpimin" ;
  war_N = mkN "sota" ;
  watch_V2 = mkV2 (mkV "katsella") cpartitive ;
  water_N = mk3N "vesi" "veden" "vesi�" ;
  white_A = mkA "valkoinen" ;
  window_N = mk2N "ikkuna" "ikkunoita" ;
  wine_N = mkN "viini" ;
  win_V2 = mkV2 (mkV "voittaa") ;
  woman_N = mkN "nainen" ;
  wonder_VQ = mkVQ (mkV "ihmetell�") ;
  wood_N = mkN "puu" ;
  write_V2 = mkV2 (mkV "kirjoittaa") ;
  yellow_A = mkA "keltainen" ;
  young_A = mkA (mkN "nuori" "nuoria") "nuorempi" "nuorin" ;

  do_V2 = mkV2 (
    mkV "tehd�" "teen" "tekee" "tekev�t" "tehk��" "tehd��n"
      "tein" "teki" "tekisi" "tehnyt" "tehty" "tehdyn") ; 

  now_Adv = mkAdv "nyt" ;
  already_Adv = mkAdv "jo" ;
  song_N = mkN "laulu" ;
  add_V3 = mkV3 (mkV "lis�t�") accPrep (casePrep illative) ;
  number_N = mk2N "numero" "numeroita" ;
  put_V2 = mkV2 (mkV "panna") ;
  stop_V = mkV "pys�hty�" ;
  jump_V = mkV "hyp�t�" ;
  left_Ord = mkOrd (mkN "vasen") ;
  right_Ord = mkOrd (mkN "oikea") ;
  far_Adv = mkAdv "kaukana" ;
  correct_A = mkA "oikea" ;
  dry_A = mkA (mkN "kuiva") "kuivempi" "kuivin" ;
  dull_A = mkA (mkN "tyls�") "tylsempi" "tylsin" ;
  full_A = mkA (mk3N "t�ysi" "t�yden" "t�ysi�") "t�ydempi" "t�ysin" ;
  heavy_A = mkA "raskas" ;
  near_A = mkA (mkN "l�heinen") ;
  rotten_A = mkA "m�t�" ;
  round_A = mkA "py�re�" ;
  sharp_A = mkA "ter�v�" ;
  smooth_A = mkA "sile�" ;
  straight_A = mkA (mkN "suora") "suorempi" "suorin" ;
  wet_A = mkA (mkN "m�rk�") "m�rempi" "m�rin" ;
  wide_A = mkA "leve�" ;
  animal_N = mk3N "el�in" "el�imen" "el�imi�" ;
  ashes_N = mkN "tuhka" ;
  back_N = mkN "selk�" ;
  bark_N = mkN "kaarna" ;
  belly_N = mkN "vatsa" ;
  blood_N = mkN "veri" "veren" "veri�" "verta" ;
  bone_N = mkN "luu" ;
  breast_N = mkN "rinta" ;
  cloud_N = mk2N "pilvi" "pilvi�" ;
  day_N = mkN "p�iv�" ;
  dust_N = mkN "p�ly" ;
  ear_N = mkN "korva" ;
  earth_N = mkN "maa" ;
  egg_N = mkN "muna" ;
  eye_N = mkN "silm�" ;
  fat_N = mkN "rasva" ;
  feather_N = mk3N "h�yhen" "h�yhenen" "h�yheni�" ;
  fingernail_N = mk3N "kynsi" "kynnen" "kynsi�" ;
  fire_N = mk2N "tuli" "tulia" ;
  flower_N = mkN "kukka" ;
  fog_N = mkN "sumu" ;
  foot_N = mkN "jalka" ;
  forest_N = mkN "mets�" ;
  grass_N = mkN "ruoho" ;
  guts_N = mkN "sis�lmys" ; --- suoli
  hair_N = mkN "hius" ;
  hand_N = mk3N "k�si" "k�den" "k�si�" ;
  head_N = mkN "p��" ;
  heart_N = mkN "syd�n" "syd�men" "syd�nt�" "syd�men�" "syd�meen"
                "syd�nten" "syd�mi�" "syd�min�" "syd�miss�" "syd�miin" ;
  horn_N = mk2N "sarvi" "sarvia" ;
  husband_N = mkN "mies" "miehen" "miest�" "miehen�" "mieheen" 
                  "miesten" "miehi�" "miehin�" "miehiss�" "miehiin" ; 
  ice_N = mkN "j��" ;
  knee_N = mk2N "polvi" "polvia" ;
  leaf_N = mk2N "lehti" "lehti�" ;
  leg_N = mkN "jalka" ; --- s��ri
  liver_N = mkN "maksa" ;
  louse_N = mkN "lude" ;
  mouth_N = mkN "suu" ;
  name_N = mk2N "nimi" "nimi�" ;
  neck_N = mkN "niska" ;
  night_N = mkN "y�" ;
  nose_N = mkN "nen�" ;
  person_N = mkN "henkil�" ;
  rain_N = mkN "sade" ;
  road_N = mkN "tie" ;
  root_N = mk2N "juuri" "juuria" ;
  rope_N = mk3N "k�ysi" "k�yden" "k�ysi�" ;
  salt_N = mkN "suola" ;
  sand_N = mkN "hiekka" ;
  seed_N = mkN "siemen" ;
  skin_N = mkN "nahka" ;
  sky_N = mk3N "taivas" "taivaan" "taivaita" ;
  smoke_N = mkN "savu" ;
  snow_N = mkN "lumi" "lumen" "lumia" "lunta" ;
  stick_N = mkN "keppi" ;
  tail_N = mkN "h�nt�" ;
  tongue_N = mk2N "kieli" "kieli�" ;
  tooth_N = mkN "hammas" ;
  wife_N = mkN "vaimo" ;
  wind_N = mk2N "tuuli" "tuulia" ;
  wing_N = mk2N "siipi" "siipi�" ;
  worm_N = mkN "mato" ;
  year_N = mk3N "vuosi" "vuoden" "vuosia" ;
  bite_V2 = mkV2 (mkV "purra") ;
  blow_V = mkV "puhaltaa" ;
  burn_V = mkV "palaa" ;
  count_V2 = mkV2 (mkV "laskea") ;
  cut_V2 = mkV2 (mk2V "leikata" "leikkasi") ;
  dig_V = mkV "kaivaa" ;
  fall_V = mkV "pudota" "putoan" "putosi" ;
  fear_V2 = mkV2 (mkV "pel�t�" "pelk��n" "pelk�si") cpartitive ;
  fight_V2 = mkV2 (mkV "taistella") (postPrep partitive "vastaan") ;
  float_V = mkV "kellua" ;
  flow_V = mkV "virrata" "virtaan" "virtasi" ;
  fly_V = mkV "lent��" ;
  freeze_V = mkV "j��ty�" ;
  give_V3 = mkV3 (mkV "antaa" "annan" "antoi") accPrep (casePrep allative) ;
  hit_V2 = mkV2 (mkV "ly�d�") cpartitive ;
  hold_V2 = mkV2 (mkV "pit��") cpartitive ;
  hunt_V2 = mkV2 (mkV "mets�st��") cpartitive ;
  kill_V2 = mkV2 (mkV "tappaa") ;
  laugh_V = mkV "nauraa" "nauroi" ;
  lie_V = mkV "maata" "makasi" ;
  play_V = mkV "pelata" ;
  pull_V2 = mkV2 (mkV "vet��") ;
  push_V2 = mkV2 (mkV "ty�nt��") ;
  rub_V2 = mkV2 (mkV "hieroa") cpartitive ;
  scratch_V2 = mkV2 (mkV "raapia") cpartitive ;
  sew_V = mkV "kylv��" ;
  sing_V = mkV "laulaa" ;
  sit_V = mkV "istua" ;
  smell_V = mk2V "haistaa" "haistoi" ;
  spit_V = mkV "sylke�" ;
  split_V2 = mkV2 (mk2V "halkaista" "halkaisi") ;
  squeeze_V2 = mkV2 (mkV "puristaa") cpartitive ;
  stab_V2 = mkV2 (mkV "pist��") cpartitive ;
  stand_V = mk12V "seist�" "seison" "seisoo" "seisovat" "seisk��" "seist��n"
      "seisoin" "seisoi" "seisoisi" "seissyt" "seisty" "seistyn" ; --- *seisoiv�t
  suck_V2 = mkV2 (mkV "ime�") cpartitive ;
  swell_V = mkV "turvota" "turposi" ;
  swim_V = mkV "uida" "uin" "ui" ;
  think_V = mkV "ajatella" "ajattelen" "ajatteli" ;
  throw_V2 = mkV2 (mkV "heitt��") ;
  tie_V2 = mkV2 (mkV "sitoa") ;
  turn_V = mkV "k��nty�" ;
  vomit_V = mkV "oksentaa" ;
  wash_V2 = mkV2 (mkV "pest�") ;
  wipe_V2 = mkV2 (mkV "pyyhki�") ;

  breathe_V = mkV "hengitt��" ;

  grammar_N = mkN "kielioppi" ;
  language_N = mk2N "kieli" "kieli�" ;
  rule_N = mkN "s��nt�" ;

    john_PN = mkPN "Jussi" ;
    question_N = mkN "kysymys" ;
    ready_A = mkA (mkN "valmis") ;
    reason_N = mkN "syy" ;
    today_Adv = mkAdv "t�n��n" ;
    uncertain_A = mkA "ep�varma" ;

 oper
    mkOrd : N -> Ord ;
    mkOrd x = {s = \\n,c => x.s ! NCase n c; lock_Ord = <> } ;
    cpartitive = casePrep partitive ;

} ;