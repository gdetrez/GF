--# -path=.:../abstract:../common:prelude

concrete AllLat of AllLatAbs = 
  LangLat,
  IrregLat-[
    blow_V,burn_V,come_V,dig_V,fall_V,fly_V,freeze_V,go_V,lie_V,run_V,
    sew_V,sing_V,sit_V,sleep_V,spit_V,stand_V,swell_V,swim_V,think_V],
  ExtraLat
  ** {} ;