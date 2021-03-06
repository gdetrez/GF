--# -path=.:../abstract:../common:../prelude

concrete StructuralLav of Structural = CatLav ** open
  Prelude,
  ResLav,
  ParadigmsLav,
  ParadigmsPronounsLav,
  NounLav
  in {

flags
  optimize = all ;
  coding = utf8 ;

lin
  language_title_Utt = ss "latviešu valoda" ;

  -- TODO: kā ar loģikā lietotajiem 'visi', 'katrs' ?
  every_Det = {
    s = (\\g,c => (mkPronoun_Gend "ikviens").s ! g ! Sg ! c) ;
    n = Sg ;
    d = Indef ;
    isNeg = False
  } ;

  someSg_Det = {
    s = (\\g,c => (mkPronoun_Gend "kāds").s ! g ! Sg ! c) ;  --  lai atļautu arī tukšo, jāliek (\\_,_ => []) |  klāt
    n = Sg ;
    d = Indef ;
    isNeg = False
  } ;

  somePl_Det = {
    s = (\\g,c => (mkPronoun_Gend "kāds").s ! g ! Pl ! c) ;   --  lai atļautu arī tukšo, jāliek (\\_,_ => []) |  klāt
    n = Pl ;
    d = Indef ;
    isNeg = False
  } ;

  few_Det = {
    s = (\\g,c => (mkPronoun_Gend "dažs").s ! g ! Pl ! c) ;
    n = Pl ;
    d = Indef ;
    isNeg = False
  } ;

  many_Det = {
    s = (\\g,c => (mkPronoun_Gend "daudzs").s ! g ! Pl ! c) ;  -- 'daudzs' izlocīsies korekti uz daudzskaitļa 'daudzi'
    n = Pl ;
    d = Indef ;
    isNeg = False
  } ;

  much_Det = {
    s = (\\g,c => "daudz") ;  -- FIXME - ņem saistību ar ģenitīvu; kā to realizēt?
    n = Sg ;
    d = Indef ;
    isNeg = False
  } ;

  this_Quant = {
    s = (mkPronoun_ThisThat This).s ;
    d = Def ;
    isNeg = False
  } ;

  that_Quant = {
    s = (mkPronoun_ThisThat That).s ;
    d = Def ;
    isNeg = False
  } ;

  no_Quant = {
    s = (mkPronoun_Gend "neviens").s ;
    d = Indef ;
    isNeg = True
  } ;

  -- P1

  i_Pron = mkPronoun_I Masc ;  -- See also: ExtraLav.i8fem_Pron

  we_Pron = mkPronoun_We Masc ;  -- See also: ExtraLav.we8fem_Pron

  -- P2

  youSg_Pron = mkPronoun_You_Sg Masc ;  -- See also: ExtraLav.youSg8fem_Pron

  youPol_Pron = mkPronoun_You_Pol Masc ;  -- See also: ExtraLav.youPol8fem_Pron

  youPl_Pron = mkPronoun_You_Pl Masc ;  -- See also: ExtraLav.youPl8fem_Pron

  -- P3

  he_Pron = {
    s = \\c => (mkPronoun_Gend "viņš").s ! Masc ! Sg ! c ;
    a = AgP3 Sg Masc ;
    possessive = \\_,_,_ => "viņa"
  } ;

  she_Pron = {
    s = \\c => (mkPronoun_Gend "viņš").s ! Fem ! Sg ! c ;
    a = AgP3 Sg Fem ;
    possessive = \\_,_,_ => "viņas"
  } ;

  they_Pron = mkPronoun_They Masc ;  -- See also: ExtraLav.they8fem_Pron

  it_Pron = mkPronoun_It_Sg Masc ;  -- See also: ExtraLav.it8fem_Pron

  -- manuprāt prievārdi tomēr ir valodas-specifiski un nebūtu tieši 1-pret-1 jātulko
  above_Prep = mkPrep "virs" Gen Dat ;
  after_Prep = mkPrep "pēc" Gen Dat ;
  before_Prep = mkPrep "pirms" Gen Dat ;
  behind_Prep = mkPrep "aiz" Gen Dat ;
  between_Prep = mkPrep "starp" Acc Dat ;
  for_Prep = mkPrep "priekš" Gen Dat ;
  from_Prep = mkPrep "no" Gen Dat ;
  on_Prep = mkPrep "uz" Gen Dat ;
  with_Prep = mkPrep "ar" Acc Dat ;  -- ar sievu, ar sievām
  in_Prep = mkPrep Loc ;
  to_Prep = mkPrep "līdz" Dat Dat ;  -- See also: ExtraLav.to8uz_Prep
  possess_Prep = mkPrep Gen ;  -- FIXME: vajadzētu vārdu secību otrādi - pirms paskaidrojamā vārda likt
  under_Prep = mkPrep "zem" Gen Dat ;
  with_Prep = mkPrep "ar" Acc Dat ;
  without_Prep = mkPrep "bez" Gen Dat ;
  by8agent_Prep = nom_Prep ; -- TODO: should get rid of this Prep
  by8means_Prep = mkPrep "ar" Acc Dat ;
  during_Prep = mkPrep "laikā" Gen Gen ;  -- FIXME: laikam postfix 'X laikā' jāliek
  in8front_Prep = mkPrep "priekšā" Dat Dat ;
  --part_Prep = mkPrep Gen ; --FIXME - vajadzētu vārdu secību otrādi - pirms paskaidrojamā vārda likt
  through_Prep = mkPrep "cauri" Dat Dat ;
  except_Prep = mkPrep "izņemot" Acc Acc ;

  very_AdA = mkAdA "ļoti" ;
  almost_AdA = mkAdA "gandrīz" ;
  so_AdA = mkAdA "tik" ;
  too_AdA = mkAdA "pārāk" ;

  and_Conj = mkConj "un" ;
  or_Conj = mkConj "vai" Sg ;
  if_then_Conj = mkConj "ja" "tad" ;

  but_PConj = ss "bet" ;
  otherwise_PConj = ss "citādi" ;
  therefore_PConj = ss "tāpēc" ;

  more_CAdv = (mkCAdv [] "nekā" Compar) | (mkCAdv "vairāk" "nekā" Posit) ;
  less_CAdv = mkCAdv "mazāk" "nekā" Posit ;
  as_CAdv = mkCAdv "tikpat" "kā" Posit ;

  here_Adv = mkAdv "šeit" ;
  there_Adv = mkAdv "tur" ;
  everywhere_Adv = mkAdv "visur" ;
  here7to_Adv = mkAdv ["uz šejieni"] ;
  here7from_Adv = mkAdv ["no šejienes"] ;
  there7to_Adv = mkAdv "uz turieni" ;
  there7from_Adv = mkAdv "no turienes" ;
  somewhere_Adv = mkAdv "kaut kur" ;
  quite_Adv = mkAdv "diezgan" ;

  both7and_DConj = mkConj "gan" ("," ++ "gan"); --FIXME - komati nav tā kā vajag
  either7or_DConj = mkConj ("vai" ++ "nu") ("," ++ "vai") Sg ; --FIXME - komati nav tā kā vajag

  want_VV = mkVV (mkV "vēlēties" third_conjugation) ;

  whoSg_IP = { -- FIXME: Fem
    s = table {
      Nom => "kurš" ;
      Gen => "kura" ;
      Dat => "kuram" ;
      Acc => "kuru" ;
      Loc => "kurā" ;
      ResLav.Voc => NON_EXISTENT
    } ;
    n = Sg
  } ;
  
  whoPl_IP = { -- FIXME: Fem
    s = table {
      Nom => "kuri" ;
      Gen => "kuru" ;
      Dat => "kuriem" ;
      Acc => "kurus" ;
      Loc => "kuros" ;
      ResLav.Voc => NON_EXISTENT
    } ;
    n = Pl
  } ;
  
  whatSg_IP = {
    s = table {
      Nom => "kas" ;
      Gen => "kā" ;
      Dat => "kam" ;
      Acc => "ko" ;
      Loc => "kur" ;
      ResLav.Voc => NON_EXISTENT
    } ;
    n = Sg
  } ;
  
  whatPl_IP = {
    s = table {
      Nom => "kas" ;
      Gen => "kā" ;
      Dat => "kam" ;
      Acc => "ko" ;
      Loc => "kur" ;
      ResLav.Voc => NON_EXISTENT
    } ;
    n = Pl
  } ;

  why_IAdv = ss "kāpēc" ;
  how_IAdv = ss "kā" ;
  how8much_IAdv = ss "cik" ;
  when_IAdv = ss "kad" ;
  where_IAdv = ss "kur" ;

  which_IQuant = {
    s = table {
      Masc => table { Sg => "kurš"; Pl => "kuri" } ;
      Fem => table { Sg => "kura"; Pl => "kuras" }
    }
  } ;

  when_Subj = ss "kad" ;
  although_Subj = ss "kaut arī" ;
  because_Subj = ss "jo" ;
  if_Subj = ss "ja" ;
  that_Subj = ss "ka" ;

  all_Predet = { s = table { Masc => "visi" ; Fem => "visas" } } ;

  only_Predet = { s = table { _ => "tikai"} } ;
  most_Predet = { s = table { _ => "vairums"} } ;

  yes_Utt = ss "jā" ;
  please_Voc = ss "lūdzu" ;

  almost_AdN = mkAdN "gandrīz" ;
  at_least_AdN = mkAdN "vismaz" ;
  at_most_AdN = mkAdN "ne vairāk kā" ;

  always_AdV = mkAdV "vienmēr" ;

  how8many_IDet = {
    s = table { _ => "cik" } ;
    n = Pl
  } ;

  somebody_NP = UsePron (mkPronoun_Body "kāds") ;
  something_NP = UsePron (mkPronoun_Thing "kaut kas") ;
  everybody_NP = UsePron (mkPronoun_Body "ikviens") ;
  everything_NP = UsePron (mkPronoun_Thing "jebkas") ;
  nobody_NP = UsePron (mkPronoun_Body "neviens") ;
  nothing_NP = UsePron (mkPronoun_Thing "nekas") ;

  have_V2 = mkV2 (mkV "būt") nom_Prep Dat ;
  have_V3 = mkV3 (mkV "būt") nom_Prep dat_Prep Dat ;

  can8know_VV, can_VV = mkVV (mkV "varēt" third_conjugation) ;
  must_VV = mkVV (mkV "vajadzēt" third_conjugation) Dat ;

oper
  
  reflPron : Case => Str = table {
    Nom => NON_EXISTENT ;
    Gen => "sevis" ;
    Dat => "sev" ;
    Acc => "sevi" ;
    Loc => "sevī" ;
    ResLav.Voc => NON_EXISTENT
  } ;

  lai_Subj = ss "lai" ;
  kameer_Subj = ss "kamēr" ;

  emptyPl_Det = { -- TODO: pašlaik netiek izmantots, jāpārnes uz ExtraLav(?) kā dsk. alternatīva a_Art, the_Art, no_Quant, every_Det 
    s : Gender => Case => Str = \\_,_ => [] ;
    n = Pl ;
    d = Indef
  } ;

  emptySg_Det = { -- TODO: analoģiski kā emptyPl_Det
    s : Gender => Case => Str = \\_,_ => [] ;
    n = Sg ;
    d = Indef
  } ;

  no_Utt = ss "nē" ;

}
