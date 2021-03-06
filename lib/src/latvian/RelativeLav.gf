--# -path=.:../abstract:../common:../prelude

concrete RelativeLav of Relative = CatLav ** open
  ResLav,
  VerbLav,
  Prelude
in {

flags
  optimize = all_subs ;
  coding = utf8 ;

lin
  RelCl cl = { s = \\m,p,_ => "ka" ++ cl.s ! m ! p } ;

  RelVP rp vp = {
    s = \\m,p,ag =>
      rp.s ! Masc ! Nom ++ 
      buildVerb vp.v m p (toAgr (fromAgr ag).n P3 (fromAgr ag).g) False vp.objNeg ++ 
      vp.compl ! ag
  } ;

  -- FIXME: vārdu secība - nevis 'kas mīl viņu' bet 'kas viņu mīl' (?)
  -- FIXME: Masc varētu nebūt labi
  RelSlash rp slash = {
    s = \\m,p,ag => slash.p.s ++ rp.s ! Masc ! (slash.p.c ! Sg) ++  slash.s ! m ! p
  } ;

  -- FIXME: placeholder
  -- TODO: jātestē, kautkas nav labi ar testpiemēru
  FunRP p np rp = {
    s = \\g,c => p.s ++ rp.s ! g ! c ++ np.s ! (p.c ! (fromAgr np.a).n)
  } ;

  IdRP = {
    s = \\_ => table {
      Nom => "kas" ;
      Gen => "kā" ;
      Dat => "kam" ;
      Acc => "ko" ;
      Loc => "kur" ;
      ResLav.Voc => NON_EXISTENT
    }
  } ;

}
