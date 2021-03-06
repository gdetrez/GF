resource ParadigmsCmn = open CatCmn, ResCmn, Prelude in {

flags coding = utf8 ;

flags coding=utf8;
oper
  mkN = overload {
    mkN : (man : Str) -> N 
      = \n -> lin N (regNoun n ge_s) ;  
    mkN : (man : Str) -> Str -> N 
      = \n,c -> lin N (regNoun n c)
    } ;  

  mkN2 = overload {
    mkN2 : Str -> N2
      = \n -> lin N2 (regNoun n ge_s ** {c2 = emptyPrep}) ; ---- possessive ?
----    mkN2 : N -> Str -> N2
----      = \n,p -> lin N2 (n ** {c2 = mkPrep p}) ;
    } ;

  mkN3 : N -> Preposition -> Preposition -> N3
      = \n,p,q -> lin N3 (n ** {c2 = p ; c3 = q}) ;

      
  mkPN : (john : Str) -> PN
     = \s -> lin PN {s = word s} ; 

  mkA = overload {
    mkA : (small : Str) -> A 
      = \a -> lin A (simpleAdj a) ;
    mkA : (small : Str) -> Bool -> A 
      = \a,b -> lin A (mkAdj a b) ;
    } ; 

  mkA2 : Str -> A2 = \a -> lin A2 (simpleAdj a ** {c2 = emptyPrep}) ;

  mkV = overload {      
    mkV : (walk : Str) -> V 
      = \walk -> lin V (regVerb walk) ;
    mkV : (arrive : Str) -> Str -> Str -> Str -> Str -> V
      = \arrive,pp,ds,dp,ep -> lin V (mkVerb arrive pp ds dp ep neg_s) ;
    mkV : (arrive : Str) -> Str -> Str -> Str -> Str -> Str -> V
      = \arrive,pp,ds,dp,ep,neg -> lin V (mkVerb arrive pp ds dp ep neg) ;
      } ;      


  mkV2 : Str -> V2 
      = \s -> lin V2 (regVerb s ** {c2 = emptyPrep}) ;

  mkV3 = overload {
    mkV3 : Str -> V3
      = \s -> lin V3 (regVerb s ** {c2,c3 = emptyPrep}) ;
    mkV3 : V -> V3
      = \s -> lin V3 (s ** {c2,c3 = emptyPrep}) ;
----    mkV3 : V -> Str -> Str -> V3
----      = \v,p,q -> lin V3 (v ** {c2 = mkPrep p ; c3 = mkPrep q}) ;
    } ;

  mkVV : Str -> VV = ----
    \v -> lin VV (regVerb v) ;

  mkVQ : V -> VQ =
    \v -> lin VQ v ;

  mkVS : V -> VS =
    \v -> lin VS v ;

  mkVA : V -> VA =
    \v -> lin VA v ;

  mkV2Q : V -> V2Q =
    \v -> lin V2Q (v ** {c2 = emptyPrep}) ; 
----  mkV2Q : V -> Str -> V2Q =
----    \v,p -> lin V2Q (v ** {c2 = mkPrep p}) ; 

  mkV2V : V -> V2V =
    \v -> lin V2V (v ** {c2 = emptyPrep ; c3 = emptyPrep}) ; 
----  mkV2V : V -> Str -> Str -> V2V =
----    \v,p,q -> lin V2V (v ** {c2 = mkPrep p ; c3 = mkPrep q}) ; 

  mkV2S : V -> V2S =
    \v -> lin V2S (v ** {c2 = emptyPrep}) ; 
----  mkV2S : V -> Str -> V2S =
----    \v,p -> lin V2S (v ** {c2 = mkPrep p}) ; 

  mkV2A : V -> V2A
    = \v -> lin V2A (v ** {c2 = emptyPrep ; c3 = emptyPrep}) ; 
----  mkV2A : V -> Str -> Str -> V2A
----    = \v,p,q -> lin V2A (v ** {c2 = mkPrep p ; c3 = mkPrep q}) ; 

  mkAdv = overload {
    mkAdv : Str -> Adv 
      = \s -> lin Adv {s = word s ; advType = ATPlace} ;
    mkAdv : Str -> AdvType -> Adv 
      = \s,at -> lin Adv {s = word s ; advType = at} ;
    } ;

  AdvType : Type
   = ResCmn.AdvType ;
  placeAdvType : AdvType
   = ATPlace ;
  timeAdvType : AdvType
   = ATTime ;
  mannerAdvType : AdvType
   = ATManner ;
    
  mkPrep = overload { ---- is this the right order of the fields?
    mkPrep : Str -> Preposition 
     = \s -> ResCmn.mkPreposition s [] ;
    mkPrep : Str -> Str -> Preposition 
     = \s,t -> ResCmn.mkPreposition s t ;
    } ;

  emptyPrep : Preposition = mkPrep [] ;


}

