instance DiffHin of DiffHindustani = open CommonHindustani, ResHindustani,Prelude in {
--instance DiffHin of DiffHindustani = CommonHindustani ** open Prelude in {
flags coding = utf8;
oper
Clause : Type = {s : VPHTense => Polarity => Order => Str} ;
  mkClause : NP -> VPH -> Clause = \np,vp -> {
      s = \\vt,b,ord => 
        let 
          subjagr : NPCase * Agr = case vt of {
            VPImpPast => case vp.subj of {
              VTrans     => <NPErg, vp.obj.a> ;
              VTransPost => <NPErg, defaultAgr> ;
              _          => <NPC Dir, np.a>
              } ;
            _ => <NPC Dir, np.a>
            } ;
          subj = subjagr.p1 ;
          agr  = subjagr.p2 ;
		  n    = (fromAgr agr).n;
		  p    = (fromAgr agr).p;
		  g    = (fromAgr agr).g;
          vps  = case vt of {

		   VPGenPres  => vp.s !  VPTense VPPres agr ;
		   VPImpPast  => vp.s !  VPTense VPPast agr ;		    
		   VPFut      => case vp.prog of { True => {fin = [] ;inf = Prelude.glue ((vp.s !  VPTense VPFutr agr).inf ++ hw p n) ((vp.s !  VPTense VPFutr agr).fin) } ;
                                                   _    => {fin = [] ; inf = Prelude.glue (vp.s !  VPTense VPFutr agr).inf (vp.s !  VPTense VPFutr agr).fin  }} ;
                   VPContPres => {fin = copula CPresent n p g ; inf = (vp.s ! VPStem).inf ++ raha g n } ;
		   VPContPast => {fin = copula CPast n p g ; inf = (vp.s ! VPStem).inf ++ raha g n } ;
		   VPContFut  => {fin = [] ; inf = Prelude.glue ((vp.s ! VPStem).inf ++ raha g n ++ hw p n) (copula CFuture n p g) } ;
		   VPPerfPres => {fin = copula CPresent n p g ; inf = (vp.s ! VPTense VPPerf agr).inf } ;  
		   VPPerfPast => {fin = copula CPast n p g ; inf = (vp.s ! VPTense VPPerf agr).inf } ;  
		   VPPerfFut  => {fin = [] ; inf = Prelude.glue ((vp.s ! VPTense VPPerf agr).inf  ++ hw p n) (copula CFuture n p g) } ;
		   VPPerfPresCont => {fin = copula CPresent n p g ; inf = (vp.s ! VPTense VPPres agr).inf ++ raha g n } ;					
	           VPPerfPastCont => {fin = copula CPast n p g ; inf = (vp.s ! VPTense VPPres agr).inf ++ raha g n } ;					
		   VPPerfFutCont =>  {fin = [] ; inf = Prelude.glue ((vp.s ! VPTense VPPres agr).inf ++ raha g n  ++ hw p n) (copula CFuture n p g) } ;					
		 --  VPSubj   => case vp.prog of { True => {fin = (vp.s !  VPTense VPFutr agr).inf ++ hw p n ; inf = "शायद" } ;
		   VPSubj   => case vp.prog of { True => {fin = Prelude.glue ((vp.s !  VPTense VPFutr agr).inf ++ hw p n) (copula CFuture n p g) ; inf =[] } ;
		   _    => {fin = Prelude.glue (vp.s !  VPTense VPFutr agr).inf (copula CFuture n p g); inf = [] } } 
                   
		  };
					
		    
          quest =
            case ord of
              { ODir => [];
                OQuest => "क्या" }; 
		  na =
            case b of
              { Pos => [];
                Neg => "न" };
           nahim =
            case b of 
              { Pos => [];
                Neg => "नहीं" };
        in
		case vt of {
		VPSubj => quest ++ np.s ! subj ++ vp.obj.s ++ vp.ad ++ vp.comp ! np.a  ++ vp.cvp ++ na ++  vps.inf ++ vps.fin ++ vp.embComp ;
		_      => quest ++ np.s ! subj ++ vp.obj.s ++ vp.ad ++ vp.comp ! np.a  ++ vp.cvp ++ nahim  ++  vps.inf ++ vps.fin ++ vp.embComp};

  } ;

  mkSClause : Str -> Agr -> VPH -> Clause =
    \subj,agr,vp -> {
      s = \\t,b,ord => 
        let 
		  n    = (fromAgr agr).n;
		  p    = (fromAgr agr).p;
		  g    = (fromAgr agr).g;
          vps  = case t of {
                    VPGenPres  => vp.s !  VPTense VPPres agr ;
		    VPImpPast  => vp.s !  VPTense VPPast agr ;
		    VPFut      => {fin = [] ; inf = Prelude.glue (vp.s !  VPTense VPFutr agr).inf ((vp.s !  VPTense VPFutr agr).fin) }  ;
		    VPContPres => {fin = copula CPresent n p g ; inf = (vp.s ! VPStem).inf ++ raha g n  } ;
		    VPContPast => {fin = copula CPast n p g ; inf = (vp.s ! VPStem).inf ++ raha g n } ;
		    VPContFut  => {fin = [] ; inf = Prelude.glue ((vp.s ! VPStem).inf ++ raha g n ++ hw p n) (copula CFuture n p g) } ;
		    VPPerfPres => {fin = copula CPresent n p g ; inf = (vp.s ! VPStem).inf ++ cka g n } ;
		    VPPerfPast => {fin = copula CPast n p g ; inf = (vp.s ! VPStem).inf ++ cka g n } ;
		    VPPerfFut  => {fin = [] ; inf = Prelude.glue ((vp.s ! VPStem).inf ++ cka g n ++ hw p n) (copula CFuture n p g) } ;
		    VPPerfPresCont => {fin = copula CPresent n p g ; inf = (vp.s ! VPStem).inf ++ raha g n } ; 
		    VPPerfPastCont => {fin = copula CPast n p g ; inf = (vp.s ! VPStem).inf ++ raha g n } ; 
		    VPPerfFutCont =>  {fin = [] ; inf = Prelude.glue ((vp.s ! VPStem).inf ++ raha g n ++ hw p n) (copula CFuture n p g) } ;
		    VPSubj   => {fin = Prelude.glue (insertSubj p (vp.s ! VPStem).inf) (copula CFuture n p g ); inf = [] }
                    
			  };

	  quest =
            case ord of
              { ODir => [];
                OQuest => "क्या" }; 
	  na =
            case b of
              { Pos => [];
                Neg => "न" };
          nahim =
            case b of 
              { Pos => [];
                Neg => "नहीं" };		
        in
		case t of {
		VPSubj => quest ++ subj ++ vp.obj.s ++ vp.ad ++ vp.comp ! agr  ++ vp.cvp ++ na ++  vps.inf ++ vps.fin ++ vp.embComp;
		_      => quest ++ subj ++ vp.obj.s ++ vp.ad ++ vp.comp ! agr  ++ vp.cvp ++ nahim ++  vps.inf ++ vps.fin ++ vp.embComp};
    } ;

 np2pronCase ppf npc a = case npc of {
       NPC c => ppf ! c;
       NPObj => ppf ! Obl ;
       NPErg =>case (fromAgr a).p of {
           (Pers3_Near|Pers3_Distant) => Prelude.glue (ppf ! Obl) "ने" ; -- in hindi in case of pronouns nE is tagged to pron rather than a separate word
	   _			     => Prelude.glue (ppf ! Dir) "ने"
	   }
      } ;
  conjThat = "कि" ;
  insertSubj : UPerson -> Str -> Str = \p,s -> 
      case p of { Pers1 => s ++ "वाँ" ; _ => Prelude.glue s "ए"}; -- check with prasad for vn~
      
  agr = "a-गर" ;
  awr = "और" ;
  ky = "की" ;
  jn = "जिन" ;
  js = "जिस" ;
  jw = "जो" ;
  kw = "को" ;
  mt = "मत" ;
  nE = "ने" ;
  nh = "न" ;
  sE = "से" ;
  waN = "वाँ" ; -- check with prasad
  hE = "हे" ;
  comma = "," ;
  indfArt = "" ; -- removed
  kwd = "ख़ुद" ; -- check with prasad

  oper 
  copula : CTense -> Number -> UPerson -> Gender -> Str = \t,n,p,g ->
    case <t,n,p,g> of {
       <CPresent,Sg,Pers1,_   > => "हूँ" ;
       <CPresent,Sg,Pers2_Casual,_   > => "है" ;
       <CPresent,Sg,Pers2_Familiar,_   > => "हो" ;
      <CPresent,Sg,Pers2_Respect,_   > => "हैं" ;
       <CPresent,Sg,Pers3_Near,_   > => "है" ;
       <CPresent,Sg,Pers3_Distant,_   > => "है" ;
	<CPresent,Pl,Pers1,_   > => "हैं" ;
       <CPresent,Pl,Pers2_Casual,_   > => "हो" ;
       <CPresent,Pl,Pers2_Familiar,_   > => "हो" ;
	<CPresent,Pl,Pers2_Respect,_   > => "हैं" ;
       <CPresent,Pl,Pers3_Near,_   > => "हैं" ;
       <CPresent,Pl,Pers3_Distant,_   > => "हैं" ;
      <CPast,Sg,Pers1,Masc   > => "था" ;
      <CPast,Sg,Pers1,Fem   > => "थी" ;
       <CPast,Sg,Pers2_Casual,Masc   > => "था" ;
      <CPast,Sg,Pers2_Casual,Fem   > => "थी" ;
       <CPast,Sg,Pers2_Familiar,Masc   > => "था" ;
      <CPast,Sg,Pers2_Familiar,Fem   > => "थी" ;
      <CPast,Sg,Pers2_Respect,Masc   > => "थे" ;
      <CPast,Sg,Pers2_Respect,Fem   > => "थीं" ;
       <CPast,Sg,Pers3_Near,Masc   > => "था" ;
      <CPast,Sg,Pers3_Near,Fem   > => "थी" ;
       <CPast,Sg,Pers3_Distant,Masc  > => "था" ;
      <CPast,Sg,Pers3_Distant,Fem  > => "थी" ;
      <CPast,Pl,Pers1,Masc   > => "थे" ;
      <CPast,Pl,Pers1,Fem   > => "थीं" ;
       <CPast,Pl,Pers2_Casual,Masc   > => "थे" ;
      <CPast,Pl,Pers2_Casual,Fem   > => "थीं" ;
       <CPast,Pl,Pers2_Familiar,Masc   > => "थे" ;
      <CPast,Pl,Pers2_Familiar,Fem   > => "थीं" ;
      <CPast,Pl,Pers2_Respect,Masc   > => "थे" ;
      <CPast,Pl,Pers2_Respect,Fem   > => "थीं" ;
       <CPast,Pl,Pers3_Near,Masc   > => "थे" ;
      <CPast,Pl,Pers3_Near,Fem   > => "थीं" ;
      <CPast,Pl,Pers3_Distant,Masc   > => "थे" ;
      <CPast,Pl,Pers3_Distant,Fem   > => "थीं" ;
      <CFuture,Sg,Pers1,Masc   > => "गा" ;
      <CFuture,Sg,Pers1,Fem   > => "गी" ;
       <CFuture,Sg,Pers2_Casual,Masc   > => "गा" ;
      <CFuture,Sg,Pers2_Casual,Fem   > => "गी" ;
       <CFuture,Sg,Pers2_Familiar,Masc   > => "गे" ;
      <CFuture,Sg,Pers2_Familiar,Fem   > => "गी" ;
      <CFuture,Sg,Pers2_Respect,Masc   > => "गे" ;
      <CFuture,Sg,Pers2_Respect,Fem   > => "गी" ;
       <CFuture,Sg,Pers3_Near,Masc   > => "गा" ;
      <CFuture,Sg,Pers3_Near,Fem   > => "गी" ;
       <CFuture,Sg,Pers3_Distant,Masc  > => "गा" ;
      <CFuture,Sg,Pers3_Distant,Fem  > => "गी" ;
      <CFuture,Pl,Pers1,Masc   > => "गे" ;
      <CFuture,Pl,Pers1,Fem   > => "गी" ;
       <CFuture,Pl,Pers2_Casual,Masc   > => "गे" ;
      <CFuture,Pl,Pers2_Casual,Fem   > => "गी" ;
       <CFuture,Pl,Pers2_Familiar,Masc   > => "गे" ;
      <CFuture,Pl,Pers2_Familiar,Fem   > => "गी" ;
      <CFuture,Pl,Pers2_Respect,Masc   > => "गे" ;
      <CFuture,Pl,Pers2_Respect,Fem   > => "गी" ;
       <CFuture,Pl,Pers3_Near,Masc   > => "गे" ;
      <CFuture,Pl,Pers3_Near,Fem   > => "गी" ;
      <CFuture,Pl,Pers3_Distant,Masc  > => "गे" ;
      <CFuture,Pl,Pers3_Distant,Fem  > => "गी"

       } ;
  

  raha : Gender -> Number -> Str = \g,n -> 
	   (regAdjective "रहा").s ! n ! g ! Dir ! Posit ;

  cka : Gender -> Number -> Str = \g,n -> 
	  (regAdjective "चुका").s ! n ! g ! Dir ! Posit ;
	  
  hw : UPerson -> Number -> Str = \pp,n ->    
	 case <pp,n> of {
	 <Pers1,_> => "हूँ";
	 <_,Pl>    => "होँ";
	 <_,_>		=> "हो"
	 };
  hwa : Agr -> Str = \agr ->
        let       n    = (fromAgr agr).n;
		  p    = (fromAgr agr).p;
		  g    = (fromAgr agr).g;
	  in
	 case <n,g> of {
	 <Sg,Masc> => "हुआ";
	 <Sg,Fem>    => "हुई";
	 <Pl,Masc>	=> "हुए" ;
	 <Pl,Fem>	=> "हुई"
	 };	 
   -----------------------------------------------
   -- Hindustani Adjectives
   -----------------------------------------------
   	 
 
  regAdjective : Str -> Adjective; 
  regAdjective x =  case x of {
	              acch + ("ा"|"न") => mkAdjective x  ("बहुत" ++ x)          ("सब से" ++ x)          (acch + "े") ("बहुत" ++ acch + "े") ("सब से" ++ acch + "े") (acch + "े") ("बहुत" ++ acch + "े") ("सब से" ++ acch + "े")
		                                      (acch + "ी") ("बहुत" ++ acch + "ी") ("सब से" ++ acch + "ी") (acch + "ी") ("बहुत" ++ acch + "ी") ("सब से" ++ acch + "ी") (acch + "ी") ("बहुत" ++ acch + "ी") ("सब से" ++ acch + "ी")
						      (acch +"े")  ("बहुत" ++ acch + "े") ("सब से" ++ acch + "े") (acch + "े") ("बहुत" ++ acch + "े") ("सब से" ++ acch + "े") (acch + "े") ("बहुत" ++ acch + "े") ("सब से" ++ acch + "े")
		                                      (acch + "ी") ("बहुत" ++ acch + "ी") ("सब से" ++ acch + "ी") (acch + "ी") ("बहुत" ++ acch + "ी") ("सब से" ++ acch + "ी") (acch + "ी") ("बहुत" ++ acch + "ी") ("सब से" ++ acch + "ी");
									
                        _                 => mkAdjective  x  ("बहुत" ++ x)  	("सब से" ++ x)  x ("बहुत" ++ x) ("सब से" ++ x) x ("बहुत" ++ x) ("सब से" ++ x)
							  x  ("बहुत" ++ x) 	("सब से" ++ x) 	x ("बहुत" ++ x) ("सब से" ++ x) x ("बहुत" ++ x) ("सब से" ++ x)
							  x  ("बहुत" ++ x) 	("सब से" ++ x)  x ("बहुत" ++ x) ("सब से" ++ x) x ("बहुत" ++ x) ("सब से" ++ x)
							  x  ("बहुत" ++ x) 	("सब से" ++ x)  x ("बहुत" ++ x) ("सब से" ++ x) x ("बहुत" ++ x) ("सब से" ++ x)
																 
                            }; 												 
                  					 
}