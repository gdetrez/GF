concrete ConjunctionTha of Conjunction = CatTha ** open Prelude, Coordination in {

  lin

    ConjS = conjunctDistrSS ;
    ConjAdv = conjunctDistrSS ;
    ConjNP = conjunctDistrSS ;
    ConjAP = conjunctDistrSS ;
    ConjRS = conjunctDistrSS ;

-- These fun's are generated from the list cat's.

    BaseS = twoSS ;
    ConsS = consrSS comma ;
    BaseAdv = twoSS ;
    ConsAdv = consrSS comma ;
    BaseNP = twoSS ;
    ConsNP = consrSS comma ;
    BaseAP = twoSS ;
    ConsAP = consrSS comma ;
    BaseRS = twoSS ;
    ConsRS = consrSS comma ;

  lincat
    [S] = {s1,s2 : Str} ;
    [Adv] = {s1,s2 : Str} ;
    [NP] = {s1,s2 : Str} ;
    [AP] = {s1,s2 : Str} ;
    [RS] = {s1,s2 : Str} ;

}
