-- (c) 2009 Aarne Ranta under LGPL

concrete WordsGer of Words = SentencesGer ** 
    open SyntaxGer, ParadigmsGer, IrregGer, (L = LexiconGer), ExtraGer, Prelude in {

   lin

-- kinds of food
 
    Apple = mkCN L.apple_N ;
    Beer = mkCN L.beer_N ;
    Bread = mkCN L.bread_N ;
    Cheese = mkCN (mkN "K�se" "K�se" "K�se" "K�se" "K�se" "K�se" masculine) ;
    Chicken = mkCN (mkN "Huhn" "Huhn" "Huhn" "Huhn" "H�hner" "H�hner" neuter) ;
    Coffee = mkCN (mkN "Kaffee" "Kaffee" "Kaffee" "Kaffee" "Kaffees" "Kaffee" masculine) ;
    Fish = mkCN L.fish_N ;
    Meat = mkCN (mkN "Fleisch" "Fleisch" "Fleisch" "Fleisch" "Fleisch" "Fleisch" neuter) ;
    Milk = mkCN L.milk_N ;
    Pizza = mkCN (mkN "Pizza" "Pizzen" feminine) ;
    Salt = mkCN L.salt_N ;
    Tea = mkCN (mkN "Tee" "Tee" "Tee" "Tee" "Tees" "Tees" masculine) ;
    Water = mkCN L.water_N ;
    Wine = mkCN L.wine_N ;

-- properties

    Bad = L.bad_A ;
    Cheap = mkA "billig" ;
    Boring = mkA "langweilig" ;
    Cold = L.cold_A ;
    Delicious = mkA "lecker" ;
    Expensive = mkA "teuer" ;
    Fresh = mkA "frisch" ;
    Good = L.good_A ;
    Warm = L.warm_A ;
    Suspect = mkA "vermut" ;

-- places

    Airport = mkPlace (mkN "Flughafen" "Flugh�fen" masculine) on_Prep to_Prep ; -- am/an dem (dative)  --im/in dem
    Church = mkPlace (mkN "Kirche") in_Prep to_Prep ;
    Hospital = mkPlace (mkN "Krankenhaus" "Krankenh�user" neuter) in_Prep to_Prep ;
    Restaurant = mkPlace (mkN "Restaurant" "Restaurants" neuter) in_Prep to_Prep ;
    Station = mkPlace (mkN "Bahnhofen" "Bahnh�fen" masculine) on_Prep to_Prep ;
    University = mkPlace (mkN "Universit�t" "Universit�ten" feminine) in_Prep to_Prep ;

    AmusementPark = mkPlace (mkN "Vergn�gungspark" "Vergn�gungspark" "Vergn�gungspark" "Vergn�gungspark" "Vergn�gungsparks" "Vergn�gungsparks" masculine) in_Prep to_Prep ;
    Bank = mkPlace (mkN "Bank" "Bank" "Bank" "Bank" "Banken" "Banken" feminine) in_Prep to_Prep ;
    Bar = mkPlace (mkN "Bar" "Bar" "Bar" "Bar" "Bars" "Bars" masculine) in_Prep to_Prep ;
    Cafeteria = mkPlace (mkN "Cafeteria" "Cafeteria" "Cafeteria" "Cafeteria" "Cafeterien" "Cafeterien" feminine) in_Prep to_Prep ;
    Center = mkPlace (mkN "Zentrum" "Zentrum" "Zentrum" "Zentrum" "Zentren" "Zentren" neuter) in_Prep to_Prep ;
    Cinema = mkPlace (mkN "Kino" "Kino" "Kino" "Kino" "Kinos" "Kinos" neuter) in_Prep to_Prep ;
    Disco = mkPlace (mkN "Disco" "Disco" "Disco" "Disco" "Discos" "Discos" feminine) in_Prep to_Prep ;
    Hotel = mkPlace (mkN "Hotel" "Hotel" "Hotel" "Hotel" "Hotels" "Hotels" neuter) in_Prep to_Prep ;
    Museum = mkPlace (mkN "Museum" "Museum" "Museum" "Museum" "Museen" "Museen" neuter) in_Prep to_Prep ;
    Park = mkPlace (mkN "Park" "Park" "Park" "Park" "Parks" "Parks" masculine) in_Prep to_Prep ;
    Parking = mkPlace (mkN "Parkplatz" "Parkplatz" "Parkplatz" "Parkplatz" "Parkplatzen" "Parkplatzen" masculine) on_Prep to_Prep ;
    Pharmacy = mkPlace (mkN "Apotheke" "Apotheke" "Apotheke" "Apotheke" "Apotheken" "Apotheken" feminine) in_Prep to_Prep ;
    PostOffice = mkPlace (mkN "Post" "Post" "Post" "Post" "Posten" "Posten" feminine) in_Prep to_Prep ;
    Pub = mkPlace (mkN "Kneipe" "Kneipe" "Kneipe" "Kneipe" "Kneipen" "Kneipen" feminine) in_Prep to_Prep;
    School = mkPlace (mkN "Schule" "Schule" "Schule" "Schule" "Schulen" "Schule" feminine) in_Prep to_Prep ;
    Shop = mkPlace (mkN "Gesch�ft" "Gesch�ft" "Gesch�ft" "Gesch�ft" "Gesch�fte" "Gesch�fte" neuter) in_Prep to_Prep ;
    Supermarket = mkPlace (mkN "Supermarkt" "Supermarkt" "Supermarkt" "Supermarkt" "Superm�rkten" "Superm�rkte" masculine) in_Prep to_Prep ;
    Theatre = mkPlace (mkN "Theater" "Theater" "Theater" "Theaters" "Theatern" "Thaters" neuter) in_Prep to_Prep ; 
    Toilet = mkPlace (mkN "Toilette" "Toilette" "Toilette" "Toilette" "Toiletten" "Toiletten" feminine) in_Prep to_Prep ; 
    Zoo = mkPlace (mkN "Zoo" "Zoo" "Zoo" "Zoo" "Zoos" "Zoos" masculine) in_Prep to_Prep ;


CitRestaurant cit = mkCNPlace (mkCN cit  (mkN "Restaurant" "Restaurants" neuter)) in_Prep to_Prep ;


-- currencies

    DanishCrown = mkCN (mkA "D�nisch") (mkN "Krone" "Kronen" feminine) | mkCN (mkN "Krone" "Kronen" feminine) ;
     Dollar = mkCN (mkN "Dollar" "Dollar" "Dollar" "Dollar" "Dollar" "Dollar" masculine) ;
    Euro = mkCN (mkN "Euro" "Euro" "Euro" "Euro" "Euro" "Euro" neuter) ;
     Lei = mkCN (mkN "Leu" "Leu" "Leu" "Leu" "Lei" "Lei" masculine) ;
    SwedishCrown = mkCN (mkA "Schwedisch") (mkN "Krone" "Kronen" feminine) | mkCN (mkN "Krone" "Kronen" feminine) ;
    Leva = mkCN (mkN "Lewa" "Lewa" "Lewa" "Lewa" "Lewa" "Lewa" feminine);
    NorwegianCrown = mkCN (mkA "Norwegisch") (mkN "Krone" "Kronen" feminine) | mkCN (mkN "Krone" "Kronen" feminine) ;
    Pound = mkCN (mkN "Pfund" "Pfund" "Pfund" "Pfund" "Pfund" "Pfund" neuter) ;
    Rouble = mkCN (mkN "Rubel" "Rubel" "Rubel" "Rubel" "Rubels" "Rubels" masculine);
    Zloty = mkCN (mkN "Zloty" "Zloty" "Zloty" "Zloty" "Zloty" "Zloty" masculine);
   


-- nationalities

    Belgian = mkA "Belgisch" ;
    Belgium = mkNP (mkPN "Belgien") ;
    Bulgarian = mkNat "Bulgarien" "Bulgarisch" "bulgarisch" ;	
    Catalan = mkNat "Katalonien" "Katalanisch" "katalanisch" ;
    Danish = mkNat "D�nemark" "D�nisch" "d�nisch" ;
    Dutch = mkNat "den Niederlanden" "Niederl�ndisch" "niederl�ndisch" ;
    English = mkNat "England" "Englisch" "englisch" ;
    Finnish = mkNat "Finnland" "Finnisch" "finnisch" ;
    Flemish = mkNP (mkPN "Fl�misch") ;
    French = mkNat "Frankreich" "Franz�sisch" "franz�sisch" ;
    German = mkNat "Deutschland" "Deutsch" "deutsche" ;
    Italian = mkNat "Italien" "Italienisch" "italienisch" ;
    Norwegian = mkNat "Norwegen" "Norwegisch" "norwegisch" ;
    Polish = mkNat "Polen" "Polnisch" "polnisch"  ;
    Romanian = mkNat "Rum�nien" "Rum�nisch" "rum�nisch" ;
    Russian = mkNat "Russland" "Russisch" "russisch" ;
    Spanish = mkNat "Spanien" "Spanisch" "spanisch" ;
    Swedish = mkNat "Schweden" "Schwedisch" "schwedisch" ;



-- actions

    AHasAge p num = mkCl p.name (mkNP num L.year_N) ;
    AHasName p name = mkCl p.name (mkV2 hei�en_V) name ;
    AHungry p = mkCl p.name (mkA "hungrig") ;
    AHasChildren p num = mkCl p.name have_V2 (mkNP num L.child_N) ;
    AHasRoom p num = mkCl p.name have_V2 
      (mkNP (mkNP a_Det (mkN "Zimmer" "Zimmer" neuter)) 
        (SyntaxGer.mkAdv for_Prep (mkNP num (mkN "Persone")))) ;
    AHasTable p num = mkCl p.name have_V2 
      (mkNP (mkNP a_Det (mkN "Tisch")) 
        (SyntaxGer.mkAdv for_Prep (mkNP num (mkN "Persone")))) ;
    AIll p = mkCl p.name (mkA "krank") ;
    AKnow p = mkCl p.name wissen_V ;
    ALike p item = mkCl p.name (mkV2 m�gen_V) item;
    ALive p co = mkCl p.name (mkVP (mkVP (mkV "wohnen")) (SyntaxGer.mkAdv in_Prep co)) ;
    ALove p q = mkCl p.name (mkV2 (mkV "lieben")) q.name ;
    AMarried p = mkCl p.name (mkA "verheiratet") ;
    AReady p = mkCl p.name (mkA "fertig") ;
    AScared p = mkCl p.name have_V2 (mkNP (mkN "Angst" "Angsten" feminine)) ;
    ASpeak p lang = mkCl p.name  (mkV2 sprechen_V) lang ;
    AThirsty p = mkCl p.name (mkA "durstig") ;
    ATired p = mkCl p.name (mkA "m�de") ;
    AUnderstand p = mkCl p.name (fixprefixV "ver" stehen_V) ;
    AWant p obj = mkCl p.name want_VV (mkVP have_V2 obj) ;
    AWantGo p place = mkCl p.name want_VV (mkVP (mkVP L.go_V) place.to) ;

-- miscellaneous

    QWhatName p = mkQS (mkQCl how_IAdv (mkCl p.name hei�en_V)) ;
    QWhatAge p = mkQS (mkQCl (ICompAP (mkAP L.old_A)) p.name) ;

    PropOpen p = mkCl p.name open_Adv ;
    PropClosed p = mkCl p.name closed_Adv ;
    PropOpenDate p d = mkCl p.name (mkVP (mkVP d) open_Adv) ; 
    PropClosedDate p d = mkCl p.name (mkVP (mkVP d) closed_Adv) ; 
    PropOpenDay p d = mkCl p.name (mkVP (mkVP d.habitual) open_Adv) ; 
    PropClosedDay p d = mkCl p.name (mkVP (mkVP d.habitual) closed_Adv) ; 

    HowMuchCost item = mkQS (mkQCl how8much_IAdv (mkCl item (mkV "kosten"))) ; 
    ItCost item price = mkCl item (mkV2 (mkV "kosten")) price ;

-- Building phrases from strings is complicated: the solution is to use
-- mkText : Text -> Text -> Text ;

    PSeeYouDate d = mkText (lin Text (ss ("Wir sehen uns"))) (mkPhrase (mkUtt d)) ;
    PSeeYouPlace p = mkText (lin Text (ss ("Wir sehen uns"))) (mkPhrase (mkUtt p.at)) ;
    PSeeYouPlaceDate p d = 
      mkText (lin Text (ss ("Wir sehen uns"))) 
        (mkText (mkPhrase (mkUtt p.at)) (mkPhrase (mkUtt d))) ;


-- Relations are expressed as "my wife" or "my son's wife", as defined by $xOf$
-- below. Languages without productive genitives must use an equivalent of
-- "the wife of my son" for non-pronouns.

    Wife = xOf sing (mkN "Frau" "Frauen" feminine) ;
    Husband = xOf sing L.man_N ;
    Son = xOf sing (mkN "Sohn" "S�hne" masculine) ;
    Daughter = xOf sing (mkN "Tochter" "T�chter" feminine) ;
    Children = xOf plur L.child_N ;

-- week days

    Monday = mkDay "Montag" ;
    Tuesday = mkDay "Dienstag" ;
    Wednesday = mkDay "Mittwoch" ;
    Thursday = mkDay "Donnerstag" ;
    Friday = mkDay "Freitag" ;
    Saturday = mkDay "Samstag" ;
    Sunday = mkDay "Sonntag" ;

    Tomorrow = ParadigmsGer.mkAdv "morgen" ;

    TheBest = mkSuperl L.good_A ;
    TheClosest = mkSuperl L.near_A ; 
    TheCheapest = mkSuperl (mkA "billig") ;
    TheMostExpensive = mkSuperl (mkA "teuer") ;
    TheMostPopular = mkSuperl (mkA "beliebt") ;
    TheWorst = mkSuperl (mkA "schlimm") ;

    SuperlPlace sup p = placeNP sup p ;


-- means of transportation 

    Bike = mkTransport L.bike_N ; 
    Bus = mkTransport (mkN "Bus" "Bus" "Bus" "Bus" "Buss" "Buss" masculine) ;
    Car = mkTransport L.car_N ;
    Ferry = mkTransport (mkN "F�hre" "F�hre" "F�hre" "F�hre" "F�hren" "F�hren" feminine) ;
    Plane = mkTransport (mkN "Flugzeug" "Flugzeug" "Flugzeug" "Flugzeug" "Flugzeuge" "Flugzeuge" neuter) ;
    Subway = mkTransport (mkN "U-Bahn" "U-Bahn" "U-Bahn" "U-Bahn" "U-Bahnen" "U-Bahnen" feminine) ;
    Taxi = mkTransport (mkN "Taxi" "Taxi" "Taxi" "Taxi" "Taxis" "Taxis" neuter) ;
    Train = mkTransport (mkN "Zug" "Zug" "Zug" "Zug" "Z�ge" "Z�ge" masculine) ;
    Tram = mkTransport (mkN "Stra�enbahn" "Stra�enbahn" "Stra�enbahn" "Stra�enbahn" "Stra�enbahnen" "Stra�enbahnen" feminine) ;

    ByFoot = ParadigmsGer.mkAdv "zu Fu�" ;


    HowFar place = mkQS (mkQCl far_IAdv place.name) ;
    HowFarFrom x y = mkQS (mkQCl far_IAdv (mkNP y.name (SyntaxGer.mkAdv from_Prep x.name))) ;
    HowFarFromBy x y t = 
      mkQS (mkQCl far_IAdv (mkNP (mkNP y.name (SyntaxGer.mkAdv from_Prep x.name)) t)) ;
    HowFarBy y t = mkQS (mkQCl far_IAdv (mkNP y.name t)) ;
 
    WhichTranspPlace trans place = 
      mkQS (mkQCl (mkIP which_IDet trans.name) (mkVP (mkVP L.go_V) place.to)) ;

    IsTranspPlace trans place =
      mkQS (mkQCl (mkCl (mkCN trans.name place.to))) ;




  oper

    mkNat : Str -> Str -> Str -> {lang : NP ; prop : A ; country : NP} = \co, la, adj -> 
      {lang = mkNP (mkPN la) ; 
       prop = mkA adj ; country = mkNP (mkPN co)} ;
     
    mkDay : Str -> {name : NP ; point : Adv ; habitual : Adv} = \d ->
      let day = mkNP (mkPN d) in
      {name = day ; 
       point = SyntaxGer.mkAdv (mkPrep "am" dative) day ; ---- am 
       habitual = ParadigmsGer.mkAdv (d + "s") ----
      } ;

    mkPlace : N -> Prep -> Prep -> {name : CN ; at : Prep ; to : Prep} = \p,at,to -> {
      name = mkCN p ;
      at = at ;
      to = to
      } ;

    open_Adv = mkAdv "ge�ffnet" ;  ---- Adv to get right word order easily
    closed_Adv = mkAdv "geschlossen" ;

    xOf : GNumber -> N -> NPPerson -> NPPerson = \n,x,p -> mkRelative n (mkCN x) p ; 
  
    
    mkSuperl : A -> Det = \a -> SyntaxGer.mkDet the_Art (SyntaxGer.mkOrd a) ;


    mkTransport : N -> {name : CN ; by : Adv} = \n -> {
      name = mkCN n ; 
      by = SyntaxGer.mkAdv by8means_Prep (mkNP the_Det n)
      } ;

   far_IAdv = ss "wie weit" ** {lock_IAdv = <>} ; 
}