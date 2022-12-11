deuxieme_etape(Abi, Abi1, Tbox) :-
    saisie_et_traitement_prop_a_demontrer(Abi, Abi1, Tbox).

saisie_et_traitement_prop_a_demontrer(Abi, Abi1, Tbox) :-
    nl,
    write('Entrer le numéro du type de proposition que l\'on souhaite démontrer :'),
    nl,
    write('\t1 - Une instance donnée appartient à un concept donné.'),
    nl,
    write('\t2 - Deux concepts n\'ont pas d\'éléments en commun (ils ont une intersection vide).'),
    nl,
    read(R),
    suite(R, Abi, Abi1, Tbox).

suite(1, Abi, Abi1, Tbox) :-
    acquisition_prop_type1(Abi, Abi1, Tbox),!.
suite(2,Abi,Abi1,Tbox) :- acquisition_prop_type2(Abi,Abi1,Tbox),!.
suite(R,Abi,Abi1,Tbox) :- 
  nl,write('Cette réponse est incorrecte.'),
  nl,saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

%est_type_1
est_type_1((A, X)) :-
  iname(A),
  concept_na_a(X).

%acquisition_prop_type1
acquisition_prop_type1([], [], _).
acquisition_prop_type1([(A, C)|Abi], [(A, Y)|Abi1], Tbox) :-
  est_type_1((A, C)),
  traitement_Abox_concept_unite(not(C), Y, Tbox),
  acquisition_prop_type1(Abi, Abi1, Tbox).
acquisition_prop_type1([X|Abi], [X|Abi1], Tbox) :-
  not(est_type_1(X)),
  acquisition_prop_type1(Abi, Abi1, Tbox).

%est_type_2
est_type_2(and(C1, C2)) :-
  concept_na_a(C1),
  concept_na_a(C2).

%acquisition_prop_type2
acquisition_prop_type2([], [], _).
acquisition_prop_type2([X|Abi], [(inst, Y)|Abi1], Tbox) :-
  est_type_2(X),
  traitement_Abox_concept_unite(X, Y, Tbox),
  acquisition_prop_type2(Abi, Abi1, Tbox).
acquisition_prop_type2([X|Abi], [X|Abi1], Tbox) :-
  not(est_type_2(X)),
  acquisition_prop_type2(Abi, Abi1, Tbox).

