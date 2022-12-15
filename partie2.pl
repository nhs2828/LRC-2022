/* Partie 2 - Saisie de la proposition à démontrer */
deuxieme_etape(Abi, Abi1, Tbox) :-
  saisie_et_traitement_prop_a_demontrer(Abi, Abi1, Tbox).

saisie_et_traitement_prop_a_demontrer(Abi, Abi1, Tbox) :-
  nl,
  write('Entrer le numero du type de proposition que l\'on souhaite demontrer :'),
  nl,
  write('\t1 - Une instance donnee appartient à un concept donne.'),
  nl,
  write('\t2 - Deux concepts n\'ont pas d\'elements en commun (ils ont une intersection vide).'),
  nl,
  read(R),
  suite(R, Abi, Abi1, Tbox).

suite(1, Abi, Abi1, Tbox) :-
    acquisition_prop_type1(Abi, Abi1, Tbox),
    !.
suite(2, Abi, Abi1, Tbox) :-
    acquisition_prop_type2(Abi, Abi1, Tbox),
    !.
suite(R, Abi, Abi1, Tbox) :-
    R\=1,
    R\=2,
    nl,
    write('Cette reponse est incorrecte.'),
    nl,
    saisie_et_traitement_prop_a_demontrer(Abi, Abi1, Tbox).

/*-----------------------------------------------------------
Saisie d'une proposition de type 1 (I:C).
-----------------------------------------------------------*/
%-- Vérifie que la saisie est bien de type 1
est_type_1((A, X)) :-
  iname(A),
  concept(X).

acquisition_prop_type1(Abi, [(A, Y)|Abi], Tbox) :-
  nl,
  write("Veuillez saisir une instance I :"),
  nl,
  read(A),
  nl,
  write("Veuillez saisir un concept C :"),
  nl,
  read(C),
  est_type_1((A, C)),
  traitement_Abox_concept_unite(not(C), Y, Tbox),
  !.

/*-----------------------------------------------------------
Saisie d'une proposition de type 2 (C1 et C2).
-----------------------------------------------------------*/
%-- Vérifie que la saisie est bien de type 2
est_type_2(and(C1, C2)) :-
  concept(C1),
  concept(C2).

acquisition_prop_type2(Abi, [(B, Y)|Abi], Tbox) :-
  nl,
  write("Veuillez saisir un concept C1 :"),
  nl,
  read(C1),
  nl,
  write("Veuillez saisir un concept C2 :"),
  nl,
  read(C2),
  est_type_2(and(C1, C2)),
  genere(B),
  traitement_Abox_concept_unite(and(C1, C2), Y, Tbox),
  !.

