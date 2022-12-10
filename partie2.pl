:- [partie1.pl].

deuxieme_etape(Abi,Abi1,Tbox) :- saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox) :-
  nl,write('Entrer le numéro du type de proposition que l\'on souhaite démontrer :'),
  nl,write('\t1 - Une instance donnée appartient à un concept donné.'),
  nl,write('\t2 - Deux concepts n\'ont pas d\'éléments en commun (ils ont une intersection vide).'),
  nl,read(R),
  suite(R,Abi,Abi1,Tbox).

suite(1,Abi,Abi1,Tbox) :- acquisition_prop_type1(Abi,Abi1,Tbox),!.
suite(2,Abi,Abi1,Tbox) :- acquisition_prop_type2(Abi,Abi1,Tbox),!.
suite(R,Abi,Abi1,Tbox) :- 
  nl,write('Cette réponse est inccorecte.'),
  nl,saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

%acquisition_prop_type1
acquisition_prop_type1([],[],_).
acquisition_prop_type1([X|Abi],[Y|Abi1],Tbox):-traitement_Abox(not(X),Y,Tbox), acquisition_prop_type1(Abi,Abi1,Tbox).

%acquisition_prop_type2
acquisition_prop_type2([],[],_).
acquisition_prop_type2([and(X1,X2)|Abi],[Y|Abi1],Tbox):-traitement_Abox(and(X1,X2),Y,Tbox), acquisition_prop_type2(Abi,Abi1,Tbox).
