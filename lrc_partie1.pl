%nnf
nnf(not(and(C1,C2)),or(NC1,NC2)):- nnf(not(C1),NC1),
                                   nnf(not(C2),NC2),!.
nnf(not(or(C1,C2)),and(NC1,NC2)):- nnf(not(C1),NC1),
                                   nnf(not(C2),NC2),!.
nnf(not(all(R,C)),some(R,NC)):- nnf(not(C),NC),!.
nnf(not(some(R,C)),all(R,NC)):- nnf(not(C),NC),!.
nnf(not(not(X)),X):-!.
nnf(not(X),not(X)):-!.
nnf(and(C1,C2),and(NC1,NC2)):- nnf(C1,NC1),nnf(C2,NC2),!.
nnf(or(C1,C2),or(NC1,NC2)):- nnf(C1,NC1), nnf(C2,NC2),!.
nnf(some(R,C),some(R,NC)):- nnf(C,NC),!.
nnf(all(R,C),all(R,NC)) :- nnf(C,NC),!.
nnf(X,X).
%concat
concat([],L1,L1).
concat([X|Y],L1,[X|L2]) :- concat(Y,L1,L2).

%partie1
equiv(sculpteur,and(personne,some(aCree,sculpture))).
equiv(auteur,and(personne,some(aEcrit,livre))).
equiv(editeur,and(personne,and(not(some(aEcrit,livre)),some(aEdite
,livre)))).
equiv(parent,and(personne,some(aEnfant,anything))).
cnamea(personne).
cnamea(livre).
cnamea(objet).
cnamea(sculpture).
cnamea(anything).
cnamea(nothing).
cnamena(auteur).
cnamena(editeur).
cnamena(sculpteur).
cnamena(parent).
iname(michelAnge).
iname(david).
iname(sonnets).
iname(vinci).
iname(joconde).
rname(aCree).
rname(aEcrit).
rname(aEdite).
rname(aEnfant).
inst(michelAnge,personne).
inst(david,sculpture).
inst(sonnets,livre).
inst(vinci,personne).
inst(joconde,objet).
instR(michelAnge, david, aCree).
instR(michelAnge, sonnets, aEcrit).
instR(vinci, joconde, aCree).
tbox([(sculpteur,and(personne,some(aCree,sculpture))),
(auteur,and(personne,some(aEcrit,livre))),
(editeur,and(personne,and(not(some(aEcrit,livre)),some(aEdite,livre)))),
(parent,and(personne,some(aEnfant,anything)))]).
abox([(michelAnge,personne), (david,sculpture), (sonnets,livre),
(vinci,personne), (joconde,objet)]).
assert_role([(michelAnge, david, aCree), (michelAnge, sonnets, aEcrit),(vinci,
joconde, aCree)]).

%Correction sémantique
testcnamea(X):- setof(Y, cnamea(Y), L), member(X,L).
testcnamena(X):- setof(Y, cnamena(Y), L), member(X,L).
testiname(X):- setof(Y, iname(Y), L), member(X,L).
testrname(X):- setof(Y, rname(Y), L), member(X,L).
%Concept
concept_a(X):- testcnamea(X).
concept_a(not(X)):- concept_a(X),!.
concept_a(and(X,Y)):- concept_a(X), concept_a(Y),!.
concept_a(or(X,Y)):- concept_a(X), concept_a(Y),!.
concept_a(some(R,X)):- testrname(R), concept_a(X),!.
concept_a(all(R,X)):- testrname(R), concept_a(X),!.

%concept_na_a
concept(X):- testcnamea(X).
concept(X):- testcnamena(X).
concept(not(X)):- concept(X),!.
concept(and(X,Y)):- concept(X), concept(Y),!.
concept(or(X,Y)):- concept(X), concept(Y),!.
concept(some(R,X)):- testrname(R), concept(X),!.
concept(all(R,X)):- testrname(R), concept(X),!.

%Autoref
autoref(X,X,_,_).
autoref(X,A,[(X,_)|L],Lbase):- autoref(X,A,L,Lbase),!.
autoref(X,A,[(Y,_)|L],Lbase):- testcnamena(A), Y \= A,autoref(X,A,L,Lbase),!.
autoref(X,A,[(A,Y)|_],Lbase):- testcnamena(A), autoref(X,Y,Lbase,Lbase),!.
autoref(X,not(A),_,Lbase):- autoref(X,A,Lbase,Lbase),!.
autoref(X,and(A,B),_,Lbase):-autoref(X,A,Lbase,Lbase); autoref(X,B,Lbase,Lbase),!.
autoref(X,or(A,B),_,Lbase):- autoref(X,A,Lbase,Lbase); autoref(X,B,Lbase,Lbase),!.
autoref(X,some(_,A),_,Lbase):- autoref(X,A,Lbase,Lbase),!.
autoref(X,all(_,A),_,Lbase):- autoref(X,A,Lbase,Lbase),!.

%remplacer Tbox
remplacerTbox(X,X,_):-concept_a(X).
remplacerTbox(X,Y,[(X,Y)|_]).
remplacerTbox(X,Y,[(A,_)|LTbox]):- X\=A, remplacerTbox(X,Y,LTbox),!.
remplacerTbox(not(X),not(Y),LTbox):-remplacerTbox(X,Y,LTbox),!.
remplacerTbox(and(X1,X2),and(Y1,Y2),LTbox):-remplacerTbox(X1,Y1,LTbox),remplacerTbox(X2,Y2,LTbox),!.
remplacerTbox(or(X1,X2),or(Y1,Y2),LTbox):-remplacerTbox(X1,Y1,LTbox),remplacerTbox(X2,Y2,LTbox),!.
remplacerTbox(some(R,X),some(R,Y),LTbox):-remplacerTbox(X,Y,LTbox),!.
remplacerTbox(all(R,X),all(R,Y),LTbox):-remplacerTbox(X,Y,LTbox),!.

%traitement Tbox
traitement_Tbox_unite(X,Y,[(_,B)|LTBOX]):- X\=B, traitement_Tbox_unite(X,Y,LTBOX).
traitement_Tbox_unite(X,Y,[(A,X)|_]):- concept_a(X), equiv(A,X), nnf(X,Y).

traitement_Tbox_entiere([],[],_).
traitement_Tbox_entiere([(A,X)|L1],[(A,Y)|L2],LTBOX):-traitement_Tbox_unite(X, Y,LTBOX) , traitement_Tbox_entiere(L1,L2, LTBOX).

traitement_Tbox(Tbox):- tbox(X), traitement_Tbox_entiere(X, Tbox, X).

%traitement Abox_concept
traitement_Abox_concept_unite(X,Y,_):- concept_a(X), nnf(X,Y).
traitement_Abox_concept_unite(X,Y,LTbox):- concept(X), remplacerTbox(X,Y1,LTbox), nnf(Y1,Y).

traitement_Abox_concept_entiere([], [], _).
traitement_Abox_concept_entiere([(A,X)|L1],[(A,Y)|L2],LTBOX):-testiname(A), traitement_Abox_concept_unite(X, Y,LTBOX) , traitement_Abox_concept_entiere(L1,L2, LTBOX).

%traitement Abox_role
traitement_Abox_role([]).
traitement_Abox_role([(A, B, R)|L]):-testiname(A), testiname(B),testrname(R), traitement_Abox_role(L).

%traitement Abox
traitement_Abox(LC, LR):-abox(X), tbox(Y),assert_role(Z), traitement_Abox_concept_entiere(X, LC, Y), traitement_Abox_role(Z), concat([],Z,LR).

%est_type_1
est_type_1((A,X)):- iname(A), concept(X).

%acquisition_prop_type1
acquisition_prop_type1([],[],_).
acquisition_prop_type1([(A,C)|Abi],[(A,Y)|Abi1],Tbox):-est_type_1((A,C)), traitement_Abox_concept_unite(not(C),Y,Tbox), acquisition_prop_type1(Abi,Abi1,Tbox),!.
acquisition_prop_type1([X|Abi],[X|Abi1],Tbox):-not(est_type_1(X)), acquisition_prop_type1(Abi,Abi1,Tbox),!.

%est_type_2
est_type_2(and(C1,C2)):-concept(C1), concept(C2).

%acquisition_prop_type2
acquisition_prop_type2([],[],_).
acquisition_prop_type2([X|Abi],[(inst,Y)|Abi1],Tbox):-est_type_2(X),traitement_Abox_concept_unite(X,Y,Tbox), acquisition_prop_type2(Abi,Abi1,Tbox),!.
acquisition_prop_type2([X|Abi],[X|Abi1],Tbox):-not(est_type_2(X)), acquisition_prop_type2(Abi,Abi1,Tbox),!.

%premiere_etape
premiere_etape(Tbox, Abi, Abr):-traitement_Tbox(Tbox), traitement_Abox(Abi, Abr).
%deuxiem_etape
deuxieme_etape(Abi,Abi1,Tbox) :-
                saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).
%programme
programme :-
             premiere_etape(Tbox,Abi,Abr),
             deuxieme_etape(Abi,Abi1,Tbox),
             troisieme_etape(Abi1,Abr).
%troisieme_etape
troisieme_etape(Abi,Abr) :-
                            tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),
                            resolution(Lie,Lpt,Li,Lu,Ls,Abr),
                            nl,write('Youpiiiiii, on a demontre la
                            proposition initiale !!!').

saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox) :-
nl,write('Entrez le numero du type de proposition que vous voulez demontrer :'),nl,
write('1 Une instance donnee appartient a un concept donne.'),nl, write('2 Deux concepts n"ont pas d"elements en commun(ils ont une intersection vide).'),nl, read(R), suite(R,Abi,Abi1,Tbox).
suite(1,Abi,Abi1,Tbox) :-
    acquisition_prop_type1(Abi,Abi1,Tbox),!.
suite(2,Abi,Abi1,Tbox) :-
    acquisition_prop_type2(Abi,Abi1,Tbox),!.
suite(R,Abi,Abi1,Tbox) :-R\=1, R\=2,nl,write('Cette reponse est incorrecte.'),nl,
    saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).
                                                                 
%tri_Abox
tri_Abox([],_,_,_,_,_).
tri_Abox([(I,some(R,C))|Abi],[(I,some(R,C))|Lie],Lpt,Li,Lu,Ls):-tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),!.
tri_Abox([(I,all(R,C))|Abi],Lie,[(I,all(R,C))|Lpt],Li,Lu,Ls):-tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),!.
tri_Abox([(I,and(C1,C2))|Abi],Lie,Lpt,[(I,and(C1,C2))|Li],Lu,Ls):-tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),!.
tri_Abox([(I,or(C1,C2))|Abi],Lie,Lpt,Li,[(I,or(C1,C2))|Lu],Ls):-tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),!.
tri_Abox([(I,C)|Abi],Lie,Lpt,Li,Lu,[(I,C)|Ls]):-testcnamea(C), tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),!.
tri_Abox([(I,not(C))|Abi],Lie,Lpt,Li,Lu,[(I,not(C))|Ls]):-testcnamea(C), tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),!.

%genere
compteur(1).
genere(Nom) :- compteur(V),nombre(V,L1),concat([105,110,115,116],L1,L2),
V1 is V+1,
dynamic(compteur/1),
retract(compteur(V)),
dynamic(compteur/1),
assert(compteur(V1)),nl,nl,nl,
name(Nom,L2).
nombre(0,[]).
nombre(X,L1) :-R is (X mod 10),
 Q is ((X-R)//10),
 chiffre_car(R,R1),
 char_code(R1,R2),
 nombre(Q,L),
 concat(L,[R2],L1).
chiffre_car(0,'0').
chiffre_car(1,'1').
chiffre_car(2,'2').
chiffre_car(3,'3').
chiffre_car(4,'4').
chiffre_car(5,'5').
chiffre_car(6,'6').
chiffre_car(7,'7').
chiffre_car(8,'8').
chiffre_car(9,'9').

%chercher_Abox_role
chercher_Abox_role(_,_,_,[]).
chercher_Abox_role(A,R,[B|L],[(A,B,R)|Abr]):-chercher_Abox_role(A,R,L,Abr).
chercher_Abox_role(A,R,L,[(A1,_,R1)|Abr]):-(A\=A1;R\=R1),chercher_Abox_role(A,R,L,Abr).
%ajouter_deduction_all
ajouter_deduction_all([],_,_).
ajouter_deduction_all([B|ListeB],C,[(B,C)|Ls]):-ajouter_deduction_all(ListeB,C,Ls).


%complete_some
complete_some([],_,_,_,_,_).
complete_some([(I,some(R,C))|Lie],Lpt,Li,Lu,[(B,C)|Ls],[(I,B,R)|Abr]):-genere(B),complete_some(Lie,Lpt,Li,Lu,Ls,Abr),!.

%transformation_and
transformation_and(_,_,[],_,_,_).
transformation_and(Lie,Lpt,[(I,and(C1,C2))|Li],Lu,[(I,C1),(I,C2)|Ls],Abr):-transformation_and(Lie,Lpt,Li,Lu,Ls,Abr),!.

%deduction_all
deduction_all(_,[],_,_,_,_).
deduction_all(Lie,[(I,all(R,C))|Lpt],Li,Lu,[L|Ls],Abr):-chercher_Abox_role(I,R,ListeB,Abr),ajouter_deduction_all(ListeB,C,L),deduction_all(Lie,Lpt,Li,Lu,Ls,Abr),!.

%transformation_or
transformation_or(_,_,_,[],_,_).
transformation_or(Lie,Lpt,Li,[(I,or(C1,_))|Lu],[(I,C1)|Ls],Abr):-transformation_or(Lie,Lpt,Li,Lu,Ls,Abr).
transformation_or(Lie,Lpt,Li,[(I,or(_,C2))|Lu],[(I,C2)|Ls],Abr):-transformation_or(Lie,Lpt,Li,Lu,Ls,Abr),!.

%test_clash_unite
test_clash_unite((I,C),[(I,not(C))|_]):-!.
test_clash_unite((I,not(C)),[(I,C)|_]):-!.
test_clash_unite((I,C),[(I1,X)|Li]):- (I\=I1;(C\=not(X);X\=not(C))),test_clash_unite((I,C),Li),!.

%test_no_clash
test_no_clash([],_).
test_no_clash([X|L], Ls):- not(test_clash_unite(X,Ls)), test_no_clash(L,Ls),!.

%resolution
resolution(Lie,Lpt,Li,Lu,Ls,Abr):-complete_some(Lie,Lpt,Li,Lu,Ls,Abr).

%evolue
evolue([], _, _, _, _, _, _, _, _, _, _).
evolue((I,some(R,C)),Lie,_,_,_,_,[(I,some(R,C))|Lie], _, _, _, _).
evolue((I,all(R,C)),_,Lpt,_,_,_,_, [(I,all(R,C))|Lpt], _, _, _).
evolue((I,and(C1,C2)),_,_,Li,_,_,_, _, [(I,and(C1,C2))|Li], _, _).
evolue((I,or(C1,C2)),_,_,_,Lu,_,_, _, _, [(I,or(C1,C2))|Lu], _).
evolue((I,C),_,_,_,_,Ls,_, _, _, _, [(I,C)|Ls]):-testcnamea(C).
evolue((I,not(C)),_,_,_,_,Ls,_, _, _, _, [(I,not(C))|Ls]):-testcnamea(C).
                                         
