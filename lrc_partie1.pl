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
concept(X):- testcnamea(X).
concept(not(X)):- concept(X).
concept(and(X,Y)):- concept(X), concept(Y).
concept(or(X,Y)):- concept(X), concept(Y).
concept(some(R,X)):- testrname(R), concept(X).
concept(all(R,X)):- testrname(R), concept(X).

%concept_na_a
concept_na_a(X):- testcnamea(X).
concept_na_a(X):- testcnamena(X).
concept_na_a(not(X)):- concept_na_a(X).
concept_na_a(and(X,Y)):- concept_na_a(X), concept_na_a(Y).
concept_na_a(or(X,Y)):- concept_na_a(X), concept_na_a(Y).
concept_na_a(some(R,X)):- testrname(R), concept_na_a(X).
concept_na_a(all(R,X)):- testrname(R), concept_na_a(X).

%Autoref
autoref(X,X,_,_).
autoref(X,A,[(X,_)|L],Lbase):- autoref(X,A,L,Lbase).
autoref(X,A,[(Y,_)|L],Lbase):- testcnamena(A), Y \= A,autoref(X,A,L,Lbase).
autoref(X,A,[(A,Y)|_],Lbase):- testcnamena(A), autoref(X,Y,Lbase,Lbase).
autoref(X,not(A),_,Lbase):- autoref(X,A,Lbase,Lbase).
autoref(X,and(A,B),_,Lbase):-autoref(X,A,Lbase,Lbase); autoref(X,B,Lbase,Lbase).
autoref(X,or(A,B),_,Lbase):- autoref(X,A,Lbase,Lbase); autoref(X,B,Lbase,Lbase).
autoref(X,some(_,A),_,Lbase):- autoref(X,A,Lbase,Lbase).
autoref(X,all(_,A),_,Lbase):- autoref(X,A,Lbase,Lbase).

%remplacer Tbox
remplacerTbox(X,X,_):-concept(X).
remplacerTbox(X,Y,[(X,Y)|_]).
remplacerTbox(X,Y,[(A,_)|LTbox]):- X\=A, remplacerTbox(X,Y,LTbox).
remplacerTbox(not(X),not(Y),LTbox):-remplacerTbox(X,Y,LTbox).
remplacerTbox(and(X1,X2),and(Y1,Y2),LTbox):-remplacerTbox(X1,Y1,LTbox),remplacerTbox(X2,Y2,LTbox),!.
remplacerTbox(or(X1,X2),or(Y1,Y2),LTbox):-remplacerTbox(X1,Y1,LTbox),remplacerTbox(X2,Y2,LTbox),!.
remplacerTbox(some(R,X),some(R,Y),LTbox):-remplacerTbox(X,Y,LTbox),!.
remplacerTbox(all(R,X),all(R,Y),LTbox):-remplacerTbox(X,Y,LTbox),!.

%traitement Tbox
traitement_Tbox_unite(X,Y,[(_,B)|LTBOX]):- X\=B, traitement_Tbox_unite(X,Y,LTBOX).
traitement_Tbox_unite(X,Y,[(A,X)|_]):- concept(X), equiv(A,X), nnf(X,Y).

traitement_Tbox_entiere([],[],_).
traitement_Tbox_entiere([(A,X)|L1],[(A,Y)|L2],LTBOX):-traitement_Tbox_unite(X, Y,LTBOX) , traitement_Tbox_entiere(L1,L2, LTBOX).

traitement_Tbox(Tbox):- tbox(X), traitement_Tbox_entiere(X, Tbox, X).

%traitement Abox_concept
traitement_Abox_concept_unite(X,Y,_):- concept(X), nnf(X,Y).
traitement_Abox_concept_unite(X,Y,LTbox):- concept_na_a(X), remplacerTbox(X,Y1,LTbox), nnf(Y1,Y).

traitement_Abox_concept_entiere([], [], _).
traitement_Abox_concept_entiere([(A,X)|L1],[(A,Y)|L2],LTBOX):-testiname(A), traitement_Abox_concept_unite(X, Y,LTBOX) , traitement_Abox_concept_entiere(L1,L2, LTBOX).

%traitement Abox_role
traitement_Abox_role([]).
traitement_Abox_role([(A, B, R)|L]):-testiname(A), testiname(B),testrname(R), traitement_Abox_role(L).

%traitement Abox
traitement_Abox(LC, LR):-abox(X), tbox(Y),assert_role(Z), traitement_Abox_concept_entiere(X, LC, Y), traitement_Abox_role(Z), concat([],Z,LR).

%est_type_1
est_type_1((A,X)):- iname(A), concept_na_a(X).

%acquisition_prop_type1
acquisition_prop_type1([],[],_).
acquisition_prop_type1([(A,C)|Abi],[(A,Y)|Abi1],Tbox):-est_type_1((A,C)), traitement_Abox_concept_unite(not(C),Y,Tbox), acquisition_prop_type1(Abi,Abi1,Tbox).
acquisition_prop_type1([X|Abi],[X|Abi1],Tbox):-not(est_type_1(X)), acquisition_prop_type1(Abi,Abi1,Tbox).

%est_type_2
est_type_2(subs(and(C1,C2),anthing)):-concept_na_a(C1), concept_na_a(C2).

%acquisition_prop_type2
acquisition_prop_type2([],[],_).
acquisition_prop_type2([subs(and(C1,C2),anything)|Abi],[(inst,Y)|Abi1],Tbox):-concept_na_a(C1), concept_na_a(C2),traitement_Abox_unite(and(C1,C2),Y,Tbox), acquisition_prop_type2(Abi,Abi1,Tbox).
acquisition_prop_type2([X|Abi],[X|Abi1],Tbox):-not(est_type_2(X)), acquisition_prop_type2(Abi,Abi1,Tbox).

%premiere_etape
premiere_etape(Tbox, Abi, Abr):-traitement_Tbox(Tbox), traitement_Abox(Abi, Abr).

%tri_Abox
tri_Abox([],_,_,_,_,_).
tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls):-

                           
                                         
                                         
                                         
