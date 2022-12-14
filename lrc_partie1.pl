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
tBox(Tbox) :- setof((X, Y), equiv(X, Y), Tbox).
aBox(Abi) :- setof((X, Y), inst(X, Y), Abi).
assert_Role(Abr) :- setof((X, Y, Z), instR(X, Y, Z), Abr).

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
traitement_Tbox_entiere([],[]).
traitement_Tbox_entiere([(A,X)|L1],[(A,Y)|L2]):-testcnamena(A), concept(X), equiv(A,X), nnf(X,Y) , traitement_Tbox_entiere(L1,L2),!.

traitement_Tbox(Tbox):- tBox(X), traitement_Tbox_entiere(X, Tbox).

%traitement Abox_concept
traitement_Abox_concept_unite(X,Y,_):- concept_a(X), nnf(X,Y).
traitement_Abox_concept_unite(X,Y,LTbox):- concept(X), remplacerTbox(X,Y1,LTbox), nnf(Y1,Y),!.

traitement_Abox_concept_entiere([], [], _).
traitement_Abox_concept_entiere([(A,X)|L1],[(A,Y)|L2],LTBOX):-testiname(A), traitement_Abox_concept_unite(X, Y,LTBOX) , traitement_Abox_concept_entiere(L1,L2, LTBOX),!.

%traitement Abox_role
traitement_Abox_role([]).
traitement_Abox_role([(A, B, R)|L]):-testiname(A), testiname(B),testrname(R), traitement_Abox_role(L),!.

%traitement Abox
traitement_Abox(LC, LR):-aBox(X), tBox(Y),assert_Role(Z), traitement_Abox_concept_entiere(X, LC, Y), traitement_Abox_role(Z), concat([],Z,LR),!.

%est_type_1
est_type_1((A,X)):- iname(A), concept(X).

%acquisition_prop_type1
acquisition_prop_type1(Abi,[(A,Y)|Abi],Tbox):-nl,write("entrez I."),nl,read(A),nl,write("Entrez C"),nl,read(C),
    		est_type_1((A,C)), traitement_Abox_concept_unite(not(C),Y,Tbox),!.

%est_type_2
est_type_2(and(C1,C2)):-concept(C1), concept(C2).

%acquisition_prop_type2
acquisition_prop_type2(Abi,[(B,Y)|Abi],Tbox):-nl,write("entrez C1."),nl,read(C1),nl,write("Entrez C2"),nl,read(C2),
    est_type_2(and(C1,C2)),genere(B), traitement_Abox_concept_unite(and(C1,C2),Y,Tbox),!.

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
tri_Abox([],[],[],[],[],[]).
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
complete_some([(I,some(R,C))|Lie],Lpt,Li,Lu,Ls,Abr):-genere(B),
    A=(B,C),
    evolue(A, Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    affiche_evolution_Abox(Ls, [(I,some(R,C))|Lie], Lpt, Li, Lu, Abr, Ls1, Lie1, Lpt1, Li1, Lu1, [(I,B,R)|Abr]),
    resolution(Lie1,Lpt1,Li1,Lu1,Ls1,[(I,B,R)|Abr]).
    
%transformation_and
transformation_and(Lie,Lpt,[(I,and(C1,C2))|Li],Lu,Ls,Abr):-
    A1=(I,C1),
    evolue(A1, Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    A2=(I,C2),
	evolue(A2, Lie1, Lpt1, Li1, Lu1, Ls1, Lie2, Lpt2, Li2, Lu2, Ls2),
    affiche_evolution_Abox(Ls, Lie, Lpt, [(I,and(C1,C2))|Li], Lu, Abr, Ls2, Lie2, Lpt2, Li2, Lu2, Abr),
    resolution(Lie2,Lpt2,Li2,Lu2,Ls2,Abr).
    
%deduction_all
deduction_all(Lie,[(I,all(R,C))|Lpt],Li,Lu,Ls,Abr):-chercher_Abox_role(I,R,ListeB,Abr),ajouter_deduction_all(ListeB,C,ListBC),
    evolue_pourtout(ListBC,Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    affiche_evolution_Abox(Ls, Lie, [(I,all(R,C))|Lpt], Li, Lu, Abr, Ls1, Lie1, Lpt1, Li1, Lu1, Abr),
    resolution(Lie1,Lpt1,Li1,Lu1,Ls1,Abr).

%transformation_or
transformation_or(Lie,Lpt,Li,[(I,or(C1,C2))|Lu],Ls,Abr):-
    A=(I,C1),
    evolue(A, Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    affiche_evolution_Abox(Ls, Lie, Lpt, Li, [(I,or(C1,C2))|Lu], Abr, Ls1, Lie1, Lpt1, Li1, Lu1, Abr),
    resolution(Lie1,Lpt1,Li1,Lu1,Ls1,Abr).

transformation_or(Lie,Lpt,Li,[(I,or(C1,C2))|Lu],Ls,Abr):-
	A=(I,C2),
    evolue(A, Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    affiche_evolution_Abox(Ls, Lie, Lpt, Li, [(I,or(C1,C2))|Lu], Abr, Ls1, Lie1, Lpt1, Li1, Lu1, Abr),
    resolution(Lie1,Lpt1,Li1,Lu1,Ls1,Abr).

%test_clash_unite
test_clash_unite((I,C),[(I,not(C))|Li]):-!.
test_clash_unite((I,not(C)),[(I,C)|Li]):-!.
test_clash_unite((I,C),[(I1,X)|Li]):- nnf(not(C),NewC),(I\=I1;NewC\=X),test_clash_unite((I,C),Li),!.

%test_no_clash
test_no_clash([],_).
test_no_clash([X|L], Ls):- not(test_clash_unite(X,Ls)), test_no_clash(L,Ls),!.


%resolution
resolution([],[],[],[],Ls,Abr):-not(test_no_clash(Ls,Ls)),!.
resolution(Lie,Lpt,Li,Lu,Ls,Abr):-test_no_clash(Ls,Ls),complete_some(Lie,Lpt,Li,Lu,Ls,Abr).
resolution([],Lpt,Li,Lu,Ls,Abr):-test_no_clash(Ls,Ls),transformation_and([],Lpt,Li,Lu,Ls,Abr).
resolution([],Lpt,[],Lu,Ls,Abr):-test_no_clash(Ls,Ls),
    deduction_all([],Lpt,[],Lu,Ls,Abr).
resolution([],[],[],Lu,Ls,Abr):-test_no_clash(Ls,Ls),transformation_or([],[],[],Lu,Ls,Abr).

%evolue
evolue_pourtout([], _, _, _, _, _, _, _, _, _, _).
evolue_pourtout([(I,some(R,C))|L],Lie, Lpt, Li, Lu, Ls, [(I,some(R,C))|Lie], Lpt1, Li1, Lu1, Ls1):-
    evolue_pourtout(L,Lie, Lpt, Li, Lu, Ls, Lie, Lpt1, Li1, Lu1, Ls1),!.
evolue_pourtout([(I,all(R,C))|L],Lie, Lpt, Li, Lu, Ls, Lie1, [(I,all(R,C))|Lpt], Li1, Lu1, Ls1):-
    evolue_pourtout(L,Lie, Lpt, Li, Lu, Ls, Lie1, Lpt, Li1, Lu1, Ls1),!.
evolue_pourtout([(I,and(C1,C2))|L],Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, [(I,and(C1,C2))|Li], Lu1, Ls1):-
    evolue_pourtout(L,Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li, Lu1, Ls1),!.
evolue_pourtout([(I,or(C1,C2))|L],Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, [(I,or(C1,C2))|Lu], Ls1):-
    evolue_pourtout(L,Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu, Ls1),!.
evolue_pourtout([(I,C)|L],Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, [(I,C)|Ls]):-
    evolue_pourtout(L,Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls),!.
evolue_pourtout([(I,not(C))|L],Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, [(I,not(C))|Ls]):-
    evolue_pourtout(L,Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls),!.
    
evolue((I,some(R,C)),Lie,Lpt,Li,Lu,Ls,[(I,some(R,C))|Lie], Lpt, Li,Lu, Ls).
evolue((I,all(R,C)),Lie,Lpt,Li,Lu,Ls,Lie, [(I,all(R,C))|Lpt], Li, Lu, Ls).
evolue((I,and(C1,C2)),Lie,Lpt,Li,Lu,Ls,Lie, Lpt, [(I,and(C1,C2))|Li], Lu, Ls).
evolue((I,or(C1,C2)),Lie,Lpt,Li,Lu,Ls,Lie, Lpt, Li, [(I,or(C1,C2))|Lu], Ls).
evolue((I,C),Lie,Lpt,Li,Lu,Ls,Lie, Lpt, Li, Lu, [(I,C)|Ls]):-testcnamea(C).
evolue((I,not(C)),Lie,Lpt,Li,Lu,Ls,Lie, Lpt, Li, Lu, [(I,not(C))|Ls]):-testcnamea(C).

%affiche
affiche([]).

affiche([X|L]) :- affiche(X), affiche(L).
affiche((I,some(R,C))) :- 
    nl,affiche(I),write(': some.'), affiche(R), write('.'),affiche(C),!.

affiche((I,all(R,C))) :-  
    nl,affiche(I), write(': all.'), affiche(R), write('.'),affiche(C),!.

affiche((I, and(C,D))) :- 
    nl,affiche(I), write(': '), affiche(C), write(' and '), affiche(D),!.

affiche((I,or(C,D))) :- 
    nl,affiche(I), write(': ') , affiche(C), write(' or '), affiche(D),!.

affiche((not(C))) :- 
    write('not '), affiche(C).

affiche((I,C,R)) :- 
    nl, write('<'), affiche(I), write(', '), affiche(C), write('>: '), affiche(R),!.

affiche((I,C)) :- 
    nl,affiche(I), write(': ') , affiche(C),!.

affiche(some(R,C)) :- 
    write(' some.'), affiche(R),write('.'),affiche(C),!.

affiche(all(R,C)) :- 
    write('all.'), affiche(R),write('.'),affiche(C),!.

affiche(and(C1,C2)) :- 
    affiche(C1), write(' and '), affiche(C2),!.

affiche(or(C1,C2)) :- 
    affiche(C1), write(' or '), affiche(C2),!.
affiche(C) :- write(C).

%affiche_evolution_Abox
affiche_evolution_Abox(Ls1, Lie1, Lpt1, Li1, Lu1, Abr1, Ls2, Lie2, Lpt2, Li2, Lu2, Abr2):-
    nl,write('---------Depart----------'),
    nl, write('Lie:'),
    affiche(Lie1),
    nl, write('Lpt:'),
    affiche(Lpt1),
    nl, write('Li:'),
    affiche(Li1),
    nl, write('Lu:'),
    affiche(Lu1),
    nl, write('Ls:'),
    affiche(Ls1),
    nl, write('Abr:'), 
    affiche(Abr1),
    nl,write('---------Arrive----------'),
    nl, write('Lie:'),
    affiche(Lie2),
    nl, write('Lpt:'),
    affiche(Lpt2),
    nl, write('Li:'),
    affiche(Li2),
    nl, write('Lu:'),
    affiche(Lu2),
    nl, write('Ls:'),
    affiche(Ls2),
    nl, write('Abr:'), 
    affiche(Abr2),!.
    
