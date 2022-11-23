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
assert_role([(michelAnge, david, aCree), (michelAnge, sonnet, aEcrit),(vinci,
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



