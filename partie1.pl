%nnf
nnf(not(and(C1,C2)),or(NC1,NC2)) :- nnf(not(C1),NC1),
                                   nnf(not(C2),NC2),!.
nnf(not(or(C1,C2)),and(NC1,NC2)) :- nnf(not(C1),NC1),
                                   nnf(not(C2),NC2),!.
nnf(not(all(R,C)),some(R,NC)) :- nnf(not(C),NC),!.
nnf(not(some(R,C)),all(R,NC)) :- nnf(not(C),NC),!.
nnf(not(not(X)),X) :- !.
nnf(not(X),not(X)) :- !.
nnf(and(C1,C2),and(NC1,NC2)) :- nnf(C1,NC1),nnf(C2,NC2),!.
nnf(or(C1,C2),or(NC1,NC2)) :- nnf(C1,NC1), nnf(C2,NC2),!.
nnf(some(R,C),some(R,NC)) :- nnf(C,NC),!.
nnf(all(R,C),all(R,NC)) :- nnf(C,NC),!.
nnf(X,X).

%-- Correction sémantique
testcnamea(X) :- setof(Y, cnamea(Y), L), member(X,L).
testcnamena(X) :- setof(Y, cnamena(Y), L), member(X,L).
testiname(X) :- setof(Y, iname(Y), L), member(X,L).
testrname(X) :- setof(Y, rname(Y), L), member(X,L).

%-- Concept
concept(X) :- testcnamea(X).
concept(not(X)) :- concept(X).
concept(and(X,Y)) :- concept(X), concept(Y).
concept(or(X,Y)) :- concept(X), concept(Y).
concept(some(R,X)) :- testrname(R), concept(X).
concept(all(R,X)) :- testrname(R), concept(X).

%-- Test si un concept est auto-référent
autoref(X,X,_,_).
autoref(X,A,[(X,_)|L],Lbase) :- autoref(X,A,L,Lbase).
autoref(X,A,[(Y,_)|L],Lbase) :- testcnamena(A), Y \= A,autoref(X,A,L,Lbase).
autoref(X,A,[(A,Y)|_],Lbase) :- testcnamena(A), autoref(X,Y,Lbase,Lbase).
autoref(X,not(A),_,Lbase) :- autoref(X,A,Lbase,Lbase).
autoref(X,and(A,B),_,Lbase) :-autoref(X,A,Lbase,Lbase); autoref(X,B,Lbase,Lbase).
autoref(X,or(A,B),_,Lbase) :- autoref(X,A,Lbase,Lbase); autoref(X,B,Lbase,Lbase).
autoref(X,some(_,A),_,Lbase) :- autoref(X,A,Lbase,Lbase).
autoref(X,all(_,A),_,Lbase) :- autoref(X,A,Lbase,Lbase).

% Remplacer Tbox
remplacerTbox(X,X,_) :- concept(X).
remplacerTbox(X,Y,[(X,Y)|_]).
remplacerTbox(X,Y,[(A,_)|LTbox]) :- 
  X\=A, remplacerTbox(X,Y,LTbox).
remplacerTbox(not(X),not(Y),LTbox) :- 
  remplacerTbox(X,Y,LTbox).
remplacerTbox(and(X1,X2),and(Y1,Y2),LTbox) :- 
  remplacerTbox(X1,Y1,LTbox),remplacerTbox(X2,Y2,LTbox),!.
remplacerTbox(or(X1,X2),or(Y1,Y2),LTbox) :- 
  remplacerTbox(X1,Y1,LTbox),remplacerTbox(X2,Y2,LTbox),!.
remplacerTbox(some(R,X),or(R,Y),LTbox) :- 
  remplacerTbox(X,Y,LTbox),!.
remplacerTbox(all(R,X),or(R,Y),LTbox) :- 
  remplacerTbox(X,Y,LTbox),!.

% Traitement Tbox
traitement_Tbox(X,Y,[(_,B)|LTBOX]) :- X\=B, traitement_Tbox(X,Y,LTBOX).
traitement_Tbox(X,Y,[(A,X)|_]) :- concept(X), equiv(A,X), nnf(X,Y).

% Traitement Abox
traitement_Abox(X,Y,_) :- concept(X), nnf(X,Y).
traitement_Abox(X,Y,LTbox) :- not(concept(X)), remplacerTbox(X,Y1,LTbox), nnf(Y1,Y).



