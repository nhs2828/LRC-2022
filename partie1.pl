%premiere_etape
premiere_etape(Tbox, Abi, Abr) :-
  traitement_Tbox(Tbox),
  traitement_Abox(Abi, Abr).

%-- Correction sémantique
/*-----------------------------------------------------------
chercher_Abox_role/4
-----------------------------------------------------------*/
testcnamea(X) :-
    setof(Y, cnamea(Y), L),
    member(X, L).
testcnamena(X) :-
    setof(Y, cnamena(Y), L),
    member(X, L).
testiname(X) :-
    setof(Y, iname(Y), L),
    member(X, L).
testrname(X) :-
    setof(Y, rname(Y), L),
    member(X, L).

/*-----------------------------------------------------------
concept/1
-----------------------------------------------------------*/
concept(X) :-
    testcnamea(X).
concept(not(X)) :-
    concept(X).
concept(and(X, Y)) :-
    concept(X),
    concept(Y).
concept(or(X, Y)) :-
    concept(X),
    concept(Y).
concept(some(R, X)) :-
    testrname(R),
    concept(X).
concept(all(R, X)) :-
    testrname(R),
    concept(X).

/*-----------------------------------------------------------
concept_na_a/1
-----------------------------------------------------------*/
concept_na_a(X) :-
  testcnamea(X).
concept_na_a(X) :-
  testcnamena(X).
concept_na_a(not(X)) :-
  concept_na_a(X).
concept_na_a(and(X, Y)) :-
  concept_na_a(X),
  concept_na_a(Y).
concept_na_a(or(X, Y)) :-
  concept_na_a(X),
  concept_na_a(Y).
concept_na_a(some(R, X)) :-
  testrname(R),
  concept_na_a(X).
concept_na_a(all(R, X)) :-
  testrname(R),
  concept_na_a(X).

/*-----------------------------------------------------------
autoref
Test si un concept est auto-référent
-----------------------------------------------------------*/
autoref(X, X, _, _).
autoref(X, A, [(X, _)|L], Lbase) :-
    autoref(X, A, L, Lbase).
autoref(X, A, [(Y, _)|L], Lbase) :-
    testcnamena(A),
    Y\=A,
    autoref(X, A, L, Lbase).
autoref(X, A, [(A, Y)|_], Lbase) :-
    testcnamena(A),
    autoref(X, Y, Lbase, Lbase).
autoref(X, not(A), _, Lbase) :-
    autoref(X, A, Lbase, Lbase).
autoref(X, and(A, B), _, Lbase) :-
    (   autoref(X, A, Lbase, Lbase)
    ;   autoref(X, B, Lbase, Lbase)
    ).
autoref(X, or(A, B), _, Lbase) :-
    (   autoref(X, A, Lbase, Lbase)
    ;   autoref(X, B, Lbase, Lbase)
    ).
autoref(X, some(_, A), _, Lbase) :-
    autoref(X, A, Lbase, Lbase).
autoref(X, all(_, A), _, Lbase) :-
    autoref(X, A, Lbase, Lbase).


/*-----------------------------------------------------------
% Remplacer Tbox
-----------------------------------------------------------*/
remplacerTbox(X, X, _) :-
    concept(X).
remplacerTbox(X, Y, [(X, Y)|_]).
remplacerTbox(X, Y, [(A, _)|LTbox]) :-
    X\=A,
    remplacerTbox(X, Y, LTbox).
remplacerTbox(not(X), not(Y), LTbox) :-
    remplacerTbox(X, Y, LTbox).
remplacerTbox(and(X1, X2), and(Y1, Y2), LTbox) :-
    remplacerTbox(X1, Y1, LTbox),
    remplacerTbox(X2, Y2, LTbox),
    !.
remplacerTbox(or(X1, X2), or(Y1, Y2), LTbox) :-
    remplacerTbox(X1, Y1, LTbox),
    remplacerTbox(X2, Y2, LTbox),
    !.
remplacerTbox(some(R, X), or(R, Y), LTbox) :-
    remplacerTbox(X, Y, LTbox),
    !.
remplacerTbox(all(R, X), or(R, Y), LTbox) :-
    remplacerTbox(X, Y, LTbox),
    !.


/*-----------------------------------------------------------
Traitement Tbox
-----------------------------------------------------------*/
traitement_Tbox_unite(X, Y, [(_, B)|LTBOX]) :-
    X\=B,
    traitement_Tbox_unite(X, Y, LTBOX).
traitement_Tbox_unite(X, Y, [(A, X)|_]) :-
    concept(X),
    equiv(A, X),
    nnf(X, Y).

traitement_Tbox_entiere([], [], _).
traitement_Tbox_entiere([(A, X)|L1], [(A, Y)|L2], LTBOX) :-
    traitement_Tbox_unite(X, Y, LTBOX),
    traitement_Tbox_entiere(L1, L2, LTBOX).

traitement_Tbox(Tbox) :-
    tbox(X),
    traitement_Tbox_entiere(X, Tbox, X).

/*-----------------------------------------------------------
Traitement Abox
-----------------------------------------------------------*/
%traitement Abox_concept
traitement_Abox_concept_unite(X, Y, _) :-
  concept(X),
  nnf(X, Y).
traitement_Abox_concept_unite(X, Y, LTbox) :-
  concept_na_a(X),
  remplacerTbox(X, Y1, LTbox),
  nnf(Y1, Y).

traitement_Abox_concept_entiere([], [], _).
traitement_Abox_concept_entiere([(A, X)|L1], [(A, Y)|L2], LTBOX) :-
  testiname(A),
  traitement_Abox_concept_unite(X, Y, LTBOX),
  traitement_Abox_concept_entiere(L1, L2, LTBOX).

%traitement Abox_role
traitement_Abox_role([]).
traitement_Abox_role([(A, B, R)|L]) :-
  testiname(A),
  testiname(B),
  testrname(R),
  traitement_Abox_role(L).

%traitement Abox
traitement_Abox(LC, LR) :-
  abox(X),
  tbox(Y),
  assert_role(Z),
  traitement_Abox_concept_entiere(X, LC, Y),
  traitement_Abox_role(Z),
  concat([], Z, LR).



