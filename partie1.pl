/* Partie 1 - Etape préliminaire */
premiere_etape(Tbox, Abi, Abr) :-
  traitement_Tbox(Tbox),
  traitement_Abox(Abi, Abr).


/*-----------------------------------------------------------*/
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
Un concept est-il valide ?
-----------------------------------------------------------*/
concept(X) :-
    testcnamea(X).
concept(X) :-
    testcnamena(X).
concept(not(X)) :-
    concept(X),!.
concept(and(X, Y)) :-
    concept(X),
    concept(Y),!.
concept(or(X, Y)) :-
    concept(X),
    concept(Y),!.
concept(some(R, X)) :-
    testrname(R),
    concept(X),!.
concept(all(R, X)) :-
    testrname(R),
    concept(X),!.

/*-----------------------------------------------------------
Gère le cas des concepts non atomiques.
-----------------------------------------------------------*/
concept_a(X) :-
    testcnamea(X).
concept_a(not(X)) :-
    concept_a(X),!.
concept_a(and(X, Y)) :-
    concept_a(X),
    concept_a(Y),!.
concept_a(or(X, Y)) :-
    concept_a(X),
    concept_a(Y),!.
concept_a(some(R, X)) :-
    testrname(R),
    concept_a(X),!.
concept_a(all(R, X)) :-
    testrname(R),
    concept_a(X),!.

/*-----------------------------------------------------------
Un concept est-il auto-référent ?
-----------------------------------------------------------*/
autoref(X, X, _, _).
autoref(X, A, [(X, _)|L], Lbase) :-
    autoref(X, A, L, Lbase),!.
autoref(X, A, [(Y, _)|L], Lbase) :-
    testcnamena(A),
    Y\=A,
    autoref(X, A, L, Lbase),!.
autoref(X, A, [(A, Y)|_], Lbase) :-
    testcnamena(A),
    autoref(X, Y, Lbase, Lbase),!.
autoref(X, not(A), _, Lbase) :-
    autoref(X, A, Lbase, Lbase),!.
autoref(X, and(A, B), _, Lbase) :-
    (   autoref(X, A, Lbase, Lbase)
    ;   autoref(X, B, Lbase, Lbase),!
    ).
autoref(X, or(A, B), _, Lbase) :-
    (   autoref(X, A, Lbase, Lbase)
    ;   autoref(X, B, Lbase, Lbase),!
    ).
autoref(X, some(_, A), _, Lbase) :-
    autoref(X, A, Lbase, Lbase),!.
autoref(X, all(_, A), _, Lbase) :-
    autoref(X, A, Lbase, Lbase),!.


/*-----------------------------------------------------------
Remplace les concepts atomiques par leur définition
-----------------------------------------------------------*/
remplacerTbox(X, X, _) :-
    concept_a(X).
remplacerTbox(X, Y, [(X, Y)|_]).
remplacerTbox(X, Y, [(A, _)|LTbox]) :-
    X\=A,
    remplacerTbox(X, Y, LTbox),!.
remplacerTbox(not(X), not(Y), LTbox) :-
    remplacerTbox(X, Y, LTbox),!.
remplacerTbox(and(X1, X2), and(Y1, Y2), LTbox) :-
    remplacerTbox(X1, Y1, LTbox),
    remplacerTbox(X2, Y2, LTbox),!.
remplacerTbox(or(X1, X2), or(Y1, Y2), LTbox) :-
    remplacerTbox(X1, Y1, LTbox),
    remplacerTbox(X2, Y2, LTbox),!.
remplacerTbox(some(R, X), some(R, Y), LTbox) :-
    remplacerTbox(X, Y, LTbox),!.
remplacerTbox(all(R, X), all(R, Y), LTbox) :-
    remplacerTbox(X, Y, LTbox),!.


/*-----------------------------------------------------------
Vérifie et met sous forme normale négative les axiomes
terminologiques de la TBox.
-----------------------------------------------------------*/
traitement_Tbox_entiere([], []).
traitement_Tbox_entiere([(A, X)|L1], [(A, Y)|L2]) :-
    testcnamena(A),
    concept(X),
    equiv(A, X),
    nnf(X, Y),
    traitement_Tbox_entiere(L1, L2),!.

traitement_Tbox(Tbox) :-
    tBox(X),
    traitement_Tbox_entiere(X, Tbox).

/*-----------------------------------------------------------
Vérifie et met sous forme normale négative les assertions 
de la ABox.
-----------------------------------------------------------*/
%-- traitement Abox_concept
traitement_Abox_concept_unite(X, Y, _) :-
    concept_a(X),
    nnf(X, Y).
traitement_Abox_concept_unite(X, Y, LTbox) :-
    concept(X),
    remplacerTbox(X, Y1, LTbox),
    nnf(Y1, Y),!.

traitement_Abox_concept_entiere([], [], _).
traitement_Abox_concept_entiere([(A, X)|L1], [(A, Y)|L2], LTBOX) :-
    testiname(A),
    traitement_Abox_concept_unite(X, Y, LTBOX),
    traitement_Abox_concept_entiere(L1, L2, LTBOX),!.

%-- traitement Abox_role
traitement_Abox_role([]).
traitement_Abox_role([(A, B, R)|L]) :-
    testiname(A),
    testiname(B),
    testrname(R),
    traitement_Abox_role(L),!.

%-- traitement Abox
traitement_Abox(LC, LR) :-
    aBox(X),
    tBox(Y),
    assert_Role(Z),
    traitement_Abox_concept_entiere(X, LC, Y),
    traitement_Abox_role(Z),
    concat([], Z, LR),!.



