troisieme_etape(Abi, Abr) :-
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls),
    resolution(Lie, Lpt, Li, Lu, Ls, Abr),
    nl,
    write('Youpiiiiii, on a demontre la proposition initiale !!!').

/*-----------------------------------------------------------
tri_Abox/6
Tri la ABox afin de faciliter la recherche 
-----------------------------------------------------------*/
tri_Abox([], _, _, _, _, _).
% Lie <- Assertions de type (I,some(R,C))
tri_Abox([(I, some(R, C))|Abi], [(I, some(R, C))|Lie], Lpt, Li, Lu, Ls) :-
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls).
% Lpt <- Assertions de type (I,all(R,C))
tri_Abox([(I, all(R, C))|Abi], Lie, [(I, all(R, C))|Lpt], Li, Lu, Ls) :-
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls).
% Li <- Assertions de type (I,and(C1,C2))  
tri_Abox([(I, and(C1, C2))|Abi], Lie, Lpt, [(I, and(C1, C2))|Li], Lu, Ls) :-
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls).
% Lu <- Assertions de type (I,or(C1,C2))  
tri_Abox([(I, or(C1, C2))|Abi], Lie, Lpt, Li, [(I, or(C1, C2))|Lu], Ls) :-
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls).
% Ls <- Assertions de type (I,C)
tri_Abox([(I, C)|Abi], Lie, Lpt, Li, Lu, [(I, C)|Ls]) :-
    testcnamea(C),
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls).
% Ls <- Assertions de type (I,not(C))  
tri_Abox([(I, not(C))|Abi], Lie, Lpt, Li, Lu, [(I, not(C))|Ls]) :-
    testcnamea(C),
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls).


/*-----------------------------------------------------------
chercher_Abox_role/4
-----------------------------------------------------------*/
chercher_Abox_role(_, _, _, []).
chercher_Abox_role(A, R, [B|L], [(A, B, R)|Abr]) :-
    chercher_Abox_role(A, R, L, Abr).
chercher_Abox_role(A, R, L, [(A1, _, R1)|Abr]) :-
    (   A\=A1
    ;   R\=R1
    ),
    chercher_Abox_role(A, R, L, Abr).

/*-----------------------------------------------------------
ajouter_deduction_all/3
-----------------------------------------------------------*/
ajouter_deduction_all([], _, _).
ajouter_deduction_all([B|ListeB], C, [(B, C)|Ls]) :-
    ajouter_deduction_all(ListeB, C, Ls).


/*-----------------------------------------------------------
complete_some/6
Règle ∃
-----------------------------------------------------------*/
complete_some([], _, _, _, _, _).
complete_some([(I, some(R, C))|Lie], Lpt, Li, Lu, Ls, Abr) :-
    genere(B),
    complete_some(Lie, Lpt, Li, Lu, [(B, C)|Ls], [(I, B, R)|Abr]).

/*-----------------------------------------------------------
transformation_and/6
Règle ⊓
-----------------------------------------------------------*/
transformation_and(_, _, [], _, _, _).
transformation_and(Lie, Lpt, [(I, and(C1, C2))|Li], Lu, Ls, Abr) :-
    concat([(I, C1),  (I, C2)], Ls, NewLs),
    transformation_and(Lie, Lpt, Li, Lu, NewLs, Abr).

/*-----------------------------------------------------------
deduction_all/6
Règle ∀
-----------------------------------------------------------*/
deduction_all(_, [], _, _, _, _).
deduction_all(Lie, [(I, all(R, C))|Lpt], Li, Lu, Ls, Abr) :-
    chercher_Abox_role(I, R, ListeB, Abr),
    ajouter_deduction_all(ListeB, C, L),
    concat(L, Ls, NewLs),
    deduction_all(Lie, Lpt, Li, Lu, NewLs, Abr).

/*----------------------------------------------------------- 
transformation_or/6
Règle ⊔
-----------------------------------------------------------*/
transformation_or(_, _, _, [], _, _).
transformation_or(Lie, Lpt, Li, [(I, or(C1, C2))|Lu], Ls, Abr) :-
    (transformation_or(Lie, Lpt, Li, Lu, [(I, C1)|Ls], Abr);
     transformation_or(Lie, Lpt, Li, Lu, [(I, C2)|Ls], Abr)).