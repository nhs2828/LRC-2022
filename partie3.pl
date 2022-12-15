/* Partie 3 - Démonstrateur */
troisieme_etape(Abi, Abr) :-
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls),
    resolution(Lie, Lpt, Li, Lu, Ls, Abr),
    nl,
    write('Youpiiiiii, on a demontre la proposition initiale !!!').

/*-----------------------------------------------------------
Tri la ABox afin de faciliter la recherche 
-----------------------------------------------------------*/
tri_Abox([], [], [], [], [], []).
tri_Abox([(I, some(R, C))|Abi], [(I, some(R, C))|Lie], Lpt, Li, Lu, Ls) :-
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls),
    !.
tri_Abox([(I, all(R, C))|Abi], Lie, [(I, all(R, C))|Lpt], Li, Lu, Ls) :-
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls),
    !.
tri_Abox([(I, and(C1, C2))|Abi], Lie, Lpt, [(I, and(C1, C2))|Li], Lu, Ls) :-
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls),
    !.
tri_Abox([(I, or(C1, C2))|Abi], Lie, Lpt, Li, [(I, or(C1, C2))|Lu], Ls) :-
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls),
    !.
tri_Abox([(I, C)|Abi], Lie, Lpt, Li, Lu, [(I, C)|Ls]) :-
    testcnamea(C),
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls),
    !.
tri_Abox([(I, not(C))|Abi], Lie, Lpt, Li, Lu, [(I, not(C))|Ls]) :-
    testcnamea(C),
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls),
    !.
/*tri_Abox([], [], [], [], [], []).
% Lie <- Assertions de type (I,some(R,C))
tri_Abox([(I, some(R, C))|Abi], [(I, some(R, C))|Lie], Lpt, Li, Lu, Ls) :-
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls),!.
% Lpt <- Assertions de type (I,all(R,C))
tri_Abox([(I, all(R, C))|Abi], Lie, [(I, all(R, C))|Lpt], Li, Lu, Ls) :-
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls),!.
% Li <- Assertions de type (I,and(C1,C2))  
tri_Abox([(I, and(C1, C2))|Abi], Lie, Lpt, [(I, and(C1, C2))|Li], Lu, Ls) :-
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls),!.
% Lu <- Assertions de type (I,or(C1,C2))  
tri_Abox([(I, or(C1, C2))|Abi], Lie, Lpt, Li, [(I, or(C1, C2))|Lu], Ls) :-
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls),!.
% Ls <- Assertions de type (I,C)
tri_Abox([(I, C)|Abi], Lie, Lpt, Li, Lu, [(I, C)|Ls]) :-
    testcnamea(C),
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls),!.
% Ls <- Assertions de type (I,not(C))  
tri_Abox([(I, not(C))|Abi], Lie, Lpt, Li, Lu, [(I, not(C))|Ls]) :-
    testcnamea(C),
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls),!.*/

/*-----------------------------------------------------------
Enlève les doublons dans les listes
-----------------------------------------------------------*/
remove_doublons(Ls, Lie, Lpt, Li, Lu, Ls1, Lie1, Lpt1, Li1, Lu1):-
    sort(Ls,Ls1), sort(Lie,Lie1), sort(Lpt,Lpt1), sort(Li,Li1), sort(Lu,Lu1),!.

/*-----------------------------------------------------------
Règle ∃
-----------------------------------------------------------*/
complete_some([(I, some(R, C))|Lie], Lpt, Li, Lu, Ls, Abr) :-
    nl,
    write('SOMEEEEE !!'),
    genere(B),
    A=(B, C),
    evolue_pourtout([A],
                    Lie,
                    Lpt,
                    Li,
                    Lu,
                    Ls,
                    Lie1,
                    Lpt1,
                    Li1,
                    Lu1,
                    Ls1),
    remove_doublons(Lie1,
                    Lpt1,
                    Li1,
                    Lu1,
                    Ls1,
                    Lie2,
                    Lpt2,
                    Li2,
                    Lu2,
                    Ls2),
    affiche_evolution_Abox(Ls,
                           [(I, some(R, C))|Lie],
                           Lpt,
                           Li,
                           Lu,
                           Abr,
                           Ls2,
                           Lie2,
                           Lpt2,
                           Li2,
                           Lu2,
                           [(I, B, R)|Abr]),
    resolution(Lie2,
               Lpt2,
               Li2,
               Lu2,
               Ls2,
               [(I, B, R)|Abr]),
    !.

/*-----------------------------------------------------------
Règle ⊓
-----------------------------------------------------------*/
transformation_and(Lie, Lpt, [(I, and(C1, C2))|Li], Lu, Ls, Abr) :-
    A1=(I, C1),
    evolue_pourtout([A1],
                    Lie,
                    Lpt,
                    Li,
                    Lu,
                    Ls,
                    Lie1,
                    Lpt1,
                    Li1,
                    Lu1,
                    Ls1),
    A2=(I, C2),
    evolue_pourtout([A2],
                    Lie1,
                    Lpt1,
                    Li1,
                    Lu1,
                    Ls1,
                    Lie2,
                    Lpt2,
                    Li2,
                    Lu2,
                    Ls2),
    remove_doublons(Lie2,
                    Lpt2,
                    Li2,
                    Lu2,
                    Ls2,
                    Lie3,
                    Lpt3,
                    Li3,
                    Lu3,
                    Ls3),
    affiche_evolution_Abox(Ls,
                           Lie,
                           Lpt,
                           [(I, and(C1, C2))|Li],
                           Lu,
                           Abr,
                           Ls3,
                           Lie3,
                           Lpt3,
                           Li3,
                           Lu3,
                           Abr),
    resolution(Lie3,
               Lpt3,
               Li3,
               Lu3,
               Ls3,
               Abr),
    !.
/*-----------------------------------------------------------
Règle ∀
-----------------------------------------------------------*/
chercher_Abox_role(_, _, _, []) :-
    !.
chercher_Abox_role(A, R, [B|L], [(A, B, R)|Abr]) :-
    chercher_Abox_role(A, R, L, Abr),
    !.
chercher_Abox_role(A, R, L, [(A1, _, R1)|Abr]) :-
    (   A\=A1
    ;   R\=R1
    ),
    chercher_Abox_role(A, R, L, Abr),
    !.

ajouter_deduction_all([], _, _).
ajouter_deduction_all([B|ListeB], C, [(B, C)|Ls]) :-
    ajouter_deduction_all(ListeB, C, Ls),
    !.

deduction_all(Lie, [(I, all(R, C))|Lpt], Li, Lu, Ls, Abr) :-
    chercher_Abox_role(I, R, ListeB, Abr),
    ajouter_deduction_all(ListeB, C, ListBC),
    evolue_pourtout(ListBC,
                    Lie,
                    Lpt,
                    Li,
                    Lu,
                    Ls,
                    Lie1,
                    Lpt1,
                    Li1,
                    Lu1,
                    Ls1),
    remove_doublons(Lie1,
                    Lpt1,
                    Li1,
                    Lu1,
                    Ls1,
                    Lie2,
                    Lpt2,
                    Li2,
                    Lu2,
                    Ls2),
    affiche_evolution_Abox(Ls,
                            Lie,
                            [(I, all(R, C))|Lpt],
                            Li,
                            Lu,
                            Abr,
                            Ls2,
                            Lie2,
                            Lpt2,
                            Li2,
                            Lu2,
                            Abr),
    resolution(Lie2,
                Lpt2,
                Li2,
                Lu2,
                Ls2,
                Abr),
    !.

/*----------------------------------------------------------- 
Règle ⊔
-----------------------------------------------------------*/
transformation_or(Lie, Lpt, Li, [(I, or(C1, C2))|Lu], Ls, Abr) :-
    A=(I, C1),
    evolue_pourtout([A],
                    Lie,
                    Lpt,
                    Li,
                    Lu,
                    Ls,
                    Lie1,
                    Lpt1,
                    Li1,
                    Lu1,
                    Ls1),
    remove_doublons(Lie1,
                    Lpt1,
                    Li1,
                    Lu1,
                    Ls1,
                    Lie2,
                    Lpt2,
                    Li2,
                    Lu2,
                    Ls2),
    affiche_evolution_Abox(Ls,
                           Lie,
                           Lpt,
                           Li,
                           [(I, or(C1, C2))|Lu],
                           Abr,
                           Ls2,
                           Lie2,
                           Lpt2,
                           Li2,
                           Lu2,
                           Abr),
    resolution(Lie2,
               Lpt2,
               Li2,
               Lu2,
               Ls2,
               Abr),
    !.

transformation_or(Lie, Lpt, Li, [(I, or(C1, C2))|Lu], Ls, Abr) :-
    A=(I, C2),
    evolue_pourtout([A],
                    Lie,
                    Lpt,
                    Li,
                    Lu,
                    Ls,
                    Lie1,
                    Lpt1,
                    Li1,
                    Lu1,
                    Ls1),
    remove_doublons(Lie1,
                    Lpt1,
                    Li1,
                    Lu1,
                    Ls1,
                    Lie2,
                    Lpt2,
                    Li2,
                    Lu2,
                    Ls2),
    affiche_evolution_Abox(Ls,
                           Lie,
                           Lpt,
                           Li,
                           [(I, or(C1, C2))|Lu],
                           Abr,
                           Ls2,
                           Lie2,
                           Lpt2,
                           Li2,
                           Lu2,
                           Abr),
    resolution(Lie2,
               Lpt2,
               Li2,
               Lu2,
               Ls2,
               Abr),
    !.

/*----------------------------------------------------------- 
Une branche est-elle fermée ?
-----------------------------------------------------------*/
test_clash_unite((I, C), [(I, not(C))|_]) :-
    nl,
    write('Clash trouve:'),
    nl,
    affiche((I, C)),
    nl,
    write('\tET\t'),
    affiche((I, not(C))),
    !.
test_clash_unite((I, not(C)), [(I, C)|_]) :-
    nl,
    write('Clash trouve:'),
    nl,
    affiche((I, not(C))),
    nl,
    write('\tET\t'),
    affiche((I, C)),
    !.
test_clash_unite((I, C), [(I1, X)|Li]) :-
    nnf(not(C), NewC),
    (   I\=I1
    ;   NewC\=X
    ),
    test_clash_unite((I, C), Li),
    !.

/*----------------------------------------------------------- 
Une branche est-elle ouverte ?
-----------------------------------------------------------*/
test_no_clash_bis([], _).
test_no_clash_bis([X|L], Ls) :-
    not(test_clash_unite(X, Ls)),
    test_no_clash_bis(L, Ls),
    !.

test_no_clash([]).
test_no_clash([(I, C)|Ls]) :-
    nnf(not(C), NC),
    not(member((I, NC), Ls)),
    test_no_clash(Ls),
    !.

/*----------------------------------------------------------- 
Application des règles pour démontrer la proposition
saisie par l'utilisateur
-----------------------------------------------------------*/
resolution([], [], [], [], Ls, _) :-
    nl,
    write('Fin resolution'),
    not(test_no_clash(Ls)),
    nl,
    write('Dans LS:'),
    affiche(Ls),
    !.
resolution([(I, some(R, C))|Lie], Lpt, Li, Lu, Ls, Abr) :-
    test_no_clash(Ls),
    complete_some([(I, some(R, C))|Lie],
                  Lpt,
                  Li,
                  Lu,
                  Ls,
                  Abr),
    !.
resolution([], Lpt, [(I, and(C1, C2))|Li], Lu, Ls, Abr) :-
    test_no_clash(Ls),
    transformation_and([],
                       Lpt,
                       [(I, and(C1, C2))|Li],
                       Lu,
                       Ls,
                       Abr),
    !.
resolution([], [(I, all(R, C))|Lpt], [], Lu, Ls, Abr) :-
    test_no_clash(Ls),
    deduction_all([],
                  [(I, all(R, C))|Lpt],
                  [],
                  Lu,
                  Ls,
                  Abr),
    !.
resolution([], [], [], [(I, or(C1, C2))|Lu], Ls, Abr) :-
    test_no_clash(Ls),
    transformation_or([],
                      [],
                      [],
                      [(I, or(C1, C2))|Lu],
                      Ls,
                      Abr),
    !.

/*-----------------------------------------------------------*/
evolue_pourtout([], Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    !.
evolue_pourtout([(I, some(R, C))|L], Lie, Lpt, Li, Lu, Ls, [(I, some(R, C))|Lie1], Lpt1, Li1, Lu1, Ls1) :-
    evolue_pourtout(L,
                    [(I, some(R, C))|Lie],
                    Lpt,
                    Li,
                    Lu,
                    Ls,
                    Lie1,
                    Lpt1,
                    Li1,
                    Lu1,
                    Ls1),
    !.

evolue_pourtout([(I, all(R, C))|L], Lie, Lpt, Li, Lu, Ls, Lie1, [(I, all(R, C))|Lpt1], Li1, Lu1, Ls1) :-
    evolue_pourtout(L,
                    Lie,
                    [(I, all(R, C))|Lpt],
                    Li,
                    Lu,
                    Ls,
                    Lie1,
                    Lpt1,
                    Li1,
                    Lu1,
                    Ls1),
    !.

evolue_pourtout([(I, and(C1, C2))|L], Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, [(I, and(C1, C2))|Li1], Lu1, Ls1) :-
    evolue_pourtout(L,
                    Lie,
                    Lpt,
                    [(I, and(C1, C2))|Li],
                    Lu,
                    Ls,
                    Lie1,
                    Lpt1,
                    Li1,
                    Lu1,
                    Ls1),
    !.

evolue_pourtout([(I, or(C1, C2))|L], Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, [(I, or(C1, C2))|Lu1], Ls1) :-
    evolue_pourtout(L,
                    Lie,
                    Lpt,
                    Li,
                    [(I, or(C1, C2))|Lu],
                    Ls,
                    Lie1,
                    Lpt1,
                    Li1,
                    Lu1,
                    Ls1),
    !.

evolue_pourtout([(I, C)|L], Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, [(I, C)|Ls1]) :-
    evolue_pourtout(L,
                    Lie,
                    Lpt,
                    Li,
                    Lu,
                    [(I, C)|Ls],
                    Lie1,
                    Lpt1,
                    Li1,
                    Lu1,
                    Ls1),
    !.

evolue_pourtout([(I, not(C))|L], Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, [(I, not(C))|Ls1]) :-
    evolue_pourtout(L,
                    Lie,
                    Lpt,
                    Li,
                    Lu,
                    [(I, not(C))|Ls],
                    Lie1,
                    Lpt1,
                    Li1,
                    Lu1,
                    Ls1),
    !.

/*-----------------------------------------------------------*/ 
evolue((I, some(R, C)), Lie, Lpt, Li, Lu, Ls, [(I, some(R, C))|Lie], Lpt, Li, Lu, Ls) :-
    !.
evolue((I, all(R, C)), Lie, Lpt, Li, Lu, Ls, Lie, [(I, all(R, C))|Lpt], Li, Lu, Ls) :-
    !.
evolue((I, and(C1, C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, [(I, and(C1, C2))|Li], Lu, Ls) :-
    !.
evolue((I, or(C1, C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, [(I, or(C1, C2))|Lu], Ls) :-
    !.
evolue((I, C), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, [(I, C)|Ls]) :-
    testcnamea(C),
    !.
evolue((I, not(C)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, [(I, not(C))|Ls]) :-
    testcnamea(C),
    !.

/*----------------------------------------------------------- 
Affiche l'évolution d'une étape d'un état
-----------------------------------------------------------*/
affiche([]).

affiche([X|L]) :-
    affiche(X),
    affiche(L).
affiche((I, some(R, C))) :-
    nl,
    affiche(I),
    write(': some.'),
    affiche(R),
    write('.'),
    affiche(C),
    !.

affiche((I, all(R, C))) :-
    nl,
    affiche(I),
    write(': all.'),
    affiche(R),
    write('.'),
    affiche(C),
    !.

affiche((I, and(C, D))) :-
    nl,
    affiche(I),
    write(': '),
    affiche(C),
    write(' and '),
    affiche(D),
    !.

affiche((I, or(C, D))) :-
    nl,
    affiche(I),
    write(': '),
    affiche(C),
    write(' or '),
    affiche(D),
    !.

affiche(not(C)) :-
    write('not '),
    affiche(C).

affiche((I, C, R)) :-
    nl,
    write(<),
    affiche(I),
    write(', '),
    affiche(C),
    write('>: '),
    affiche(R),
    !.

affiche((I, C)) :-
    nl,
    affiche(I),
    write(': '),
    affiche(C),
    !.

affiche(some(R, C)) :-
    write(' some.'),
    affiche(R),
    write('.'),
    affiche(C),
    !.

affiche(all(R, C)) :-
    write('all.'),
    affiche(R),
    write('.'),
    affiche(C),
    !.

affiche(and(C1, C2)) :-
    affiche(C1),
    write(' and '),
    affiche(C2),
    !.

affiche(or(C1, C2)) :-
    affiche(C1),
    write(' or '),
    affiche(C2),
    !.
affiche(C) :-
    write(C).

/*----------------------------------------------------------- 
Affiche l'évolution d'un état de la Abox étendue
-----------------------------------------------------------*/
affiche_evolution_Abox(Ls1, Lie1, Lpt1, Li1, Lu1, Abr1, Ls2, Lie2, Lpt2, Li2, Lu2, Abr2) :-
    nl,
    write('--------- Etat de depart ----------'),
    nl,
    write('Lie:'),
    affiche(Lie1),
    nl,
    write('Lpt:'),
    affiche(Lpt1),
    nl,
    write('Li:'),
    affiche(Li1),
    nl,
    write('Lu:'),
    affiche(Lu1),
    nl,
    write('Ls:'),
    affiche(Ls1),
    nl,
    write('Abr:'),
    affiche(Abr1),
    nl,
    write('--------- Etat d\'arrivee ----------'),
    nl,
    write('Lie:'),
    affiche(Lie2),
    nl,
    write('Lpt:'),
    affiche(Lpt2),
    nl,
    write('Li:'),
    affiche(Li2),
    nl,
    write('Lu:'),
    affiche(Lu2),
    nl,
    write('Ls:'),
    affiche(Ls2),
    nl,
    write('Abr:'),
    affiche(Abr2),
    !.

    
