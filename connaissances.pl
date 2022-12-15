/* Base de connaissances */
% Concepts
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

% Instances
iname(michelAnge).
iname(david).
iname(sonnets).
iname(vinci).
iname(joconde).

% Rôles
rname(aCree).
rname(aEcrit).
rname(aEdite).
rname(aEnfant).

% Axiomes terminologiques
equiv(sculpteur, and(personne, some(aCree, sculpture))).
equiv(auteur, and(personne, some(aEcrit, livre))).
equiv(editeur, and(personne, and(not(some(aEcrit, livre)), some(aEdite, livre)))).
equiv(parent, and(personne, some(aEnfant, anything))).

% Assertions de concepts
inst(michelAnge, personne).
inst(david, sculpture).
inst(sonnets, livre).
inst(vinci, personne).
inst(joconde, objet).

% Assertions de rôles
instR(michelAnge, david, aCree).
instR(michelAnge, sonnets, aEcrit).
instR(vinci, joconde, aCree).

% TBox et Abox
tBox(Tbox) :-
      setof((X, Y), equiv(X, Y), Tbox).
aBox(Abi) :-
      setof((X, Y), inst(X, Y), Abi).
assert_Role(Abr) :-
      setof((X, Y, Z), instR(X, Y, Z), Abr).

/* % Chargement "manuel" de la TBox et de la ABox
tBox([(sculpteur, and(personne, some(aCree, sculpture))),  (auteur, and(personne, some(aEcrit, livre))),  (editeur, and(personne, and(not(some(aEcrit, livre)), some(aEdite, livre)))),  (parent, and(personne, some(aEnfant, anything)))]).

aBox([(michelAnge, personne),  (david, sculpture),  (sonnets, livre),  (vinci, personne),  (joconde, objet)]).

assert_role([(michelAnge, david, aCree),  (michelAnge, sonnets, aEcrit),  (vinci, joconde, aCree)]).
*/