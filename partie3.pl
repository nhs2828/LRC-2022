troisieme_etape(Abi,Abr) :- 
  tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),
  resolution(Lie,Lpt,Li,Lu,Ls,Abr),
  nl,write('Youpiiiiii, on a demontre la proposition initiale !!!').

%--- Tri les assertions de la Abox étendue
tri_Abox([],[],[],[],[],[]).
% Lie <- Assertions de type (I,some(R,C))
tri_Abox([(I,some(R,C))|Abi],[(I,some(R,C))|Lie],Lpt,Li,Lu,Ls) :- 
  tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).
% Lpt <- Assertions de type (I,all(R,C))
tri_Abox([(I,all(R,C))|Abi],Lie,[(I,all(R,C))|Lpt],Li,Lu,Ls) :- 
  tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).
% Li <- Assertions de type (I,and(C1,C2))
tri_Abox([(I,and(C1,C2))|Abi],Lie,Lpt,[(I,and(C1,C2))|Li],Lu,Ls) :- 
  tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).
% Lu <- Assertions de type (I,or(C1,C2))
tri_Abox([(I,or(C1,C2))|Abi],Lie,Lpt,Li,[(I,or(C1,C2))|Lu],Ls) :- 
  tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).
% Ls <- Assertions de type (I,C)
tri_Abox([(I,C)|Abi],Lie,Lpt,Li,Lu,[(I,C)|Ls]) :- 
  tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),!.
% Ls <- Assertions de type (I,not(C))
tri_Abox([(I,not(C))|Abi],Lie,Lpt,Li,Lu,[(I,not(C))|Ls]) :- 
  tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),!.

%--- Résolution
% ∃R.C
complete_some(Lie,Lpt,Li,Lu,Ls,Abr).
% C ⊓ D
transformation_and(Lie,Lpt,Li,Lu,Ls,Abr).
% ∀R.C
deduction_all(Lie,Lpt,Li,Lu,Ls,Abr).
% C ⊔ D
transformation_or(Lie,Lpt,Li,Lu,Ls,Abr).