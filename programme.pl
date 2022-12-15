/*---- Chargement des différents fichiers et de la base de connaissances ----*/
:- [partie1,
    partie2,
    partie3,
    utilitaires,
    connaissances].

/*----- Initialisation du compteur utilisé à la génération de nouveaux identificateurs ----*/
compteur(1). 

/*---- Démonstrateur ----*/
programme :- 
  premiere_etape(Tbox, Abi, Abr),
  deuxieme_etape(Abi, Abi1, Tbox),
  troisieme_etape(Abi1, Abr).