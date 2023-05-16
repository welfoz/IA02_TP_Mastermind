% IA02 - TP4 - Masterming en prolog

% étant donnés 2 codes Code1 et Code2, donne le nombre de couleurs bien placées.
nBienPlace([], [], 0).
nBienPlace([H|B1], [H|B2], Nb) :- nBienPlace(B1, B2, Nb1), Nb is Nb1 + 1.
nBienPlace([_|B1], [_|B2], Nb) :- nBienPlace(B1, B2, Nb).

%?- nBienPlace([1,2,3,4], [1,2,3,5], BP), write(BP), nl.

longueur([], 0).
longueur([_|B], N) :-
    longueur(B, N1),
    N is N1 + 1.

%?- longueur([1,2,3,4], N), write(N), nl.

% vérifie que les codes 1 et 2 sont identiques. 
gagne(Code1, Code2) :- longueur(Code1, N), nBienPlace(Code1, Code2, N).

%?- (gagne([1,2,3,4], [1,2,3,4]) -> write('true') ; write('false')), nl.

% vérifie que l’élément E appartient à la liste L.
element(X, [X|_]).
element(X, [_|B]) :- element(X,B).

%?- (element(2, [1,2,3,4]) -> write('true') ; write('false')), nl.

% construit la liste L2 de telle sorte qu’elle soit identique à L1 privée de la première occurrence de E.
enleve(_, [], []).
enleve(E, [E|B], B).
enleve(E, [H|B], [H|L2]) :- dif(E, H), enleve(E, B, L2).

%?- enleve(2, [1,2,3,4], L2), write(L2), nl.

% étant donné Code1 et Code2, est tel que Code1Bis (resp. Code2Bis) contiennent les éléments de Code1 (resp. Code2) privé des éléments bien placés (communs) avec Code2 (resp. Code1).
% si les 2 head =
enleveBP([HCode1|BCode1], [HCode1|BCode2], Code1Result, Code2Result) :- 
    enleveBP(BCode1, BCode2, Code1Result, Code2Result).
% si les 2 head /=
enleveBP([HCode1|BCode1], [HCode2|BCode2], [HCode1|Code1Result], [HCode2|Code2Result]) :- 
    dif(HCode1, HCode2),
    enleveBP(BCode1, BCode2, Code1Result, Code2Result).
% condition d'arret
enleveBP([], [], [], []).

%?- enleveBP([1,2,3,4,5,6], [1,2,5,4,3,4], Code1Result, Code2Result), write(Code1Result), nl, write(Code2Result), nl.

% donne le nombre d’éléments mal placés
nMalPlaces(Code1, Code2, Result) :- 
    enleveBP(Code1, Code2, Code1Result, Code2Result),
    nMalPlacesAux(Code1Result, Code2Result, Result).

nMalPlacesAux([], _, 0).
nMalPlacesAux([HCode1 | BCode1], Code2, Total) :-
    nMalPlacesAux(BCode1, Code2, Result),
    (element(HCode1, Code2) -> Total is Result+1 ; Total = Result).

?- nMalPlacesAux([1,2,3,4], [4,3,2,1], MP), write(MP), nl.
?- nMalPlaces([1,2,3,4], [1,3,2,1], MP), write(MP), nl.

%?- write("----------"), nl.

% créé un code de taille N basé sur M couleurs.
codeur(_, 0, []) :- !.
codeur(M, N, Code) :- 
    Max is M+1,
    random(1, Max, Random),
    N1 is N - 1,
    codeur(M, N1, Code1),
    Code = [Random|Code1].

%?- codeur(4, 4, Code), write(Code), nl.

% permet de jouer en tant que décodeur, code de taille N basé sur M couleurs, Max le nombre de tours.
jouons(M, N, Max) :-
    codeur(M, N, Code),
    write("Code : "), write(Code), nl,
    tour(Max, Code).

tour(0, _) :- write("Vous avez perdu !"), nl, !.
tour(Max, Code) :-
    write("Il reste "), write(Max), write(" tours"), nl,
    write("Entrez votre code : "), nl,
    read(Proposition),
    nBienPlace(Proposition, Code, BP),
    write("Bien placé : "), write(BP), nl,
    nMalPlaces(Proposition, Code, MP),
    write("Mal placé : "), write(MP), nl,
    (gagne(Proposition, Code) -> write('Vous avez gagné !'), ! ; newMax is Max-1, tour(newMax, Code)).

%?- jouons(5, 4, 10).