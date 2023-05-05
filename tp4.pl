% Masterming en prolog

nBienPlace([HCode1|BCode1], [HCode2|BCode2], BP) :-
    HCode1 = HCode2,
    nBienPlace(BCode1, BCode2, BP1),
    BP is BP1 + 1.

nBienPlace([_|BCode1], [_|BCode2], BP) :- nBienPlace(BCode1, BCode2, BP).

nBienPlace([], [], 0).

?- nBienPlace([1,2,3,4], [1,2,3,5], BP), write(BP), nl.

longueur([], 0).
longueur([_|L], N) :-
    longueur(L, N1),
    N is N1 + 1.

?- longueur([1,2,3,4], N), write(N), nl.

gagne(Code1, Code2) :- longueur(Code1, N), nBienPlace(Code1, Code2, N).

?- (gagne([1,2,3,4], [1,2,3,4]) -> write('true') ; write('false')), nl.

element(X, [X|_]).
element(X, [_|R]) :- element(X,R).

?- (element(2, [1,2,3,4]) -> write('true') ; write('false')), nl.

enleve(_, [], L2).
enleve(E, [E|BL1], BL1).
enleve(E, [HL1|BL1], [HL1|L2]) :- dif(E, HL1), enleve(E, BL1, L2).

?- enleve(2, [1,2,3,4], L2), write(L2), nl.

% si les 2 head =
enleveBP([HCode1|BCode1], [HCode1|BCode2], Code1Result, Code2Result) :- 
    enleveBP(BCode1, BCode2, Code1Result, Code2Result).
% si les 2 head /=
enleveBP([HCode1|BCode1], [HCode2|BCode2], [HCode1|Code1Result], [HCode2|Code2Result]) :- 
    dif(HCode1, HCode2),
    enleveBP(BCode1, BCode2, Code1Result, Code2Result).
% condition d'arret
enleveBP([], [], [], []).

?- enleveBP([1,2,3,4,5,6], [1,2,5,4,3,4], Code1Result, Code2Result), write(Code1Result), nl, write(Code2Result), nl.
