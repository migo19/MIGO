

%% ------ CANONICAL TRANSFORMATIONS ------

% the first solution returned is the cononical element of the class
eq(X,Y) :- symmetry_horizontal(X,Y).
eq(X,X).

eq2(X,Z) :- eq1(X,Y), eq2(Y,Z).
eq2(X,X).

eq1(X,Y) :- symmetry_horizontal(X,Y), lower_than(Y,X).

%% symetries and rotations
equality(A,A).
symmetry_horizontal(b(X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15),b(X3,X2,X1,X0,X7,X6,X5,X4,X11,X10,X9,X8,X15,X14,X13,X12)).

symmetry_horizontal([X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15],[X3,X2,X1,X0,X7,X6,X5,X4,X11,X10,X9,X8,X15,X14,X13,X12]).

lower_than(X,Y):-
    X =.. [b|X1],
    Y =.. [b|Y1],
    lower_than_(X1,Y1).

% lower_than(H1,H2) H1 is lower than  H2 in the lexicographic order
lower_than_([H1|T1],[H1|T2]):-
    lower_than_(T1,T2),!.
lower_than_([e|T1],[H2|T2]):-
    \+(H2=e),!.
    lower_than_([x|T1],[o|T2]) :- !.
