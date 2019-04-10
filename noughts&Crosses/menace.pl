%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Implementation of the MENACE system
%% Original article:
%% Michie, D., "Experiments on the Mechanization of Game-Learning Part I. Characterization of the Model and its parameters",
%% The Computer Journal, Volume 6, Issue 3, 1 November 1963, Pages 232–236,https://doi.org/10.1093/comjnl/6.3.232
%% Menace starts and has mark o
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(box/4).

%% beads and their correspondance
beads([white, lilac, silver, black, gold, green, amber, red, pink]).


%% move of Menace
move_menace(C,M,B1,B2):-
    mark(M),
    beads(Colors),
    nth0(I,Colors,C),
    I1 is I+1,
    arg(I1,B1,e),
    switch(B1,I,M,B2),!.

%% If Menace has only one choice left it plays this move
play_menace(_,M,B1,B2,Bead):-
    empty_positions(B1,I),
    length(I,1),!,
    I = [I1],
    switch(B1,I1,M,B2),
    move_menace(Bead,M,B1,B2),
    retractall(board(_)),
    asserta(board(B2)),
    retractall(bead(_)),
    asserta(bead(Bead)).

%% otherwise it draws a bead from the corresponding box
play_menace(S,M,B1,B2,Bead):-
    empty_positions(B1,I),
    \+(length(I,1)),!,
    ((draw_bead(S,B1,Bead),
    move_menace(Bead,M,B1,B2))->
        (retractall(board(_)),
        asserta(board(B2)),
        retractall(bead(_)),
        asserta(bead(Bead)));
        (Bead = no)).

draw_bead(S,Board,B):-
    ((find_box(S,Board,_,Beads,_,Transformation));
    (equivalent_(Board,Box),equivalent(Board,Box,Transformation),
    initial_beads(Box,Beads))),
    beads(C),
    transform(C,C1,Transformation),
    extend(C1,Beads,L),
    random_member(B,L).

initial_beads(Box,Beads):-
    playing(P),
    player_mark(P,M),
    positions(Box,M,N),!,
    N1 is 4-N,
    empty_positions(Box,I),
    reduced_indexes(Box,I,I1),!,
    replicate_at_pos((N1,0),9,I1,Beads).

reduced_indexes(Board,I1,I2):-
    playing(P),
    player_mark(P,M),
    findall(B2,(member(I,I1),switch(Board,I,M,B2)),Boards),
    reduced_indexes_(Boards,I1,I2).
reduced_indexes_([],[],[]).
reduced_indexes_([B|Bs],[_|I1s],I2):-
    member(B1,Bs),
    equivalent(B1,B,_),
    reduced_indexes_(Bs,I1s,I2).
reduced_indexes_([B|Bs],[I|I1s],[I|I2s]):-
    \+((member(B1,Bs),
    equivalent(B1,B,_))),
    reduced_indexes_(Bs,I1s,I2s).

%% update the number of Beads at the end of a game
update_beads([],[],_).
update_beads([_],[],_).
%% no box if there is only one position left
update_beads([B1,B2],[Color1],_):-
    move_menace(Color1,o,B1,B2),
    empty_positions(B1,I),
    length(I,1),!.
update_beads([B1,B2],[Color1],Outcome):-
    move_menace(Color1,o,B1,B2),
    update_beads_(B1,Color1,Outcome).
update_beads([B1,B2|RestGame],[Color1,_|Colors],Outcome):-
    move_menace(Color1,o,B1,B2),
    update_beads_(B1,Color1,Outcome),
    update_beads(RestGame,Colors,Outcome).
update_beads_(Board,Color,Outcome):-
    find_box(current,Board,Box,Beads,Counter1,Transformation),!,
    Counter is Counter1+1,
    retractall(box(current,Box,_,_)),
    beads(C),
    transform(C,C1,Transformation),
    nth0(I,C1,Color),
    nth0(I,Beads,X),
    X = (N,CountBead),
    change(N,N1,Outcome),
    NewCountBead is CountBead+1,
    switch_(Beads,I,(N1,NewCountBead),NewBeads),
    asserta(box(current,Box,NewBeads,Counter)).
update_beads_(Board,Color,Outcome):-
    initial_beads(Board,Beads),
    Box = Board,
    Counter = 1,
    retractall(box(current,Box,_,_)),
    beads(C),
    nth0(I,C,Color),
    nth0(I,Beads,X),
    X = (N,CountBead),
    change(N,N1,Outcome),
    NewCountBead is CountBead+1,
    switch_(Beads,I,(N1,NewCountBead),NewBeads),
    asserta(box(current,Box,NewBeads,Counter)).

find_box(S,Board,Box,Beads,Counter,Transformation):-
    equivalent_(Board,Box),
    box(S,Box,Beads,Counter),
    equivalent(Board,Box,Transformation).

change(N,N1,d):-
    N1 is N+1.
change(0,0,x):- !.
change(N,N1,x):-
    N1 is N-1.
change(N,N1,o):-
    N1 is N+3.

%% MENACE system
menace(N):-
    set_rand,
    write('START'), nl,
    get_time(T1),
    print(T1), nl,
    asserta(mark(learner,o)),
    asserta(mark(opponent,x)),
    asserta(box(current,[e,e,e,e,e,e,e,e,e],[(0,0),(0,0),(0,0),(0,0),(4,0),(0,0),(0,0),(4,0),(4,0)],0)),
    menace_(N),!.

menace_(0):- !.
menace_(N):-
    N1 is N-1,
    board(N1,B),
    asserta(board(B)),
    asserta(playing(1)),
    play(current,minimax,Game1,Outcome,Colors),
    append([B],Game1,Game),
    regret(B,Outcome,R),
    %write('Regret:'),
    write(R), nl,
    assert_last_strategy,
    update_beads(Game,Colors,Outcome),
    get_time(T1),
    print(T1), nl,
    menace_(N1).

assert_last_strategy:-
    findall(Box,(box(current,Box,_,_)),Boxes),
    assert_last(Boxes).

assert_last([]) :-!.
assert_last([Box|Boxes]):-
    box(current,Box,Beads,C),
    retractall(box(last,Box,_,_)),
    asserta(box(last,Box,Beads,C)),
    assert_last(Boxes).

%% symetries and rotations
equality(A,A).
symmetry_vertical(b(X0,X1,X2,X3,X4,X5,X6,X7,X8),b(X6,X7,X8,X3,X4,X5,X0,X1,X2)):- !.
symmetry_horizontal(b(X0,X1,X2,X3,X4,X5,X6,X7,X8),b(X2,X1,X0,X5,X4,X3,X8,X7,X6)):- !.
symmetry_diagonal1(b(X0,X1,X2,X3,X4,X5,X6,X7,X8),b(X0,X3,X6,X1,X4,X7,X2,X5,X8)):- !.
symmetry_diagonal2(b(X0,X1,X2,X3,X4,X5,X6,X7,X8),b(X8,X5,X2,X7,X4,X1,X6,X3,X0)):- !.
rotation_90(b(X0,X1,X2,X3,X4,X5,X6,X7,X8),b(X6,X3,X0,X7,X4,X1,X8,X5,X2)) :- !.
doublerotation_90(A,B):-
    rotation_90(A,C),
    rotation_90(C,B),!.
triplerotation_90(A,B):-
    rotation_90(A,C),
    rotation_90(C,D),
    rotation_90(D,B),!.

equivalent(A,B):-
    setof(B,equivalent_(A,B),Bs),
    member(B,Bs).

equivalent_(A,B):- equivalent(A,B,_).

equivalent(A,B,Transformation):-
    A = B, Transformation = equality;
    symmetry_vertical(A,B), Transformation = symmetry_vertical;
    symmetry_horizontal(A,B), Transformation = symmetry_horizontal;
    symmetry_diagonal1(A,B), Transformation = symmetry_diagonal1;
    symmetry_diagonal2(A,B), Transformation = symmetry_diagonal2;
    rotation_90(A,B), Transformation = rotation_90;
    doublerotation_90(A,B), Transformation = doublerotation_90;
    triplerotation_90(A,B), Transformation = triplerotation_90.

transform(A,B,Transformation) :- call(Transformation,A,B).
