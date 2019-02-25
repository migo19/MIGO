%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Implementation of HER system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(box/4).
:- dynamic(beads/1).

%% beads and their correspondance
find_moves :-
    findall(B,move(_,B,_,_),Bs),
    asserta(beads(Bs)).

%% symetries and rotations
equality(A,A).
symmetry_horizontal([X0,X1,X2,X3,X4,X5,X6,X7,X8],[X2,X1,X0,X5,X4,X3,X8,X7,X6]):- !.
symmetry_horizontal(1/4,3/6).   symmetry_horizontal(3/6,1/4).   symmetry_horizontal(4/7,6/9).   symmetry_horizontal(6/9,4/7).   symmetry_horizontal(1/5,3/5).   symmetry_horizontal(3/5,1/5).   symmetry_horizontal(2/4,2/6).   symmetry_horizontal(2/6,2/4).   symmetry_horizontal(4/8,6/8).   symmetry_horizontal(6/8,4/8).   symmetry_horizontal(5/7,5/9).   symmetry_horizontal(5/9,5/7).
symmetry_horizontal(9/6,7/4).   symmetry_horizontal(7/4,9/6).   symmetry_horizontal(6/3,4/1).   symmetry_horizontal(4/1,6/3).   symmetry_horizontal(9/5,7/5).   symmetry_horizontal(7/5,9/5).   symmetry_horizontal(8/6,8/4).   symmetry_horizontal(8/4,8/6).   symmetry_horizontal(6/2,4/2).   symmetry_horizontal(4/2,6/2).   symmetry_horizontal(5/1,5/3).   symmetry_horizontal(5/3,5/1).
symmetry_horizontal(5/8,5/8).
symmetry_horizontal(2/5,2/5).
symmetry_horizontal(8/5,8/5).
symmetry_horizontal(5/2,5/2).

equivalent(A,B):-
    board_(A),!,
    setof(B,equivalent_(A,B),Bs),
    member(B,Bs),
    board_(B).

equivalent_(A,B):- equivalent(A,B,_).

equivalent(A,B,Transformation):-
    A = B, Transformation = equality;
    symmetry_horizontal(A,B), Transformation = symmetry_horizontal.

transform(A,B,Transformation) :- call(Transformation,A,B).

%% If Menace has only one choice left it plays this move
play_menace(_,M,B1,B2,Bead):-
    findall(B,(move(s(M,_,B1),s(_,_,B))),Bs),
    length(Bs,1),!,
    copy([B2],Bs),
    move(M,Bead,B1,B2),!,
    retractall(board(_)),
    asserta(board(B2)),
    retractall(bead(_)),
    asserta(bead(Bead)).

%% otherwise it draws a bead from the corresponding box
play_menace(S,M,B1,B2,Bead):-
    findall(B,(move(s(M,_,B1),s(_,_,B))),Bs),
    \+(length(Bs,1)),!,
    ((draw_bead(S,B1,Bead),
      move(M,Bead,B1,B2))->
        (retractall(board(_)),
        asserta(board(B2)),
        retractall(bead(_)),
        asserta(bead(Bead)));
            (Bead = no)).

draw_bead(S,Board,L1):-
    ((find_box(S,Board,Box,Beads,_,Transformation));
    (equivalent_(Board,Box), equivalent(Board,Box,Transformation),
    initial_beads(Box,Beads))),
    extend2(Beads,Beads2),
    length(Beads2,Length),
    random_member(Beads2,L),
    transform(L,L1,Transformation).

initial_beads(Box,Beads2):-
    board_to_list(Box,L),
    next_pos(L,_,M,N,_),
    findall(Bead,move(M,Bead,Box,_),Beads),
    N1 is floor(4-N/2),
    findall((I,N1,0),member(I,Beads),Beads2).

%% update the number of Beads at the end of a game
update_beads([],[],_).
update_beads([B1],[],_).
%% no box if there is only one position left
update_beads([B1,B2],[Bead1],Outcome):-
    move(_,Bead1,B1,B2),
    findall(B,(move(s(M,_,B2),s(_,_,B))),Bs),
    length(Bs,1),!.
update_beads([B1,B2],[Bead1],Outcome):-
    move(_,Bead1,B1,B2),
    update_beads_(B1,Bead1,Outcome).
update_beads([B1,B2|RestGame],[Bead1,_Bead2|Beads],Outcome):-
    move(_,Bead1,B1,B2),
    update_beads_(B1,Bead1,Outcome),
    update_beads(RestGame,Beads,Outcome).

update_beads_(Board,Bead,Outcome):-
    find_box(current,Board,Box,Beads,Counter1,Transformation),!,
    Counter is Counter1+1,
    transform(Bead,Bead1,Transformation),
    member((Bead1,N,M),Beads),
    change(N,N1,Outcome),
    M1 is M+1,
    replace((Bead1,N,M),(Bead1,N1,M1),Beads,NewBeads),
    retractall(box(current,Box,_,_)),
    asserta(box(current,Box,NewBeads,Counter)).
update_beads_(Board,Bead,Outcome):-
    initial_beads(Board,Beads),
    member((Bead,N,M),Beads),
    change(N,N1,Outcome),
    M1 is M+1,
    replace((Bead,N,M),(Bead,N1,M1),Beads,NewBeads),
    asserta(box(current,Board,NewBeads,1)).

find_box(S,Board,Box,Beads,Counter,Transformation):-
    box(S,Box,Beads,Counter),
    equivalent_(Board,Box),
    equivalent(Board,Box,Transformation).

change(0,0,_):- !.
change(N,N1,d):-
    N1 is N+1.
change(N,N1,w):-
    N1 is N+3.
change(N,N1,b):-
    N1 is N-1.

%% MENACE system
menace(N):-
    set_rand,
    write('START'), nl,
    get_time(T1),
    print(T1), nl,
    asserta(mark(learner,w)),
    asserta(mark(opponent,b)),
    find_moves,
    initial_board(B),
    asserta(box(current,B,[],0)),
    menace_(N),!.

menace_(0):- !.
menace_(N):-
  %  all_accessible_boards(BsAll),
  %  random_member(BsAll,B),!,
    board(K,B),
 %   printboard(B),
    asserta(board(B)),
    asserta(playing(1)),
    play(current,minimax,Game1,Outcome,Beads),!,
    append([B],Game1,Game),
    regret(B,Outcome,R),
  %  write('iteration:'), write(N), nl,
    write(R), nl,
    assert_last_strategy,
    findall([Box,B1,C],(box(last,Box,B1,C)),Boxes),
    update_beads(Game,Beads,Outcome),
    findall([Box,B1,C],(box(current,Box,B1,C)),Boxes2),
    N1 is N-1,!,
    get_time(T1),
    print(T1), nl,
    menace_(N1).

assert_last_strategy:-
    findall(Box,(box(current,Box,Beads,C)),Boxes),
    assert_last(Boxes).

assert_last([]) :-!.
assert_last([Box|Boxes]):-
    box(current,Box,Beads,C),
    retractall(box(last,Box,_,_)),
    asserta(box(last,Box,Beads,C)),
    assert_last(Boxes).
