
:- [menace].

:- dynamic(board/1).
:- dynamic(playing/1).
:- dynamic(bead/1).

player(1).
player(2).

next_player(1, 2).
next_player(2, 1).

next_mark(o,x) :-!.
next_mark(x,o) :-!.

player_mark(1, o) :- !.
player_mark(2, x) :- !.

marks([x,o]).

mark(M) :- marks(Ms), member(M,Ms).

initial_board([e,e,e,e,e,e,e,e,e]) :- !.

reset :-
    retractall(board(_)),
    retractall(playing(_)),
    retractall(strategy(_)).

initialisation_game(B) :-
    asserta(board(B)),
    asserta(playing(1)),!.

game(S1,S2,Game,Colors,Outcome) :-
    initial_board(B),
    game(B,S1,S2,Game,Colors,Outcome).

game(B,S1,S2,Game,Colors,Outcome):-
    reset,
    initialisation_game(B),!,
    play(S1,S2,Game1,Outcome,Colors),
    append([B],Game1,Game).

%% play against menace
make_move(current,M,B,B2,Bead):-play_menace(current,M,B,B2,Bead).
make_move(last,M,B,B2,Bead):-play_menace(last,M,B,B2,Bead).

%% play with optimal strategy = minimax algorithm
make_move(minimax,_,B,B2,none):- !,
    board_to_list(B,L),
    next_pos(L,L2,_,_),
    list_to_board(L2,B2),!,
    retractall(board(_)),
    asserta(board(B2)),
    asserta(bead(none)).

%% play with random strategy
make_move(random,M,B,B2,none):-
    findall(s(M1,_,B1),(move(s(M,_,B),s(M1,_,B1))),Ps),!,
    random_member(s(M1,_,B2),Ps),
    retractall(board(_)),
    asserta(board(B2)),
    asserta(bead(none)).

%% play with a strategy described by a logic program LP
make_move(learned_strategy,o,B1,B2,none) :-
    ((execute_strategy(s(o,_,B1),_,s(x,_,B2)))->
    (asserta(bead(none)),
    retract(board(_)),
    asserta(board(B2)));
    (make_move(random,o,B1,B2,none))).

play(S1,S2,Game,Outcome,Colors) :-
    board(B),
    playing(P),
    player_mark(P,M),
    ((P is 1) -> make_move(S1,M,B,B2,Bead); make_move(S2,M,B,B2,Bead)),
    ((Bead = no) -> (Game = [], Colors = [],
    playing(Q), next_player(Q,R),
    get_outcome(win,R,Outcome));
    board(B2),
    bead(Bead),
    playing(P),
    player_mark(P,M),
    next_player(P,P2),
    player_mark(P2,M2),
    ((won(s(M2,_,B2)))
    -> (append([B2],[],Game),append([Bead],[],Colors),
                playing(P3), %write('WIN: '), write('Player '), write(P), nl,
                    get_outcome(win,P3,Outcome));
    ((drawn(s(M2,_,B2))) -> (%write('END, draw'), nl,
                append([B2],[],Game),append([Bead],[],Colors),
                 playing(P3),
                get_outcome(draw,P3,Outcome));
    (next_player,
    play(S1,S2,Game2,Outcome,Colors2),!,
    append([B2],Game2,Game),
    append([Bead],Colors2,Colors))))).

get_outcome(win,1,o).
get_outcome(win,2,x).
get_outcome(draw,_,d).

next_player :-
    playing(P),
    next_player(P,P2),
    retract(playing(_)),
    asserta(playing(P2)).

switch(B,N,M,B1):-
    B =.. L,
    N1 is N+1,
    switch_(L,N1,M,L1),
    B1 =.. L1.

switch_([_|R],0,M,[M|R]) :- !.
switch_([X|R],N,M,[X|R2]):-
    N > 0,
    N1 is N-1,
    switch_(R,N1,M,R2),!.

positions(B,M,N) :-
    findall(N1,(between(1,9,N),arg(N,B,M),N1 is N-1),Indexes),
    length(Indexes,N).

empty_positions(B,Indexes):-
    findall(N1,(between(1,9,N),arg(N,B,e),N1 is N-1),Indexes).


empty(Pos,Board):-
    arg(Pos,Board,e).
