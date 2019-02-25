
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(timeout)).
:- use_module(library(system)).

player(1).
player(2).

next_player(1, 2).
next_player(2, 1).

player_mark(1, w) :- !.
player_mark(2, b) :- !.


initial_board(b(b,b,b,e,e,e,w,w,w)) :- !.

:- dynamic(board/1).
:- dynamic(playing/1).

reset :-
    retractall(board(_)),
    retractall(playing(_)),
    retractall(strategy(_)).

initialisation_game :-
    set_rand,
    initial_board(B),
    asserta(board(B)),
    asserta(playing(1)),!.

game(S1,S2,Game,Outcome) :-
    reset,
    %write('Player 1'), write(S1), nl,
    %write('Player 2'), write(S2), nl,
    initialisation_game,!,
    play(S1,S2,Game1,Outcome),
    initial_board(B),
    append([B],Game1,Game).

make_move(current,s(M,_,B),s(M1,_,B2),Bead):- next_mark(M,M1),play_menace(current,M,B,B2,Bead).
make_move(last,s(M,_,B),s(M1,_,B2),Bead):- next_mark(M,M1),play_menace(last,M,B,B2,Bead).

%% play with random strategy
make_move(random,s(M,_,B1),s(M1,_,B2),[]):-
    next_mark(M,M1),
    findall(s(M1,_,B2),(move(s(M,_,B1),s(M1,_,B2))),Ps),!,
    random_member(Ps,s(M1,_,B2)),
    retractall(board(_)),
    asserta(board(B2)).

%% play with minimax strategy
make_move(minimax,s(M,_,B1),s(M1,_,B2),[]):-
    next_mark(M,M1),
    board_to_list(B1,L1),
    next_pos(L1,L2,M,_),
    list_to_board(L2,B2),!,
    retractall(board(_)),
    asserta(board(B2)).

%% play with a strategy described by a logic program LP
%% play with a strategy described by a logic program LP
make_move(LP,s(M,_,B1),s(M1,_,B2),[]) :-
    ((execute_strategy(s(M,_,B1),LP,s(M1,_,B2)))->
    (retract(board(_)),
    asserta(board(B2)));
    (make_move(random,s(M,_,B1),s(M1,_,B2),Bead))).

%% play against user
make_move(human,s(M,_,B1),s(M1,_,B3),[]) :-
    nl, write('Next move?'), nl,
    read(Pos), nl,
    next_mark(M,M1),
    ((move(M,Pos,B1,B3)) ->
    copy(B2,B3),
    retractall(board(_)),
    asserta(board(B3));
    (write('Not a valid move'),
    make_move(human,s(M,_,B1),s(M1,_,B3),none))).

play(Strategy1,Strategy2,Game,Outcome,[Bead|Beads]) :-
    board(B),
    playing(P),
    player_mark(P,M),
    ((P is 1) -> make_move(Strategy1,s(M,_,B),s(M1,_,B2),Bead); make_move(Strategy2,s(M,_,B),s(M1,_,B2),Bead),!),
    board(B2),
 %   printboard(B2),
 %   writeln(B2),
    playing(P),
    player_mark(P,M),
    next_mark(M,M1),
    ((won(s(M1,_,B2)))
    -> (append([B2],[],Game),
                playing(P3), %write('WIN: '), write('Player '), write(P), nl,
                    copy(Beads,[]),
                    get_outcome(win,P3,Outcome));
    ((board(B2),drawn(s(_,_,B2))) -> (%write('END, draw'), nl,
                append([B2],[],Game),
                 playing(P3),
                    copy(Beads,[]),
                get_outcome(draw,P3,Outcome));
    (next_player,
    board(B2),
    play(Strategy1,Strategy2,Game2,Outcome,Bs),!,
    copy(Bs,Beads),
    append([B2],Game2,Game)))).

get_outcome(win,1,w).
get_outcome(win,2,b).
get_outcome(draw,_,d).

next_player :-
    playing(P),
    next_player(P,P2),
    retract(playing(_)),
    asserta(playing(P2)).
