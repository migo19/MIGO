
:- dynamic(win/2).
:- dynamic(draw/2).

%% generate an example for Metagol by executing the current strategy
get_example(K,Sw,Sd,B,O,E):-
    board(K,B),
    asserta(board(B)), asserta(playing(1)),
    assert_prog_and_prims(Sw),assert_win(Sw),
    assert_prog_and_prims(Sd),assert_draw(Sd),
    play(learned_strategy,minimax,G,O,_),!,
    retract_win, retract_draw,
    retract_prog_and_prims(Sw),
    retract_prog_and_prims(Sd),
    update_exs([B|G],O,Sw,Sd),
    ((learning(mixed) -> update_draw_moves(Sw,E));
    true).

assert_prog_and_prims(LP):-
    assert_prog_prims(LP),
    maplist(assert_clause,LP).

retract_prog_and_prims(LP):-
    retract_prog_prims(LP),
    retract_program(LP).

assert_win(LP) :-
    depth_game(N),
    forall(between(1,N,X),(U is N-X, assert_win(U,LP))).

assert_win(X,LP) :-
    newpred(win,P,X),
    (current_predicate(P/2) -> asserta((win(A,B) :- call(P,A,B))));true.

assert_draw(LP) :-
    depth_game(N),
    forall(between(1,N,X),(U is N-X, assert_draw(U,LP))).

assert_draw(X,LP) :-
    newpred(draw,P,X),
    (current_predicate(P/2) -> asserta((draw(A,B) :- call(P,A,B))));true.

retract_win :-
    depth_game(N),
    forall(between(1,N,X),retract_win(X)).

retract_win(X):-
    newpred(win,P,X),
    (retract((win(A,B) :- call(user:P,A,B)))); true.

retract_draw :-
    depth_game(N),
    forall(between(1,N,X),retract_draw(X)).

retract_draw(X):-
    newpred(draw,P,X),
    (retract((draw(A,B) :- call(user:P,A,B)))); true.

% first try to win then to draw when making a move
execute_strategy(B1,LP,B):- win(B1,B).
execute_strategy(B1,LP,B):- draw(B1,B).

newpred(F,P,K) :-
    atom_codes(F,X),
    name(K,KC),
    append(X,[95|KC],PC),
    name(P,PC), !.