
:- ['../MIGO/migo'].
:- ['../util'].
:- [environment_hexapawn].
:- [hexapawn].
:- [equivalence].
:- [her].
:- [minimax_database].
:- [minimax_read].

:- use_module(library(timeout)).


%% ---------- LEARNING TASK ----------

initialisation(L) :-
    set_rand,
    asserta(mark(learner,w)),
    asserta(mark(opponent,b)),
    asserta(depth_game(3)),
    ref_counter(L,C1),
    asserta(ref_counter(C1)),
    asserta(counter(0)),
    asserta(learning(L)).

ref_counter(mixed,0).
ref_counter(separated,5).

% N number of iterations
% Learning type: mixed or separated
test_hexa3(N,L):-
    initialisation(L),
    write('START'),nl,
    get_time(T1), print(T1), nl,
    N1 is N-1,
    get_example(N1,[],[],B,O1,1),
    regret(B,O1,R),write(R), nl,
    dependent_learning(5,Sw,Sd),!,
    prog_equivalent([],Sw,E),
    get_time(T2), print(T2), nl,
    test_hexa3(N1,Sw,Sd,SwF,SdF,E),
    writeln('Sw = '), write(SwF), nl,
    writeln('Sd = '), write(SdF).

test_hexa3(0,Sw,Sd,Sw,Sd,_) :- !.
test_hexa3(N,Sw,Sd,SwF,SdF,E):-
    N1 is N-1,
    get_example(N1,Sw,Sd,B,O1,E),!,
    regret(B,O1,R), write(R), nl,
    dependent_learning(5,Sw2,Sd2),!,
    prog_equivalent(Sw,Sw2,E1),
    update_counter(E1,Sw2),
    get_time(T1), print(T1), nl,
    test_hexa3(N1,Sw2,Sd2,SwF,SdF,E1).
