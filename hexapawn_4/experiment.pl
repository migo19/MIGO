

:- ['../MIGO/migo'].
:- ['../util'].
:- [environment_hexapawn].
:- [equivalence].
:- [minimax_read].
:- [hexapawn_4].
:- [minimax_output].

%% ---------- LEARNING TASK ----------

initialisation(SwIni, SdIni, L) :-
    set_rand,
    assert_program(SwIni),
    assert_program(SdIni),
    asserta(mark(learner,w)),
    asserta(mark(opponent,b)),
    asserta(depth_game(9)),
    ref_counter(L,C1),
    asserta(ref_counter(C1)),
    asserta(counter(0)),
    asserta(learning(L)).

ref_counter(mixed,0).
ref_counter(separated,15).

% N number of iterations
% Learning type: mixed or separated
test(SwIni, SdIni,N,L):-
    initialisation(SwIni, SdIni,L),
    write('START'),nl,
    T1 is cputime, print(T1), nl,
    N1 is N-1,
    get_example(N1,[],[],B,O1,0),!,
    regret(B,O1,R), write(R), nl,
    retract_program(SwIni),
    retract_program(SdIni),
    dependent_learning(5,Sw,Sd),!,
    prog_equivalent([],Sw,E),
    merge(SwIni,Sw,SwIni2),
    merge(SdIni,Sd,SdIni2),
    T2 is cputime, print(T2), nl,
    test(N1,SwIni2,SdIni2,Sw,Sd,E).

test(0,_,_,_,_,_) :- !.
test(N,SwIni,SdIni,Sw,Sd,E):-
    N1 is N-1,
    assert_program(SwIni),
    assert_program(SdIni),
    get_example(N1,Sw,Sd,B,O1,E),!,
    regret(B,O1,R), write(R), nl,
    retract_program(SwIni),
    retract_program(SdIni),
    dependent_learning(5,Sw2,Sd2),!,
    merge(SwIni,Sw2,SwIni2),
    merge(SdIni,Sd2,SdIni2),
    prog_equivalent(Sw,Sw2,E1),
    update_counter(E1,Sw2),
    T1 is cputime, print(T1), nl,
    test(N1,SwIni2,SdIni2,Sw2,Sd2,E1).
