
%% general background predicates

%% evaluate the minimax regret as the difference between the expected outcome (from the minimax database) and the outcome observed
regret(B,O,R) :-
    board_to_list(B,L),
    mark(learner,M),
    next_pos(L,_,M,PredO),
    regret_(PredO,O,R).

regret_(win,M,0) :- mark(learner,M).
regret_(win,M,2) :- mark(opponent,M).
regret_(win,d,1).
regret_(draw,M,1) :- mark(opponent,M).
regret_(draw,d,0).
regret_(loose,M,0) :- mark(opponent,M).

%% depth of a game measured as the length of the game sequence observed
depth(Game,D):-
    length(Game,N),
    D is floor(N/2).

list_to_state(L,O,S):-
    list_to_board(L,B),
    board_to_state(B,O,S).

boards_to_state(B1,B2,S1,S2):-
    toplay(B1,w),!,
    board_to_state_mark(B1,w,d,S1),
    board_to_state_mark(B2,b,d,S2).

boards_to_state(B1,B2,S1,S2):-
    toplay(B1,b),!,
    board_to_state_mark(B1,b,d,S1),
    board_to_state_mark(B2,w,d,S2).

board_to_state(B,M2,s(o,M2,B)):- toplayO(s(_,M2,B)),!.
board_to_state(B,M2,s(x,M2,B)):- toplayX(s(_,M2,B)),!.

board_to_list(B,L):-
    findall(M,(pos(P),arg(P,B,M)),L),!.

list_to_board(L1,B):-
    append([b],L1,L),
    B =.. L.

%% update the counter if the winning strategy is stable otherwise reset it
update_counter(_,_):- counter(C),ref_counter(C1),C>C1,!.
update_counter(0,_):-
    retractall(counter(_)),
    asserta(counter(0)).
update_counter(1,[]) :- !.
update_counter(1,_):-
    counter(C),
    retractall(counter(_)),
    C1 is C+1,
    asserta(counter(C1)).

%% merge program transfered with program learned
merge([],_,[]).
merge([sub(_,P,A1,_,_)|Prog],Prog2,Prog1):-
    member(sub(_,P,A1,_,_),Prog2),!,
    merge(Prog,Prog2,Prog1).
merge([sub(Name,P,A,MetaSub,PredTypes)|Prog],Prog2,[sub(Name,P,A,MetaSub,PredTypes)|Prog1]):-
    merge(Prog,Prog2,Prog1).
