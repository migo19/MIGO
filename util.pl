set_rand:-
    sleep(1),
    get_time(X),
    U is floor(X),
    set_random(seed(U)).

replicate(X,N,L) :-
    findall(X,between(1,N,_),L).

replicate_at_pos(X,N,Ind,L):-
    replicate((0,0),N,L1),
    mask(L1,X,Ind,L).

replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- H \= O, replace(O, R, T, T2).

sub_list([],_) :- !.
sub_list([H|T],L) :-
    member(H,L),!,
    sub_list(T, L).

delMember(_, [], []).
delMember(X, [X|Xs], Y) :-
    delMember(X, Xs, Y).
delMember(X, [T|Xs], [T|Y]) :-
    dif(X, T),
    delMember(X, Xs, Y).

mask(L,_,[],L).
mask(L1,X,[I|Is],L2):-
    switch_(L1,I,X,L3),
    mask(L3,X,Is,L2).

extend([],[],[]).
extend([H|T],[(N,_)|Rest],L):-
    extend(T,Rest,L2),
    replicate(H,N,L1),
    append(L1,L2,L).

extend2([],[]).
extend2([(E,N,_)|T],L):-
    extend2(T,T2),
    replicate(E,N,L1),
    append(L1,T2,L).

copy(B,B) :- !.

get_last(L1,L2,Last):-
    last(L1,Last),
    append(L2,[Last],L1).

even(0).
even(X) :- succ(X1,X), odd(X1).
odd(X) :- succ(X1,X), even(X1).
