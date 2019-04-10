%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Primitives for playing O+X. It is assumed
%	the board is a 9-vector and moves are
%	represented by pairs of boards connected
%	by sequences of legal moves.
%   board b(e,e,e,e,e,e,e,e,e)
%   state S s(Toplay,Outcome,Board)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% board representation

board_(B):- functor(B,b,9).

pos(1). pos(2). pos(3). pos(4). pos(5). pos(6). pos(7). pos(8). pos(9).

line(13). line(17). line(19). line(28). line(37). line(39). line(46). line(79).

inline(1,13). inline(2,13). inline(3,13).
inline(1,17). inline(4,17). inline(7,17).
inline(1,19). inline(5,19). inline(9,19).
inline(2,28). inline(5,28). inline(8,28).
inline(3,37). inline(5,37). inline(7,37).
inline(3,39). inline(6,39). inline(9,39).
inline(4,46). inline(5,46). inline(6,46).
inline(7,79). inline(8,79). inline(9,79).

% to play
toplay(o,S) :- toplayO(S).
toplay(x,S) :- toplayX(S).

toplayO(S):- moves_left(S,N), odd(N),!.
toplayX(S):- moves_left(S,N), even(N),!.

moves_left(s(_,_,B1),N) :-
    findall(P,(pos(P),arg(P,B1,e)),Ps),!,
    length(Ps,N).

% move generator
move(s(o,Z,B1),s(x,Z,B2)):- moveo(B1,B2).
move(s(x,Z,B1),s(o,Z,B2)):- movex(B1,B2).

moveo(B1,B2) :- board_(B1), pos(P), move(o,P,B1,B2).

movex(B1,B2) :- board_(B1), pos(P), move(x,P,B1,B2).

move(X,1,b(e,A,B,C,D,E,F,G,H),b(X,A,B,C,D,E,F,G,H)).
move(X,2,b(A,e,B,C,D,E,F,G,H),b(A,X,B,C,D,E,F,G,H)).
move(X,3,b(A,B,e,C,D,E,F,G,H),b(A,B,X,C,D,E,F,G,H)).
move(X,4,b(A,B,C,e,D,E,F,G,H),b(A,B,C,X,D,E,F,G,H)).
move(X,5,b(A,B,C,D,e,E,F,G,H),b(A,B,C,D,X,E,F,G,H)).
move(X,6,b(A,B,C,D,E,e,F,G,H),b(A,B,C,D,E,X,F,G,H)).
move(X,7,b(A,B,C,D,E,F,e,G,H),b(A,B,C,D,E,F,X,G,H)).
move(X,8,b(A,B,C,D,E,F,G,e,H),b(A,B,C,D,E,F,G,X,H)).
move(X,9,b(A,B,C,D,E,F,G,H,e),b(A,B,C,D,E,F,G,H,X)).


% drawn classifier
drawn(s(M,_,B)) :- moves_left(s(M,_,B),0), \+(won(s(M,_,B))).

% win classifier
won(s(M,_,B)) :- next_mark(M,M1),won_(M1,s(M,_,B)).

won_(X,s(_,_,B)) :- line(L), forall(inline(U,L),arg(U,B,X)).

% representation
space :- write(' ').
bar :- write('|').
pos(X,B) :- space, val(X,B,Y), write(Y), space.
val(X,B,' ') :- arg(X,B,Y),Y=e.
val(X,B,Y) :- arg(X,B,Y),Y\=e.
hline :- write('---------').

printboard(B) :- nl,
    pos(1,B), bar, pos(2,B), bar, pos(3,B), nl, hline, nl,
    pos(4,B), bar, pos(5,B), bar, pos(6,B), nl, hline, nl,
    pos(7,B), bar, pos(8,B), bar, pos(9,B), nl, nl, !.
