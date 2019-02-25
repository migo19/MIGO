%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Primitives for playing hexapawn. It is assumed
%	the board is represented by a 3 by 3 matrix.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%x is b
%o is w

%% w starts the game

mark(w).
mark(b).

next_mark(w,b).
next_mark(b,w).

board_(B):- functor(B,b,9),!.

move(s(w,Z,B1),s(b,Z,B2)):- movew(s(w,Z,B1),s(b,Z,B2)).
move(s(b,Z,B1),s(w,Z,B2)):- moveb(s(b,Z,B1),s(w,Z,B2)).

movew(s(M1,Z,B1),s(M2,Z,B2)):- next_mark(M1,M2),movew_(B1,B2).
moveb(s(M1,Z,B1),s(M2,Z,B2)):- next_mark(M1,M2),moveb_(B1,B2).

movew_(B1,B2) :- board_(B1), pos(P1), pos(P2), movew(P1/P2,B1,B2).

moveb_(B1,B2) :- board_(B1), pos(P1), pos(P2), moveb(P1/P2,B1,B2).

movew(P1/P2,B1,B2) :- board_(B1), move(w,P1/P2,B1,B2).

moveb(P1/P2,B1,B2) :- board_(B1), move(b,P1/P2,B1,B2).

move(b,1/4,b(b,A,B,e,D,E,F,G,H),b(e,A,B,b,D,E,F,G,H)).
move(b,2/5,b(A,b,B,C,e,E,F,G,H),b(A,e,B,C,b,E,F,G,H)).
move(b,3/6,b(A,B,b,C,D,e,F,G,H),b(A,B,e,C,D,b,F,G,H)).
move(b,4/7,b(A,B,C,b,D,E,e,G,H),b(A,B,C,e,D,E,b,G,H)).
move(b,5/8,b(A,B,C,D,b,E,F,e,H),b(A,B,C,D,e,E,F,b,H)).
move(b,6/9,b(A,B,C,D,E,b,F,G,e),b(A,B,C,D,E,e,F,G,b)).

move(b,1/5,b(b,A,B,C,w,E,F,G,H),b(e,A,B,C,b,E,F,G,H)).
move(b,2/4,b(A,b,B,w,D,E,F,G,H),b(A,e,B,b,D,E,F,G,H)).
move(b,2/6,b(A,b,B,C,D,w,F,G,H),b(A,e,B,C,D,b,F,G,H)).
move(b,3/5,b(A,B,b,C,w,E,F,G,H),b(A,B,e,C,b,E,F,G,H)).
move(b,4/8,b(A,B,C,b,D,E,F,w,H),b(A,B,C,e,D,E,F,b,H)).
move(b,5/7,b(A,B,C,D,b,E,w,G,H),b(A,B,C,D,e,E,b,G,H)).
move(b,5/9,b(A,B,C,D,b,E,F,G,w),b(A,B,C,D,e,E,F,G,b)).
move(b,6/8,b(A,B,C,D,E,b,F,w,H),b(A,B,C,D,E,e,F,b,H)).

move(w,9/6,b(A,B,C,D,E,e,G,H,w),b(A,B,C,D,E,w,G,H,e)).
move(w,8/5,b(A,B,C,D,e,F,G,w,I),b(A,B,C,D,w,F,G,e,I)).
move(w,7/4,b(A,B,C,e,E,F,w,G,H),b(A,B,C,w,E,F,e,G,H)).
move(w,6/3,b(A,B,e,D,E,w,G,H,I),b(A,B,w,D,E,e,G,H,I)).
move(w,5/2,b(A,e,C,D,w,F,G,H,I),b(A,w,C,D,e,F,G,H,I)).
move(w,4/1,b(e,B,C,w,E,F,G,H,I),b(w,B,C,e,E,F,G,H,I)).

move(w,9/5,b(A,B,C,D,b,F,G,H,w),b(A,B,C,D,w,F,G,H,e)).
move(w,8/6,b(A,B,C,D,E,b,G,w,I),b(A,B,C,D,E,w,G,e,I)).
move(w,8/4,b(A,B,C,b,E,F,G,w,I),b(A,B,C,w,E,F,G,e,I)).
move(w,7/5,b(A,B,C,D,b,F,w,G,H),b(A,B,C,D,w,F,e,G,H)).

move(w,6/2,b(A,b,C,D,E,w,G,H,I),b(A,w,C,D,E,e,G,H,I)).
move(w,5/1,b(b,B,C,D,w,F,G,H,I),b(w,B,C,D,e,F,G,H,I)).
move(w,5/3,b(A,B,b,D,w,F,G,H,I),b(A,B,w,D,e,F,G,H,I)).
move(w,4/2,b(A,b,C,w,E,F,G,H,I),b(A,w,C,e,E,F,G,H,I)).


pos(1). pos(2). pos(3). pos(4). pos(5). pos(6). pos(7). pos(8). pos(9).

drawn(s(M,_,B)) :- \+(move(s(M,_,B),_)).
won(s(M,_,B)) :- mark(M), next_mark(M,M1), won_(M1,s(M,_,B)),!.

won_(b,s(_,_,b(_,_,_,_,_,_,b,_,_))).
won_(b,s(_,_,b(_,_,_,_,_,_,_,b,_))).
won_(b,s(_,_,b(_,_,_,_,_,_,_,_,b))).
won_(w,s(_,_,b(w,_,_,_,_,_,_,_,_))).
won_(w,s(_,_,b(_,w,_,_,_,_,_,_,_))).
won_(w,s(_,_,b(_,_,w,_,_,_,_,_,_))).

pawns_left(M,s(_,_,B),L):-
    mark(M),
    findall(P,(pos(P),arg(P,B,M)),Ps),
    length(Ps,L).

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

