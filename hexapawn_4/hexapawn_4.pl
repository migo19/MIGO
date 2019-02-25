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

board_(B):- functor(B,b,16),!.

move(s(w,Z,B1),s(b,Z,B2)):- movew(s(w,Z,B1),s(b,Z,B2)).
move(s(b,Z,B1),s(w,Z,B2)):- moveb(s(b,Z,B1),s(w,Z,B2)).

movew(s(M1,Z,B1),s(M2,Z,B2)):- next_mark(M1,M2),movew_(B1,B2).
moveb(s(M1,Z,B1),s(M2,Z,B2)):- next_mark(M1,M2),moveb_(B1,B2).

movew_(B1,B2) :- board_(B1), pos(P1), pos(P2), movew(P1/P2,B1,B2).

moveb_(B1,B2) :- board_(B1), pos(P1), pos(P2), moveb(P1/P2,B1,B2).

movew(P1/P2,B1,B2) :- board_(B1), move(w,P1/P2,B1,B2).

moveb(P1/P2,B1,B2) :- board_(B1), move(b,P1/P2,B1,B2).

allowed_move_be(1/5). allowed_move_be(2/6).  allowed_move_be(3/7).      allowed_move_be(4/8).  allowed_move_be(5/9).  allowed_move_be(6/10). allowed_move_be(7/11).     allowed_move_be(8/12).  allowed_move_be(9/13). allowed_move_be(10/14).    allowed_move_be(11/15).   allowed_move_be(12/16).

allowed_move_we(13/9).  allowed_move_we(14/10). allowed_move_we(15/11).  allowed_move_we(16/12). allowed_move_we(9/5).  allowed_move_we(10/6).   allowed_move_we(11/7).  allowed_move_we(12/8).  allowed_move_we(5/1).  allowed_move_we(6/2).    allowed_move_we(7/3).  allowed_move_we(8/4).

allowed_move_bw(1/6).  allowed_move_bw(2/5). allowed_move_bw(2/7).      allowed_move_bw(3/6).   allowed_move_bw(3/8).  allowed_move_bw(4/7). allowed_move_bw(5/10).  allowed_move_bw(6/9).  allowed_move_bw(6/11).  allowed_move_bw(7/10). allowed_move_bw(7/12).  allowed_move_bw(8/11).   allowed_move_bw(9/14).  allowed_move_bw(10/13). allowed_move_bw(10/15).  allowed_move_bw(11/14). allowed_move_bw(11/16).  allowed_move_bw(12/15).

allowed_move_wb(13/10).   allowed_move_wb(14/9).   allowed_move_wb(14/11).   allowed_move_wb(15/10).   allowed_move_wb(15/12).   allowed_move_wb(16/11).   allowed_move_wb(9/6).   allowed_move_wb(10/5).   allowed_move_wb(10/7).   allowed_move_wb(11/6).   allowed_move_wb(11/8).   allowed_move_wb(12/7).   allowed_move_wb(5/2).   allowed_move_wb(6/1).   allowed_move_wb(6/3).   allowed_move_wb(7/2).   allowed_move_wb(7/4).   allowed_move_wb(8/3).


move(b,I/J,B1,B2) :-
    pos(I), pos(J), allowed_move_be(I/J), board_(B1), arg(I,B1,b), arg(J,B1,e), B1 =.. [b|L1], replace(L1,I,e,L2), replace(L2,J,b,L3), B2 =.. [b|L3].

move(w,I/J,B1,B2) :-
pos(I), pos(J), allowed_move_we(I/J), board_(B1), arg(I,B1,w), arg(J,B1,e), B1 =.. [b|L1], replace(L1,I,e,L2), replace(L2,J,w,L3), B2 =.. [b|L3].

move(b,I/J,B1,B2) :-
pos(I), pos(J), allowed_move_bw(I/J), board_(B1), arg(I,B1,b), arg(J,B1,w), B1 =.. [b|L1], replace(L1,I,e,L2), replace(L2,J,b,L3), B2 =.. [b|L3].

move(w,I/J,B1,B2) :-
pos(I), pos(J), allowed_move_wb(I/J), board_(B1), arg(I,B1,w), arg(J,B1,b), B1 =.. [b|L1], replace(L1,I,e,L2), replace(L2,J,w,L3), B2 =.. [b|L3].

replace([_|T], 1, X, [X|T]) :-!.
replace([H|T], I, X, [H|R]):-
    I > 1,
    NI is I-1,
    replace(T, NI, X, R), !.
replace(L, I, _, L) :- I < 1 ,!.

pos(1). pos(2). pos(3). pos(4). pos(5). pos(6). pos(7). pos(8). pos(9).  pos(10). pos(11). pos(12).  pos(13). pos(14). pos(15). pos(16).

notwono(B) :- not(wono(B)).

notwonx(B) :- not(wonx(B)).

drawn(s(M,_,B)) :- \+(move(s(M,_,B),_)).
won(s(M,_,B)) :- next_mark(M,M1), won_(M1,s(M,_,B)),!.

won_(b,s(_,_,b(_,_,_,_,_,_,_,_,_,_,_,_,b,_,_,_))).
won_(b,s(_,_,b(_,_,_,_,_,_,_,_,_,_,_,_,_,b,_,_))).
won_(b,s(_,_,b(_,_,_,_,_,_,_,_,_,_,_,_,_,_,b,_))).
won_(b,s(_,_,b(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,b))).
won_(w,s(_,_,b(w,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))).
won_(w,s(_,_,b(_,w,_,_,_,_,_,_,_,_,_,_,_,_,_,_))).
won_(w,s(_,_,b(_,_,w,_,_,_,_,_,_,_,_,_,_,_,_,_))).
won_(w,s(_,_,b(_,_,_,w,_,_,_,_,_,_,_,_,_,_,_,_))).

pawns_left(M,s(_,_,B),L):-
    mark(M),
    findall(P,(pos(P),arg(P,B,M)),Ps),
    length(Ps,L).

space :- write(' ').
bar :- write('|').
pos(X,B) :- space, val(X,B,Y), write(Y), space.
val(X,B,' ') :- arg(X,B,Y),Y=e.
val(X,B,Y) :- arg(X,B,Y),Y\=e.
hline :- write('----------------').

printboard(B) :- nl,
	pos(1,B), bar, pos(2,B), bar, pos(3,B), bar, pos(4,B), nl, hline, nl,
	pos(5,B), bar, pos(6,B), bar, pos(7,B), bar, pos(8,B), nl, hline, nl,
	pos(9,B), bar, pos(10,B), bar, pos(11,B), bar, pos(12,B),  nl, hline, nl,
    pos(13,B), bar, pos(14,B), bar, pos(15,B), bar, pos(16,B), nl, nl, !.

