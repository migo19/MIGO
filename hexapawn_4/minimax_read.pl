%%% read next board from minimax database by checking symmetry if necessary %%%

transform(L,L).
transform(L1,L2) :- symmetry_horizontal(L1,L2).

next_pos(L1,L2,M,O):-
    transform(L1,L11),
    next_pos1(M,L11,L22,O),
    transform(L22,L2),
    next_mark(M,M1),
    list_to_state_mark(L1,M,_,Sw),
    list_to_state_mark(L2,M1,_,Sb),
    move(Sw,Sb),!.

list_to_state_mark(L,M,O,S):-
    list_to_board(L,B),
    board_to_state_mark(B,M,O,S).

board_to_state_mark(B,w,M2,s(w,M2,B)).
board_to_state_mark(B,b,M2,s(b,M2,B)).
