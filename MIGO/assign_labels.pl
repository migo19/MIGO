
%%%%% EPISODES AND LABELING

:- dynamic(episode/4).

episode(win_1,[],[],[]). episode(win_2,[],[],[]).
episode(win_3,[],[],[]). episode(win_4,[],[],[]).
episode(win_5,[],[],[]). episode(win_6,[],[],[]).
episode(win_7,[],[],[]). episode(win_8,[],[],[]).

episode(draw_1,[],[],[]). episode(draw_2,[],[],[]).
episode(draw_3,[],[],[]). episode(draw_4,[],[],[]).
episode(draw_5,[],[],[]). episode(draw_6,[],[],[]).
episode(draw_7,[],[],[]). episode(draw_8,[],[],[]).

%% if won for the learner, every move of the learner can be a positive example for the task of winning
%% practically, all moves that fail a call to win/2 are added to the training set
%% only valid if the counter is smaller than ref_counter for separated learning
update_exs([B1|Seq],M,Sw,Sd) :-
    ((learning(mixed));(counter(C),ref_counter(C1),C<C1)),
    mark(learner,M),
    depth([B1|Seq],Depth),
    assert_prog_and_prims(Sw),assert_win(Sw),
    ((\+(win_pos(Depth,Sw,s(M,M,B1))))->
    (retract_prog_and_prims(Sw),retract_win,
    update_exs_pos([B1|Seq]));
    (retract_prog_and_prims(Sw),retract_win)).

update_exs(_,M,_Sw,_Sd) :- mark(learner,M).

%% else if won for the opponent, throw any move
update_exs(_,M,_Sw,_Sd) :- mark(opponent,M).

update_exs_pos([]).
update_exs_pos([_]).
update_exs_pos([B1,B2|Game]):-
    depth([B1,B2|Game],Depth),
    mark(learner,M),
    mark(opponent,M1),
    newpred(win,P,Depth),
    Ex2 =.. [P,s(M,M,B1),s(M1,M,B2)],
    update_ex(Ex2,positive,P),
    update_exs_pos(Game).

%% if drawn, every move is a positive example for the task of drawing if the initial position is not won for o if an accurate strategy for winning has been learned
%% only valid if the counter is greater than ref_counter for separated learning
update_exs([B1|Seq],d,Sw,Sd) :-
    ((learning(mixed));(counter(C), ref_counter(C1), C>=C1)),
    assert_prog_and_prims(Sw),assert_win(Sw),
    assert_prog_and_prims(Sd),assert_draw(Sd),
    depth([B1|Seq],Depth),
    mark(learner,M),
    ((\+(win_pos(Depth,Sw,s(M,d,B1))),\+(draw_pos(Depth,Sd,s(M,d,B1))))->
    (retract_prog_and_prims(Sw),retract_win,
    retract_prog_and_prims(Sd),retract_draw,
    mark(learner,M),
    update_exs_draw([B1|Seq],M));
    (retract_prog_and_prims(Sw),retract_win,
    retract_prog_and_prims(Sd),retract_draw)).
update_exs(_,d,_,_).

update_exs_draw([],_).
update_exs_draw([_],_).
update_exs_draw([B1,B2|Game],M):-
    depth([B1,B2|Game],Depth),
    newpred(draw,P,Depth),
    next_mark(M,M1),
    Ex2 =.. [P,s(M,d,B1),s(M1,d,B2)],
    update_ex(Ex2,positive,P),
    update_exs_draw([B2|Game],M1).

update_ex(Ex,positive,P) :-
    episode(P,Pos,Neg,BK),
    \+(member(Ex,Pos)),!,
    retract(episode(P,Pos,Neg,BK)),
    assert(episode(P,[Ex|Pos],Neg,BK)),!.
update_ex(Ex,positive,P) :-
    episode(P,Pos,Neg,BK),
    member(Ex,Pos),!.

%% reclassify draw position with respect to the updated winning strategy for mixed learning
update_draw_moves(_,1) :- !.
update_draw_moves(Sw,0) :-
    assert_prog_and_prims(Sw),assert_win(Sw),
    depth_game(D),
    update_draw_moves_(1,Sw,D),
    retract_prog_and_prims(Sw),retract_win.

update_draw_moves_(N,_,N):- !.
update_draw_moves_(M,Sw,N):-
    newpred(draw,P,M),
    episode(P,Pos,Neg,BK),
    update_pos(M,Sw,Pos,Pos1),
    retractall(episode(P,_,_,_)),
    assert(episode(P,Pos1,Neg,BK)),
    M1 is M+1,
    update_draw_moves_(M1,Sw,N).

update_pos(_,_,[],[]).
update_pos(M,Sw,[Ex|Pos],Pos1):-
    Ex =.. [_,S1,_],
    win_pos(M,Sw,S1),
    update_pos(M,Sw,Pos,Pos1).
update_pos(M,Sw,[Ex|Pos],[Ex|Pos1]):-
    update_pos(M,Sw,Pos,Pos1).


%% check whether a board is a winning position with respect to the current strategy
win_pos(M,Sw,S1):-
    between(1,M,N),
    newpred(win,P,M),
    member(sub(_,P,2,_,_),Sw),
    call(P,S1,_).

%% check whether a board is covered with respect to the current drawing strategy
draw_pos(M,Sd,S1):-
    newpred(draw,P,M),
    member(sub(_,P,2,_,_),Sd),
    call(P,S1,_).
