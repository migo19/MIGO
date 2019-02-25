%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                   dependent learning
%%  adapated implementation of D. Lin, E. Dechter, K. Ellis, J.B. Tenenbaum, and S.H. Muggleton. Bias reformulation for one-shot function induction. In Proceedings of the 23rd European Conference on Artificial Intelligence (ECAI 2014), pages 525-530, Amsterdam, 2014. IOS Press.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% first learn win/2 then learn draw/2
dependent_learning(N,Sw,Sd):-
    tasks(K1,K2),
    one_shot_learning_aux(win,1,K1,N,Primw,Invw,Gw),!,
    one_shot_learning_aux(draw,1,K2,N,Primd,Invd,Gd),!,
    retractall(Primw,Invw,Gw),
    retractall(Primd,Invd,Gd),
    append(Invw,Gw,Sw1),
    flatten(Sw1,Sw),
    append(Invd,Gd,Sd1),
    flatten(Sd1,Sd).

retractall(Prim,Inv,G):-
    flatten(Prim,Prim2),
    retractall_prim(Prim2),
    append(Inv,G,All),
    flatten(All,All1),
    retract_program(All1).

%% for each depth first perform one-shot learning then learn a general predicate for win_i/draw_i
one_shot_learning_aux(_,_,1,_,[],[],[]):-!.
one_shot_learning_aux(Name,M,M,N,[],[],[]).
one_shot_learning_aux(Name,M1,M,N,[Prim3|Prim],[Prog1|Prog],[G|G1]):-
    newpred(Name,P,M1),
    episode(P,Pos,Neg,_),
    append(Pos,Neg,All),
    one_shot_learning_(Name,All,M1,1,N,Prim1,Prog1),!,
    learn_task(Pos/Neg,G),
    find_prims(G,Prim2),
    append(Prim1,Prim2,Prim3),
    M2 is M1+1,
    one_shot_learning_aux(Name,M2,M,N,Prim,Prog,G1).

one_shot_learning_(_,[],_,_,_,[],[]) :-!.
one_shot_learning_(_,_,_,N,N,[],[]) :-!.
one_shot_learning_(Name,All,K,M,N,[Prim|Prims],[Prog|Prog1]):-
    one_shot_learning1(Name,All,K,Rest,M,Progs,Prim),
    flatten(Progs,Prog),
    maplist(assert_clause,Prog),
    assert_prog_prims(Prog),
    M1 is M+1,
    one_shot_learning_(Name,Rest,K,M1,N,Prims,Prog1).

%% one-shot learning: learn a rule from single example
%% remove examples that are covered by this rule
one_shot_learning1(_,[],_,[],_,[],[]) :-!.
one_shot_learning1(Name,[Ex|All],K,Rest,M,[Prog|Prog2],[PrimSet|Prims]):-
    Ex =.. [_|Args], length([Ex|All],L),
    newpred(Name,P,K,M,L), Ex2 =.. [P|Args],
    learn([Ex2],[],Prog),!,
    find_prims(Prog,PrimSet),
    check_coverage(P,[Ex|All],All2,Prog),
    one_shot_learning1(Name,All2,K,Rest,M,Prog2,Prims).
one_shot_learning1(Name,[Ex|All],K,[Ex|Rest],M,Prog,Prims):-
    one_shot_learning1(Name,All,K,Rest,M,Prog,Prims).

find_prims(Prog,PrimSet):-
    findall(P/A,(member(sub(_Name,P,A,_MetaSub,_PredTypes),Prog)),PrimSet).

check_coverage(_,[],[],_) :- !.
check_coverage(P,Exs,Exs2,G) :-
    findall(Atom1,(member(Atom1,Exs),Atom1 =.. [_|Args],Atom2 =.. [P|Args], prove_deduce([Atom2],G)),Covered),!,
    subtract(Exs,Covered,Exs2).

newpred(F,P,K,N,L) :-
    atom_codes(F,X),
    name(K,KC), name(N,NC), name(L,LC),
    append(X,[95|KC],PC),
    append(PC,[95|NC],PC2),
    append(PC2,[95|LC],PC3),
    name(P,PC3), !.

tasks(N1,N):-
    depth_game(N),
    N1 is N-1,
    tasks(win,1,N1),
    tasks(draw,1,N), !.
tasks(K1,K3):-
    tasks(win,1,K1),
    tasks(draw,1,K2),
    K3 is min(K1,K2).

tasks(Name,N1,N1) :- depth_game(N),N1 is N-1.
tasks(Name,N,M):-
    newpred(Name,Ep2,N),
    episode(Ep2,Pos,Neg,BK),
    \+(Pos = []),!,
    N1 is N+1,
    tasks(Name,N1,M).
tasks(Name,M,M):- !.

%% check whether two programs are equivalent
prog_equivalent([],[],1) :- !.
prog_equivalent(LP1,LP2,1):- rename_predicates(LP1,LP2),!.
prog_equivalent(_,_,0).

%% rename predicates in a program
rename_predicates([],[]) :- !,fail.
rename_predicates(LP1,LP2):-
    findall(P/A1,(member(sub(_,P,A1,_,_),LP1),\+(member(sub(_,P,A,_,_),LP2))),Preds1),
    findall(P/A1,(member(sub(_,P,A1,_,_),LP2),\+(member(sub(_,P,A1,_,_),LP1))),Preds2),
    match(Preds1,Preds2,Matches),
    rename_predicates(LP1,Matches,LP2).

match(Preds1,Preds2,Matches):-
    length(Preds1,L),
    match(L,Preds1,Preds2,Matches).
match(0,_Preds1,_Preds2,[]).
match(L,Preds1,Preds2,[P/A-P1/A1|Matches]):-
    L1 is L-1,
    select(P/A,Preds1,Preds11),
    select(P1/A1,Preds2,Preds22),
    match(L1,Preds11,Preds22,Matches).

rename_predicates(LP,[],LP).
rename_predicates(LP1,[P/A-P1/A1|Rest],LP2) :-
    replace_prog(P/A,P1/A,LP1,LP3),
    rename_predicates(LP3,Rest,LP2).

replace_prog(_,_,[],[]).
replace_prog(P/A,P1/A,[sub(Name,P2,A2,MetaSub,PredTypes)|T],[sub(Name,P2,A2,MetaSub1,PredTypes)|T2]):-
    \+(P2=P),
    replace_pred(P,P1,MetaSub,MetaSub1),
    replace_prog(P/A,P1/A,T,T2).
    replace_prog(P/A,P1/A,[sub(Name,P,A,MetaSub,PredTypes)|T],[sub(Name,P1,A,MetaSub1,PredTypes)|T2]):-
    replace_pred(P,P1,MetaSub,MetaSub1),
    replace_prog(P/A,P1/A,T,T2).

replace_pred(_,_,[],[]).
replace_pred(O,R,[O|T],[R|T2]) :- replace_pred(O,R,T,T2).
replace_pred(O,R,[H|T],[H|T2]) :- H \= O,replace_pred(O,R,T,T2).
