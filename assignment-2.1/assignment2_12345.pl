candidate_number(12345).


solve_task(Task,Cost) :-
  ( part(1) -> solve_task_1_3(Task, Cost)
  ; part(3) -> solve_task_1_3(Task, Cost)
  ; part(4) -> solve_task_4(Task, Cost)
  ).

%%%%%%%%%% Part 1 & 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve_task_1_3(Task,Cost) :-
  b_setval(target_pos,Task),
  (Task = go(_) -> nb_setval(opt, 1);
   otherwise -> nb_setval(opt, 0)
  ),
  agent_current_position(oscar,P),

  solve_task_astar(Task, [[c(0,0,P),P]],R,Cost,_NewPos,[]),!,
  nb_getval(opt, Opt),
  (Opt = 1 -> reverse(R,[_Init|Path]),agent_do_moves(oscar,Path);
  otherwise -> R = [Last|_],solve_task_1_3(go(Last),_)).
%%%%%%%%%% Part 1 & 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Part 4 (Optional) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve_task_4(Task,Cost):-
  b_setval(target_pos,Task),
  (Task = go(_) -> nb_setval(opt, 1);
   otherwise -> nb_setval(opt, 0) % find(o(A)) / find(c(A))
  ),
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  solve_task_astar(Task, [[c(0,0,P),P]],R,Cost,_NewPos,[]),!,

  nb_getval(opt, Opt),
  (Opt = 1 ->  reverse(R,[_Init|Path]),do_move(Agent,Path);
  Opt = 0 -> R = [Last|_],solve_task_4(go(Last),_)).

%%%%%%%%%% Part 4 (Optional) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Move the Agent step by step
% for part 4
do_move(_,[]).

do_move(Agent, [Next|Tail]):-
	query_world( agent_do_moves, [Agent,[Next]] ),
	do_move(Agent, Tail).

%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtracking depth-first search, needs to be changed to agenda-based A*
solve_task_bt(Task,Current,Depth,RPath,[cost(Cost),depth(Depth)],NewPos) :-
  achieved(Task,Current,RPath,Cost,NewPos).
solve_task_bt(Task,Current,D,RR,Cost,NewPos) :-
  Current = [c(F,P)|RPath],
  search(P,P1,R,C),
  \+ memberchk(R,RPath),  % check we have not been here already
  D1 is D+1,
  F1 is F+C,
  solve_task_bt(Task,[c(F1,P1),R|RPath],D1,RR,Cost,NewPos).  % backtrack search


%% A star algorithm
% begin-agenda-based A*
% g is the cost it took to get to the node
% h is our guess as to how much it'll cost to reach the goal from that node

solve_task_astar(Task,[Current|_],RPath,Cost,NewPos,_) :-
	achieved_astar(Task,Current,RPath,Cost,NewPos).

solve_task_astar(Task,[Current|Agenda],RR,Cost,NewPos,Closelist) :-
	Current = [c(_,G,P)|RPath],
	%write('begin solve a star - Current: '), writeln(Current),
	G1 is G + 1,

	(setof([c(F1,G1,Pos1),Pos1|RPath], search_astar(P,Pos1,F1,G1,Closelist), Children)
	-> merge(Agenda, Children, NewAgenda);
	NewAgenda = Agenda),

	exclude(astar_memberchk(Closelist), NewAgenda, FilteredAgenda),

	solve_task_astar(Task,FilteredAgenda,RR,Cost, NewPos,[P|Closelist]).

astar_memberchk(CList, Ele) :-
	Ele = [c(_,_,P)|_],
    memberchk(P, CList).

achieved_astar(go(Exit),Current,RPath,Cost,NewPos) :-
  Current = [c(_,Cost,NewPos)|RPath],
  %write('Cost:'), writeln(Cost),
  ( Exit=none -> true
  ; otherwise -> memberchk(Exit, RPath)
  ).

achieved_astar(find(O),Current,RPath,Cost,NewPos) :-
  Current = [c(_,Cost,NewPos)|RPath],
  %write('achieved_astar find(O) - RPath: '), writeln(RPath),
  ( O=none    -> true
  ; otherwise -> memberchk(Last, RPath),map_adjacent(Last,_,O)
  ),
  (O = o(_) -> (part(3) -> \+ agent_check_oracle(oscar, O)
  ; part(4) -> my_agent(Agent),\+ query_world(agent_check_oracle,[Agent, O]));
  otherwise -> true).
  % check if this oracle is visited
% end-agenda-based A*


achieved(go(Exit),Current,RPath,Cost,NewPos) :-
  Current = [c(Cost,NewPos)|RPath],
  ( Exit=none -> true
  ; otherwise -> RPath = [Exit|_]
  ).
achieved(find(O),Current,RPath,Cost,NewPos) :-
  Current = [c(Cost,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).



search(F,N,N,1) :-
  map_adjacent(F,N,empty).

search_adjacent(X):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  map_adjacent(P,_,X).

cal_manhattan_dis(Pos, Goal, Dist):-
	Pos = p(X_0,Y_0),
	Goal = p(X_1,Y_1),
	Dist is abs(X_0-X_1) + abs(Y_0-Y_1).

search_astar(P,N,F1,G1,Closelist):-
	map_adjacent(P,N,empty),
	%write('search_astar - N: '), writeln(N),

	\+ memberchk(N,Closelist),
	nb_getval(opt, Opt),
	%write('search_astar_Opt: '), writeln(Opt),

	(Opt = 1 -> b_getval(target_pos, go(Goal)),
	cal_manhattan_dis(N, Goal, Dist),
	H is Dist;
	otherwise -> H is 0),
	%write('search_astar_H : '), writeln(H),

	F1 is H + G1.
