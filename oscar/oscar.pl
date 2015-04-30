/*
 *      oscar.pl
 *
 *		Students edit this program to complete the assignment.
 */

:-consult(wp).
candidate_number(17655).

find_task(Task,Cost,RHead):-
	agent_current_position(oscar,P),
	breadth_first(Task,[[c(0,P),P]],[],[P],[RHead|R],[Cost,Depth],_NewPos),!.	% prune choice point for efficiency

find_nearest_oracle(OraclesVisited,QCost,cost(Cost),R,Oracle):-
	agent_current_position(oscar,P),
	breadth_first_nearest_oracle(OraclesVisited,[[c(0,P),P]],[],[P],R,[cost(Cost1),Depth],_NewPos,Oracle),!,
	Cost is Cost1 + QCost.
	
find_oracle_charging(OraclesVisited,QCost,cost(Cost),[RHead|R],Oracle,Charging):-
	find_nearest_oracle(OraclesVisited,QCost,cost(Cost1),[RHead|R],Oracle),
	find_nearest_charging(RHead,cost(Cost2),CR,Charging),
	Cost is Cost1 + Cost2.

find_nearest_charging(P,Cost,R,Charging):-
	breadth_first_nearest_charging([[c(0,P),P]],[],[P],R,[Cost,Depth],_NewPos,Charging),!.
	
breadth_first_nearest_oracle(OV,[H|Lst1], Lst2, Visited, RPath, [cost(Cost), depth(Cost)], NewPos,Oracle) :-
	achieved_nearest_oracle(OV, H, RPath, Cost, NewPos, Oracle).
breadth_first_nearest_oracle(OV, [], [], Visited, RPath, Cost, NewPos, Oracle) :-
	!,
	false.
breadth_first_nearest_oracle(OV,[], Lst2, Visited, RPath, Cost, NewPos,Oracle) :-
	breadth_first_nearest_oracle(OV,Lst2, [], Visited, RPath, Cost, NewPos,Oracle).
breadth_first_nearest_oracle(OV,[Current|Lst1], Lst2, Visited, RR, Cost, NewPos,Oracle) :-
	Current = [c(F,Pos)|RPath],
	(  setof([R,C], (search(Pos,R,R,C), \+ memberchk(R, Visited)), Adjs) ->
		appendAll(Lst2, Adjs, Current, Visited, NewLst2, NewVisited)
	;	appendAll(Lst2, [], Current, Visited, NewLst2, NewVisited)
	),
	breadth_first_nearest_oracle(OV,Lst1, NewLst2, NewVisited, RR, Cost, NewPos,Oracle).
	
achieved_nearest_oracle(OraclesVisited,Current,RPath,Cost,NewPos,N) :-
	Current = [c(Cost,NewPos)|RPath],
	RPath = [Last|_],map_adjacent(Last,_,o(N)),
	\+ member(N,OraclesVisited).
	
	
breadth_first_nearest_charging([H|Lst1], Lst2, Visited, RPath, [cost(Cost), depth(Cost)], NewPos,Charging) :-
	achieved_nearest_charging(H, RPath, Cost, NewPos, Charging).
breadth_first_nearest_charging([], [], Visited, RPath, Cost, NewPos, Charging) :-
	!,
	false.
breadth_first_nearest_charging([], Lst2, Visited, RPath, Cost, NewPos,Charging) :-
	breadth_first_nearest_charging(Lst2, [], Visited, RPath, Cost, NewPos,Charging).
breadth_first_nearest_charging([Current|Lst1], Lst2, Visited, RR, Cost, NewPos,Charging) :-
	Current = [c(F,Pos)|RPath],
	(  setof([R,C], (search(Pos,R,R,C), \+ memberchk(R, Visited)), Adjs) ->
		appendAll(Lst2, Adjs, Current, Visited, NewLst2, NewVisited)
	;	appendAll(Lst2, [], Current, Visited, NewLst2, NewVisited)
	),
	breadth_first_nearest_charging(Lst1, NewLst2, NewVisited, RR, Cost, NewPos,Charging).
	
achieved_nearest_charging(Current,RPath,Cost,NewPos,N) :-
	Current = [c(Cost,NewPos)|RPath],
	RPath = [Last|_],map_adjacent(Last,_,c(N)).

solve_task(Task,Cost):-
	agent_current_position(oscar,P),
	writeln(P),
	breadth_first(Task,[[c(0,P),P]],[],[P],R,Cost,_NewPos),!,	% prune choice point for efficiency
	reverse(R,[_Init|Path]),
	agent_do_moves(oscar,Path).

%% backtracking depth-first search, needs to be changed to agenda-based A*
solve_task_bt(Task,Current,Depth,RPath,[cost(Cost),depth(Depth)],NewPos) :- 
	achieved(Task,Current,RPath,Cost,NewPos).
solve_task_bt(Task,Current,D,RR,Cost,NewPos) :-
	Current = [c(F,P)|RPath],
	search(P,P1,R,C),
	\+ memberchk(R,RPath), % check we have not been here already
	D1 is D+1,
	F1 is F+C,
	solve_task_bt(Task,[c(F1,P1),R|RPath],D1,RR,Cost,NewPos). % backtracking search
% List of points, with their associated min costs and it's path
% [c(Cost, Point, PrevPointInPath)
breadth_first(Task, [H|Lst1], Lst2, Visited, RPath, [cost(Cost), depth(Cost)], NewPos) :-
	achieved(Task, H, RPath, Cost, NewPos).
breadth_first(Task, [], [], Visited, RPath, Cost, NewPos) :-
	!,
	false.
breadth_first(Task, [], Lst2, Visited, RPath, Cost, NewPos) :-
	breadth_first(Task, Lst2, [], Visited, RPath, Cost, NewPos).
breadth_first(Task, [Current|Lst1], Lst2, Visited, RR, Cost, NewPos) :-
	Current = [c(F,Pos)|RPath],
	(  setof([R,C], (search(Pos,R,R,C), \+ memberchk(R, Visited)), Adjs) ->
		appendAll(Lst2, Adjs, Current, Visited, NewLst2, NewVisited)
	;	appendAll(Lst2, [], Current, Visited, NewLst2, NewVisited)
	),
	breadth_first(Task, Lst1, NewLst2, NewVisited, RR, Cost, NewPos).
appendAll(Lst, [], Current, Visited, Lst, Visited).
appendAll(Lst, [A|Adjs], Current, Visited, NewLst, NewVisited) :-
	Current = [c(F,_)|RPath],
	A = [Point, C],
	F1 is F+C,
	appendAll([[c(F1,Point),Point|RPath]|Lst], Adjs, Current, [Point|Visited], NewLst, NewVisited).
	
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


search(F,N,N,1):-
	map_adjacent(F,N,empty).


%%% command shell %%%

shell:-
	get_input(Input),
	handle_input(Input).

handle_input(Input):-
	( Input = stop -> true
	; Input = reset -> ailp_reset,shell
	; Input = [H|T] -> handle_input(H),handle_input(T),shell
	; callable(Input,G,R) -> ( call(G) -> show_response(R) ; show_response('This failed.') ),shell
	; otherwise -> show_response('Unknown command, please try again.'),shell
	).

% get input from user
get_input(Input):-
	write('? '),read(Input).

% show answer to user
show_response(R):-
	( R=shell(Response)   -> writes('! '),writes(Response),writes(nl)
	; R=console(Response) -> term_to_atom(Response,A),do_command([oscar,console,A])
	; R=both(Response)    -> show_response(shell(Response)),show_response(console(Response))
	; R=agent(Response)   -> term_to_atom(Response,A),do_command([oscar,say,A])
	; R=[H|T]             -> show_response(H),show_response(T)
	; R=[]                -> true
	; otherwise           -> writes(['! ',R])
	).

writes(A):-
	( A=[]      -> nl
	; A=nl      -> nl
	; A=[H|T]   -> writes(H),writes(T)
	; A=term(T) -> write(T)
	; otherwise -> write(A)
	).

% callable(+Command, +Goal, ?Response)
callable(call(G),call(G),G).
callable(topup(S),agent_topup_energy(oscar,S),agent(topup)).
callable(energy,agent_current_energy(oscar,E),both(current_energy(E))).
callable(position,agent_current_position(oscar,P),both(current_position(P))).
callable(ask(S,Q),agent_ask_oracle(oscar,S,Q,A),A).
callable(Task,solve_task(Task,Cost),[console(Task),shell(term(Cost))]):-
	task(Task).

task(go(_Pos)).
task(find(_O)).	% oracle o(N) or charging station c(N)
