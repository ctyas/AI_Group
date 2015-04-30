/*
 *      oscar.pl
 *
 *		Students edit this program to complete the assignment.
 */


candidate_number(17655).

:-consult(wp).	% Wikipedia stuff

% Finds the nearest oracle not yet queried, returning the Cost of movement and query, and the id of the oracle
find_nearest_oracle(OraclesVisited,QCost,cost(Cost),R,Oracle):-
	my_agent(AgentID),
	query_world(agent_current_position,[AgentID,P]),
	breadth_first_nearest_oracle(OraclesVisited,[[c(0,P),P]],[],[P],R,[cost(Cost1),_Depth],_NewPos,Oracle),!,
	Cost is Cost1 + QCost.

% Finds the nearest oracle that will leave enough energy to also reach a charging station afterwards
find_oracle_charging(OraclesVisited,QCost,cost(Cost),[RHead|R],Oracle,Charging):-
	find_nearest_oracle(OraclesVisited,QCost,cost(Cost1),[RHead|R],Oracle),
	find_nearest_charging(RHead,cost(Cost2),_CR,Charging),	% Look for nearest charging station to the oracle
	Cost is Cost1 + Cost2.

% Finds the nearest charging station to position P, returning the cost and id of the charging station
find_nearest_charging(P,Cost,R,Charging):-
	breadth_first_nearest_charging([[c(0,P),P]],[],[P],R,[Cost,_Depth],_NewPos,Charging),!.
	
% Performs breadth first search to find the nearest oracle
breadth_first_nearest_oracle(OV,[H|_Lst1], _Lst2, _Visited, RPath, [cost(Cost), depth(Cost)], NewPos,Oracle) :-
	achieved_nearest_oracle(OV, H, RPath, Cost, NewPos, Oracle).
breadth_first_nearest_oracle(_OV, [], [], _Visited, _RPath, _Cost, _NewPos, _Oracle) :-
	!,
	false.
breadth_first_nearest_oracle(OV,[], Lst2, Visited, RPath, Cost, NewPos,Oracle) :-
	breadth_first_nearest_oracle(OV,Lst2, [], Visited, RPath, Cost, NewPos,Oracle).
breadth_first_nearest_oracle(OV,[Current|Lst1], Lst2, Visited, RR, Cost, NewPos,Oracle) :-
	Current = [c(_F,Pos)|_RPath],
	(  setof([R,C], (search(Pos,R,R,C), \+ memberchk(R, Visited)), Adjs) ->
		appendAll(Lst2, Adjs, Current, Visited, NewLst2, NewVisited)
	;	appendAll(Lst2, [], Current, Visited, NewLst2, NewVisited)
	),
	breadth_first_nearest_oracle(OV,Lst1, NewLst2, NewVisited, RR, Cost, NewPos,Oracle).

% Returns true if adjacent to any oracle that has not been visited, returning the id of that oracle	
achieved_nearest_oracle(OraclesVisited,Current,RPath,Cost,NewPos,N) :-
	Current = [c(Cost,NewPos)|RPath],
	RPath = [Last|_],map_adjacent(Last,_,o(N)),
	\+ member(N,OraclesVisited).
	
% Performs breadth first search for the nearest charging station	
breadth_first_nearest_charging([H|_Lst1], _Lst2, _Visited, RPath, [cost(Cost), depth(Cost)], NewPos,Charging) :-
	achieved_nearest_charging(H, RPath, Cost, NewPos, Charging).
breadth_first_nearest_charging([], [], _Visited, _RPath, _Cost, _NewPos, _Charging) :-
	!,
	false.
breadth_first_nearest_charging([], Lst2, Visited, RPath, Cost, NewPos,Charging) :-
	breadth_first_nearest_charging(Lst2, [], Visited, RPath, Cost, NewPos,Charging).
breadth_first_nearest_charging([Current|Lst1], Lst2, Visited, RR, Cost, NewPos,Charging) :-
	Current = [c(_F,Pos)|_RPath],
	(  setof([R,C], (search(Pos,R,R,C), \+ memberchk(R, Visited)), Adjs) ->
		appendAll(Lst2, Adjs, Current, Visited, NewLst2, NewVisited)
	;	appendAll(Lst2, [], Current, Visited, NewLst2, NewVisited)
	),
	breadth_first_nearest_charging(Lst1, NewLst2, NewVisited, RR, Cost, NewPos,Charging).

% Returns true if position is adjacent to a charging station
achieved_nearest_charging(Current,RPath,Cost,NewPos,N) :-
	Current = [c(Cost,NewPos)|RPath],
	RPath = [Last|_],map_adjacent(Last,_,c(N)).

% Appends the entirety of a list to another list.
appendAll(Lst, [], _Current, Visited, Lst, Visited).
appendAll(Lst, [A|Adjs], Current, Visited, NewLst, NewVisited) :-
	Current = [c(F,_)|RPath],
	A = [Point, C],
	F1 is F+C,
	appendAll([[c(F1,Point),Point|RPath]|Lst], Adjs, Current, [Point|Visited], NewLst, NewVisited).

solve_task(Task,Cost):-
	my_agent(Agent),
	query_world( agent_current_position, [Agent,P] ),
	solve_task_bt(Task,[c(0,P),P],0,R,Cost,_NewPos),!,	% prune choice point for efficiency
	reverse(R,[_Init|Path]),
	query_world( agent_do_moves, [Agent,Path] ).

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
	; Input = setup -> join_game(_A),handle_input(reset)
	; Input = status -> query_world(game_status,[S]),show_response(S),shell
	; Input = reset -> reset_game,start_game,shell
	; Input = whoami -> my_agent(A),show_response(A),shell
	; Input = [H|T] -> handle_input(H),handle_input(T),shell
	; callable(Input,G,R) -> ( call(G) -> show_response(R) ; show_response('This failed.') ),shell
	; otherwise -> show_response('Unknown command, please try again.'),shell
	).

% get input from user
get_input(Input):-
	write('? '),read(Input).

% show answer to user
show_response(R):-
	my_agent(Agent),
	( R=shell(Response)   -> writes('! '),writes(Response),writes(nl)
	; R=console(Response) -> term_to_atom(Response,A),do_command([Agent,console,A])
	; R=both(Response)    -> show_response(shell(Response)),show_response(console(Response))
	; R=agent(Response)   -> term_to_atom(Response,A),do_command([Agent,say,A])
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
callable(topup(S),(my_agent(Agent),query_world( agent_topup_energy, [Agent,S] )),agent(topup)).
callable(energy,(my_agent(Agent),query_world( agent_current_energy, [Agent,E] )),both(current_energy(E))).
callable(position,(my_agent(Agent),query_world( agent_current_position, [Agent,P] )),both(current_position(P))).
callable(ask(S,Q),(my_agent(Agent),query_world( agent_ask_oracle, [Agent,S,Q,A] )),A).
callable(Task,solve_task(Task,Cost),[console(Task),shell(term(Cost))]):-
	task(Task).

task(go(_Pos)).
task(find(_O)).	% oracle o(N) or charging station c(N)