-module(node).
-export([ start/1, init/1, start/2, init/2 ]).

start(InitTemp) ->
	
	%% start a thermal node with no boundaries  

	spawn(?MODULE, init, [ InitTemp ]).

start(InitTemp, Bound) ->

	%% start a thermal node with boundares
	%% Bound must be: [ { Dir, Pid }, ... ]
	
	spawn(?MODULE, init, [ InitTemp, Bound ]).

init(InitTemp) ->

	%% initate a thermal node process with no boundaries

	io:format("Node ~p started with ~p °K ~n", [ self(), InitTemp ]),
	Bound = { bound, boundfuns:comp([ ]) },
	Temp = { temp, InitTemp },
	Supervisor = { supervisor, none },
	Cache = { cache, InitTemp },

	loop({ Temp, Bound, Supervisor, Cache }).

init(InitTemp, Bound) ->

	%% start a thermal node with boundaries
	%% Bound must be: [ { Dir, Pid }, ... ]

	[ P ! { dev, { changebound, { dir:inv(D), self() } } } || { D, P } <- Bound, is_pid(P) ],	%% hello neighbors

	io:format("Node ~p started with ~p °K ~n", [ self(), InitTemp ]),
	Temp = { temp, InitTemp },
	BoundTuple = { bound, boundfuns:comp(Bound) },
	Supervisor = { supervisor, none },
	Cache = { cache, InitTemp },
	
	loop({ Temp, BoundTuple, Supervisor, Cache }).

loop({ { temp, Temp }, { bound, Bound }, { supervisor, BB }, { cache, Cache } } = State) ->
	
	%% STATE:
	%%
	%% {
	%% 	{ temp, TEMPERATURE },
	%% 	{ bound, [
	%% 		{ up, PID },
	%% 		{ down, PID },
	%% 		{ left, PID },
	%% 		{ right, PID } 
	%% 	] },
	%% 	{ supervisor, BB },
	%% 	{ cache, Cache }
	%% }

	{ Up, Down, Left, Right } = boundfuns:decomp(Bound),

	receive

		{ dev, { newstate, NewStateReq } } -> NewState = NewStateReq;

		{ dev, { newtemp, NewTemp } } -> NewState = { { temp, NewTemp }, { bound, Bound }, { supervisor, BB }, { cache, Cache } };

		{ dev, { newbound, NewBound } } -> NewState = { { temp, Temp }, { bound, NewBound }, { supervisor, BB }, { cache, Cache } };

		{ dev, { changebound, { Dir, NewPid } } } ->
			
			NewBound = lists:keyreplace(Dir, 1, Bound, { Dir, NewPid }),		%% goodbye old neighbor
			
			NewPid ! { dev, { changebound_only, { dir:inv(Dir), self() } } },	%% say hello to the new neighbor

			NewState = { { temp, Temp }, { bound, NewBound }, { supervisor, BB }, { cache, Cache } };

		{ dev, { changebound_only, { Dir, NewPid } } } ->

			NewBound = lists:keyreplace(Dir, 1, Bound, { Dir, NewPid }),		%% goodbye old neighbor, again
												%% no hello this time
			NewState = { { temp, Temp }, { bound, NewBound }, { supervisor, BB }, { cache, Cache } };

		{ dev, pos } ->

			if
				Left =/= none ->

					Left ! { self(), { myposx, 1 } },	%% give me my X postition
					receive { yourposx, NX } -> X = NX end;	%% thank you
				
				true -> X = 0					%% I am the origin ?
			end,
			if
				Up =/= none ->
					Up ! { self(), { myposy, 1 } },		%% give me my Y position
					receive { yourposy, NY } -> Y = NY end;	%% thanks again
				
				true -> Y = 0					%% I am the origin ?
			end,

			io:format("(~p; ~p)~n", [ X, Y ]),

			NewState = State;

		{ dev, log } ->

			io:format("====================~n"),
			io:format("thermal node PID: ~p~n", [ self() ]),
			io:format("supervisor: ~p~n", [ BB ]),
			io:format("temperature: ~p °K~n", [ Temp ]),
			io:format("upper node: ~p~n", [ Up ]),
			io:format("lower node: ~p~n", [ Down ]),
			io:format("left node: ~p~n", [ Left ]),
			io:format("right node: ~p~n", [ Right ]),
			io:format("====================~n"),

			NewState = State;
		
		{ dev, vlog } ->

			%%       ( )
			%%        |
			%% ( ) - ( ) - ( )
			%%        |
			%%       ( )

			Empty = n,

			%% the if cluster 2.0
			if Up =/= none -> Up ! { self(), temp }, receive { Up, { temp, UT } } -> UpTemp = UT end; true -> UpTemp = Empty end,
			if Down =/= none -> Down ! { self(), temp }, receive { Down, { temp, DT } } -> DownTemp = DT end; true -> DownTemp = Empty end,
			if Left =/= none -> Left ! { self(), temp }, receive { Left, { temp, LT } } -> LeftTemp = LT end; true -> LeftTemp = Empty end,
			if Right =/= none -> Right ! { self(), temp }, receive { Right, { temp, RT } } -> RightTemp = RT end; true -> RightTemp = Empty end,

			%% console art
			io:format("      (~p°K)~n         |~n", [ UpTemp ]),
			io:format("(~p°K) - (~p°K) - (~p°K)~n", [ LeftTemp, Temp, RightTemp ]),
			io:format("         |~n      (~p°K)~n", [ DownTemp ]),

			NewState = State;

		{ Client, temp } when is_pid(Client) ->

			Client ! { self(), { temp, Temp } },

			NewState = State;

		{ Client, supervise } -> NewState = { { temp, Temp }, { bound, Bound }, { supervisor, Client }, { cache, Cache } };

		{ BB, { evolve, { { dir, Dir }, { dt, DT } } } } ->

			%% heat equation calc tour
			
			BB ! { self(), heatrequest },						%% I have questions, Big Brother
			receive { BB, R } -> Response = R end,					%% waiting for answers, Big Brother

			NewState = nodefuns:heatequation(State, Response, DT),			%% the heat equation
			
			%% continue the tour
			DirTuple = lists:keyfind(Dir, 1, Bound),
			if
				not DirTuple -> NextNode = Down, NextDir = dir:inv(Dir);	%% going down
				true -> { Dir, NextNode } = DirTuple, NextDir = Dir		%% invert direction
			end,
			
			if
				NextNode =/= none -> NextNode ! { BB, { evolve, { { dir, NextDir }, { dt, DT } } } };
				true -> BB ! { self(), { evolve, done } }
			end;

		{ Client, cache } when is_pid(Client) ->

			Client ! { self(), { cache, Cache } },

			NewState = State;

		{ BB, { cache, reset } } -> NewState = { { temp, Temp }, { bound, Bound }, { supervisor, BB }, { cache, Temp } };

		{ Client, { myposx, N } } when is_pid(Client) ->
			
			if
				Left =/= none -> Left ! { Client, { myposx, N + 1 } };	%% pass the X position
				true -> Client ! { yourposx, N }			%% I am the last one
			end,

			NewState = State;

		{ Client, { myposy, N } } when is_pid(Client) ->

			if
				Up =/= none -> Up ! { Client, { myposy, N + 1 } };	%% pass the Y position
				true -> Client ! { yourposy, N }			%% I am also the last one
			end,

			NewState = State;

		Any ->

			io:format("~p is undefined for node: ~p~n", [ Any, self() ]),

			NewState = State

	end,

	loop(NewState).
