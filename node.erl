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
	Bound = { bound, [ { up, none }, { down, none }, { left, none }, { right, none } ] },
	Temp = { temp, InitTemp },
	loop({ Temp, Bound }).

init(InitTemp, Bound) ->

	%% start a thermal node with boundaries
	%% Bound must be: [ { Dir, Pid }, ... ]

	[ P ! { dev, { changebound, { dir:inv(D), self() } } } || { D, P } <- Bound, is_pid(P) ],

	io:format("Node ~p started with ~p °K ~n", [ self(), InitTemp ]),
	Temp = { temp, InitTemp },
	loop({ Temp, { bound, Bound } }).

loop({ { temp, Temp }, { bound, Bound } } = State) ->
	
	%% STATE:
	%%
	%% {
	%% 	{ temp, TEMPERATURE },
	%% 	{ bound, [
	%% 		{ up, PID },
	%% 		{ down, PID },
	%% 		{ left, PID },
	%% 		{ right, PID } 
	%% 	] }
	%% }

	UpTuple = lists:keyfind(up, 1, Bound),
	if not UpTuple -> Up = none; true -> { up, Up } = UpTuple end,

	DownTuple = lists:keyfind(down, 1, Bound),
	if not DownTuple -> Down = none; true -> { down, Down } = DownTuple end,

	LeftTuple = lists:keyfind(left, 1, Bound),
	if not LeftTuple -> Left = none; true -> { left, Left } = LeftTuple end,

	RightTuple = lists:keyfind(right, 1, Bound),
	if not RightTuple -> Right = none; true -> { right, Right } = RightTuple end,

	receive

		{ dev, { newstate, NewStateReq } } -> NewState = NewStateReq;

		{ dev, { newtemp, NewTemp } } -> NewState = { { temp, NewTemp }, { bound, Bound } };

		{ dev, { newbound, NewBound } } -> NewState = { { temp, Temp }, { bound, NewBound } };

		{ dev, { changebound, { Dir, NewPid } } } ->
			
			io:format(":: ~p ::~n", [self()]),

			NewBound = lists:keyreplace(Dir, 1, Bound, { Dir, NewPid }),

			io:format("::> ~p~n", [NewBound]),
			NewState = { { temp, Temp }, { bound, NewBound } };

		{ dev, pos } ->

			if Left =/= none -> Left ! { self(), { myposx, 1 } }, receive { yourposx, NX } -> X = NX end; true -> X = 0 end,
			if Up =/= none -> Up ! { self(), { myposy, 1 } }, receive { yourposy, NY } -> Y = NY end; true -> Y = 0 end,

			io:format("(~p; ~p)~n", [ X, Y ]),

			NewState = State;

		{ dev, log } ->

			io:format("====================~n"),
			io:format("thermal node PID: ~p~n", [ self() ]),
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

			if Up =/= none -> Up ! { self(), temp }, receive { Up, { temp, UT } } -> UpTemp = UT end; true -> UpTemp = '' end,
			if Down =/= none -> Down ! { self(), temp }, receive { Down, { temp, DT } } -> DownTemp = DT end; true -> DownTemp = '' end,
			if Left =/= none -> Left ! { self(), temp }, receive { Left, { temp, LT } } -> LeftTemp = LT end; true -> LeftTemp = '' end,
			if Right =/= none -> Right ! { self(), temp }, receive { Right, { temp, RT } } -> RightTemp = RT end; true -> RightTemp = '' end,

			io:format("      (~p°K)~n         |~n", [ UpTemp ]),
			io:format("(~p°K) - (~p°K) - (~p°K)~n", [ LeftTemp, Temp, RightTemp ]),
			io:format("         |~n      (~p°K)~n", [ DownTemp ]),

			NewState = State;

		{ Client, temp } when is_pid(Client) ->

			Client ! { self(), { temp, Temp } },

			NewState = State;

		{ Client, { myposx, N } } when is_pid(Client) ->
			
			if Left =/= none -> Left ! { Client, { myposx, N + 1 } }; true -> Client ! { yourposx, N } end,

			NewState = State;

		{ Client, { myposy, N } } when is_pid(Client) ->

			if Up =/= none -> Up ! { Client, { myposy, N + 1 } }; true -> Client ! { yourposy, N } end,

			NewState = State;

		Any ->

			io:format("~p is undefined for ~p~n", [ Any, self() ]),

			NewState = State

	end,

	loop(NewState).
