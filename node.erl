-module(node).
-export([ start/1, init/1 ]).

start(InitTemp) -> spawn(?MODULE, init, [ InitTemp ]).

init(InitTemp) ->

	io:format("Node ~p started with ~p °K ~n", [ self(), InitTemp ]),
	Bound = { bound, [ { up, none }, { down, none }, { left, none }, { right, none } ] },
	Temp = { temp, InitTemp },
	loop({ Temp, Bound }).

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

	{ up, Up } = lists:keyfind(up, 1, Bound),
	{ down, Down } = lists:keyfind(down, 1, Bound),
	{ left, Left } = lists:keyfind(left, 1, Bound),
	{ right, Right } = lists:keyfind(right, 1, Bound),

	receive

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

		Any ->

			io:format("~p is undefined for ~p~n", [ Any, self() ]),

			NewState = State

	end,

	loop(NewState).
