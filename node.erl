-module(node).
-export([ start/1, init/1 ]).

start(InitTemp) -> spawn(?MODULE, init, [ InitTemp ]).

init(InitTemp) ->

	io:format("Node ~p started with ~p °K ~n", [ self(), InitTemp ]),
	loop().

loop() ->

	loop().
