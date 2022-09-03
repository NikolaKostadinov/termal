-module(bigbrother).
-export([ start/0, init/0 ]).

start() ->

	%% start the supervisor process with an empty lattice of nodes

	spawn(?MODULE, init, [ ]).

init() ->
	
	%% initate the supervisor loop

	loop().

loop() ->

	%% STATE:
	%% {
	%% 	{ diff, COEF },
	%% 	{ nodes, [ PID... ] }
	%% }

	loop().
