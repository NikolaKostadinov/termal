-module(bigbrother).
-export([ start/0, start/1, init/1 ]).

start(Coef) when is_number(Coef), Coef > 0 ->

	%% start the supervisor process with an arbitrary diffusivity

	spawn(?MODULE, init, [ Coef ]);

start(_) -> error(badarg).

start() ->

	%% start the supervisor process with no diffusivity
	
	spawn(?MODULE, init, [ 1 ]).

init(Coef) ->
	
	%% initate the supervisor loop

	State = { { diff, Coef }, { nodes, [ ] } },

	loop(State).

loop({ { diff, Coef }, { nodes, Nodes } } = State) ->

	%% STATE:
	%% {
	%% 	{ diff, COEF },
	%% 	{ nodes, [ PID... ] }
	%% }

	receive
	
		Any ->

			io:format("Big Brother received undefined: ~p~n", [ Any ]),
			
			NewState = State

	end,

	loop(NewState).
