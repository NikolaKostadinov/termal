-module(bigbrother).
-export([ start/0, start/1, init/1 ]).

start(Material) when is_atom(Material) ->

	%% start the supervisor process with an arbitrary diffusivity

	spawn(?MODULE, init, [ Material ]);

start(_) -> error(badarg).

start() ->

	%% start the supervisor process with no diffusivity
	
	spawn(?MODULE, init, [ iron ]).

init(Material) ->
	
	%% initate the supervisor loop

	Coef = materials:coef(Material), 
	State = { { diff, Coef }, { dx, 1 }, { nodes, [ ] } },

	io:format("====================~n"),
	io:format("Big Brother: ~p started~n", [ self() ]),
	io:format("material: ~p~n", [ Material ]),
	io:format("diffusity: ~p mm^2/s~n", [ Coef ]),
	io:format("====================~n"),

	loop(State).

loop({ { diff, Coef }, { dx, DX }, { nodes, Nodes } } = State) ->

	%% STATE:
	%% {
	%% 	{ diff, COEF },
	%% 	{ dx, DX }
	%% 	{ nodes, [ PID... ] }
	%% }

	receive
		
		{ dev, { start, { beam, TempList } } } ->

			NewNodes = nodefuns:beam(TempList),
			
			NewState = { { diff, Coef }, { dx, DX }, { nodes, NewNodes } };
		
		{ dev, { start, { sheet, TempMatrix } } } ->

			NodeMatrix = nodefuns:sheet(TempMatrix),
			NewNodes = lists:flatten(NodeMatrix),

			NewState = { { diff, Coef }, { dx, DX }, { nodes, NewNodes } };

		{ Client, diff } when is_pid(Client) ->

			Client ! { self(), { diff, Coef } },

			NewState = State;

		Any ->

			io:format("Big Brother received undefined: ~p~n", [ Any ]),
			
			NewState = State

	end,

	[ link(N) || N <- Nodes ],

	loop(NewState).
