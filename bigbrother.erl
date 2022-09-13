-module(bigbrother).
-export([ start/0, start/1, init/1 ]).

-define(DEFAULT_MATERIAL, iron).

start(Material) when is_atom(Material) ->

	%% start the supervisor process with an arbitrary diffusivity

	spawn(?MODULE, init, [ Material ]);

start(_) -> error(badarg).

start() ->

	%% start the supervisor process with no diffusivity
	
	spawn(?MODULE, init, [ ?DEFAULT_MATERIAL ]).

init(Material) ->
	
	%% initate the supervisor loop

	Coef = materials:coef(Material), 
	State = { { diff, Coef }, { dx, 1 }, { nodes, [ ] } },

	io:format("====================~n"),
	io:format("Big Brother: ~p started~n", [ self() ]),
	io:format("material: ~p~n", [ Material ]),
	io:format("diffusity: ~p mm^2/s~n", [ Coef ]),
	io:format("====================~n"),

	process_flag(trap_exit, true),
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
			[ link(N) || N <- NewNodes ],
			[ N ! { self(), supervise } || N <- NewNodes ],

			NewState = { { diff, Coef }, { dx, DX }, { nodes, NewNodes } };
		
		{ dev, { start, { sheet, TempMatrix } } } ->

			NodeMatrix = nodefuns:sheet(TempMatrix),
			NewNodes = lists:flatten(NodeMatrix),
			[ link(N) || N <- NewNodes ],
			[ N ! { self(), supervise } || N <- NewNodes ],

			NewState = { { diff, Coef }, { dx, DX }, { nodes, NewNodes } };

		{ dev, { set, { basis, X } } } when is_list(X) ->

			[ X0, X1 | _ ] = X,	%% not the best way, but works for basis
			NewDX = X1 - X0,

			NewState = { { diff, Coef }, { dx, NewDX }, { nodes, Nodes } };


		{ dev, { evolve, DT } } when is_number(DT) ->

			[ Origin | _ ] = Nodes,
			Origin ! { self(), { evolve, { { dir, right }, { dt, DT } } } },
			
			NewState = State;
	
		{ dev, { nevolve, 1, DT } } when is_number(DT) ->

			self() ! { dev, { evolve, DT } },
			
			NewState = State;

		{ dev, { nevolve, N, DT } } when is_number(DT), is_integer(N) ->

			%% sorry doesn't work right now
			%% i need more time for new logic

			self() ! { dev, { evolve, DT } },
			self() ! { dev, { nevolve, N - 1, DT } },

			NewState = State;

		{ dev, log } ->

			io:format("====================~n"),
			io:format("Big Brother PID: ~p~n", [ self() ]),
			io:format("nodes: ~p~n", [ Nodes ]),
			io:format("diff: ~p~n", [ Coef ]),
			io:format("dx: ~p~n", [ DX ]),
			io:format("====================~n"),

			NewState = State;
		
		{ dev, vlog } ->

			[ Origin | _ ] = Nodes,
			Origin ! { dev, { vlog, { row, Origin } } },

			NewState = State;

		{ Client, { evolve, done } } when is_pid(Client) ->

			[ N ! { self(), { cache, reset } } || N <- Nodes ],
			io:format("DONE~n"),

			NewState = State;
		
		{ Client, heatrequest } when is_pid(Client) ->

			Client ! { self(), { { diff, Coef }, { dx, DX } } },

			NewState = State;
		
		{ 'EXIT', Node, _ } ->

			Neighbours = [ N || N <- Nodes, nodefuns:is_neighbour(N, Node) ],
			NeighbourDirs = [ nodefuns:rel_dir(N, Node) || N <- Neighbours ],
			NeighbourTemps = [ nodefuns:get_temp(N) || N <- Neighbours ],

			NewTemp = lists:sum(NeighbourTemps) / length(NeighbourTemps),				%% get average temp
			NewBound = [ { dir:inv(D), N } || { D, N } <- lists:zip(NeighbourDirs, Neighbours) ],	%% get new node bounds

			NewNode = node:start(NewTemp, NewBound),
			link(NewNode),
			NewNodes = ( Nodes -- [ Node ] ) ++ [ NewNode ],					%% replace old node

			NewState = { { diff, Coef }, { dx, DX }, { nodes, NewNodes } };

		Any ->

			io:format("Big Brother received undefined: ~p~n", [ Any ]),
			
			NewState = State

	end,

	loop(NewState).
