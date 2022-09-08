-module(nodefuns).
-compile(export_all).

get_prop(none, _) -> false;

get_prop(Node, Prop) ->

	Node ! { self(), Prop },
	receive
		{ Node, { Prop, Value } } -> Value;
		_ -> false
	end.

get_temp(Node) -> get_prop(Node, temp).

get_bound(Node) -> get_prop(Node, bound).

get_cache(Node) -> get_prop(Node, cache).

is_neighbour(Node, Node) -> false;

is_neighbour(ThisNode, OtherNode) ->

	%% check if OtherNode is ThisNode's neighbour
	
	Bound = get_bound(ThisNode),
	
	lists:keymember(OtherNode, 2, Bound).

heatequation({ { temp, Temp }, { bound, Bound }, { supervisor, BB }, { cache, Cache } }, { { diff, Coef }, { dx, DX } }, DT) ->

	%% heatequation(OldState, SystemParams, DT) -> NewState

	{ Up, Down, Left, Right } = boundfuns:decomp(Bound),
	
	TempCounter = fun (Node) ->					%% get temp-counter pair
			 
		NodeTemp = get_temp(Node),
		if
			not NodeTemp -> { 0, 0 };
			true -> { NodeTemp, 1 }
		end

	end,

	{ UpTemp, UpCounter } = TempCounter(Up), 
	{ DownTemp, DownCounter } = TempCounter(Down), 
	{ LeftTemp, LeftCounter } = TempCounter(Left), 
	{ RightTemp, RightCounter } = TempCounter(Right), 

	%% the core
	Counter = UpCounter + DownCounter + LeftCounter + RightCounter,	%% how many neighbors ?
	BoundarySum = UpTemp + DownTemp + LeftTemp + RightTemp,
	Laplacian = ( BoundarySum - Counter * Temp ) / ( DX * DX ),	%% aka the inverse triangle guy

	TempChange = Coef * Laplacian * DT,				%% the heat equation
	NewTemp = Temp + TempChange,

	{ { temp, NewTemp }, { bound, Bound }, { supervisor, BB }, { cache, Cache } }.

beamlist([ ], BeamList) -> BeamList;

beamlist(TempList, BeamList) ->

	[ HeadTemp | TempTail ] = TempList,
	LastNode = lists:last(BeamList),

	Bound = boundfuns:comp([ { left, LastNode } ]),
	Node = node:start(HeadTemp, Bound),

	NewBeamList = BeamList ++ [ Node ],
	beamlist(TempTail, NewBeamList).

beam(TempList) ->

	[ OriginTemp | TempTail ] = TempList,
	Origin = node:start(OriginTemp),
	beamlist(TempTail, [ Origin ]).					%% evil recursion

sheetmatrix([ ], NodeMatrix) -> NodeMatrix;

sheetmatrix(TempMatrix, NodeMatrix) ->

	[ HeadTempRow | TailTempRows ] = TempMatrix,
	LastNodes = lists:last(NodeMatrix),

	TheseNodes = beam(HeadTempRow), 								%% name of the year
	[ N ! { dev, { changebound, { up, UN } } } || { N, UN } <- lists:zip(TheseNodes, LastNodes) ],	%% brain breaker
	
	NewNodeMatrix = NodeMatrix ++ [ TheseNodes ],
	sheetmatrix(TailTempRows, NewNodeMatrix).

sheet(TempMatrix) ->

	[ FirstTempRow | TailRows ] = TempMatrix,
	FirstNodes = beam(FirstTempRow),
	sheetmatrix(TailRows, [ FirstNodes ]).				%% devil recursion
