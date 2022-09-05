-module(nodefuns).
-compile(export_all).

decomp_bound(Bound) ->

	%% tuple or false
	UpTuple = lists:keyfind(up, 1, Bound),
	DownTuple = lists:keyfind(down, 1, Bound),
	LeftTuple = lists:keyfind(left, 1, Bound),
	RightTuple = lists:keyfind(right, 1, Bound),

	%% false is none
	if not UpTuple -> Up = none; true -> { up, Up } = UpTuple end,
	if not DownTuple -> Down = none; true -> { down, Down } = DownTuple end,
	if not LeftTuple -> Left = none; true -> { left, Left } = LeftTuple end,
	if not RightTuple -> Right = none; true -> { right, Right } = RightTuple end,
	
	{ Up, Down, Left, Right }.

heatequation({ { temp, Temp }, { bound, Bound }, { supervisor, BB } }, { { diff, Coef }, { dx, DX } }, DT) ->


	{ Up, Down, Left, Right } = decomp_bound(Bound),

	%% the if cluster 1.0, not proud of it
	if
		Up =/= none -> Up ! { self(), temp }, receive { Up, { temp, UT } } -> UpTemp = UT, UC = 1 end;
		true ->	UpTemp = 0, UC = 0
	end,
	if
		Down =/= none -> Down ! { self(), temp }, receive { Down, { temp, DT } } -> DownTemp = DT, DC = 1 end;
		true ->	DownTemp = 0, DC = 0
	end,
	if
		Left =/= none -> Left ! { self(), temp }, receive { Left, { temp, LT } } -> LeftTemp = LT, LC = 1 end;
		true ->	LeftTemp = 0, LC = 0
	end,
	if
		Right =/= none -> Right ! { self(), temp }, receive { Right, { temp, RT } } -> RightTemp = RT, RC = 1 end;
		true ->	RightTemp = 0, RC = 0
	end,

	%% the core
	Counter = UC + DC + LC + RC,					%% how many neighbors ?
	BoundarySum = UpTemp + DownTemp + LeftTemp + RightTemp,
	Laplacian = ( BoundarySum - Counter * Temp ) / ( DX * DX ),	%% aka the inverse triange guy

	TempChange = Coef * Laplacian * DT,				%% the heat equation
	NewTemp = Temp + TempChange,

	{ { temp, NewTemp }, { bound, Bound }, { supervisor, BB } }.

beamlist([ ], BeamList) -> BeamList;

beamlist(TempList, BeamList) ->

	[ HeadTemp | TempTail ] = TempList,
	LastNode = lists:last(BeamList),

	Node = node:start(HeadTemp, [ { up, none }, { down, none }, { left, LastNode }, { right, none } ]),	%% i will simplify this, i promise

	NewBeamList = BeamList ++ [ Node ],
	beamlist(TempTail, NewBeamList).

beam(TempList) ->

	[ OriginTemp | TempTail ] = TempList,
	Origin = node:start(OriginTemp),
	beamlist(TempTail, [ Origin ]).		%% evil recursion

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
	sheetmatrix(TailRows, [ FirstNodes ]).	%% devil recursion
