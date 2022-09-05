-module(nodefuns).
-compile(export_all).

beamlist([ ], BeamList) -> BeamList;

beamlist(TempList, BeamList) ->

	[ HeadTemp | TempTail ] = TempList,
	LastNode = lists:last(BeamList),

	Node = node:start(HeadTemp, [ { up, none }, { down, none }, { left, LastNode }, { right, none } ]),

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

	TheseNodes = beam(HeadTempRow), 	%% name of the year
	[ N ! { dev, { changebound, { up, UN } } } || { N, UN } <- lists:zip(TheseNodes, LastNodes) ], %% brain breaker
	
	NewNodeMatrix = NodeMatrix ++ [ TheseNodes ],
	sheetmatrix(TailTempRows, NewNodeMatrix).

sheet(TempMatrix) ->

	[ FirstTempRow | TailRows ] = TempMatrix,
	FirstNodes = beam(FirstTempRow),
	sheetmatrix(TailRows, [ FirstNodes ]).		%% devil recursion
