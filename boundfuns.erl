-module(boundfuns).
-compile(export_all).

decomp(Bound) ->

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

comp(List) ->

	{ Up, Down, Left, Right } = decomp(List),				%% decompose
	[ { up, Up }, { down, Down }, { left, Left }, { right, Right } ].	%% compose
