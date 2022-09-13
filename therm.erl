-module(therm).
-compile(export_all).

base(Start, DX, End) when is_number(Start), is_float(DX), is_number(End) ->
	
	%% create a [ START, START + DX, ... START + N . DX, ... END ] base for TPLS beam

	Length = End - Start,
	FloatingDescreteNumber = Length / DX,
	DescreteNumber = round(FloatingDescreteNumber), %% conversion wizzardary thank you erlang for not having toInt()

	[ Start + N * DX || N <- lists:seq(0, DescreteNumber) ];

base(Start, DX, End) when is_integer(Start), is_integer(DX), is_integer(End) -> lists:seq(Start, End, DX);
base(_, _, _) -> error(badarg).

mesh(X, Y) when is_list(X), is_list(Y) ->
	
	%% create a mesh from two basis for TPLS sheet
	
	[ [ { M, N } || N <- Y ] || M <- X ];

mesh(_, _) -> error(badarg).

square(X) -> mesh(X, X).

beam(F, X) when is_function(F), is_list(X) -> lists:map(F, X);
beam(_, _) -> error(badarg).

sheet(F, M) when is_function(F), is_list(M) -> [ [ F(X, Y) || { X, Y } <- R ] || R <- M ];
sheet(_, _) -> error(badarg).
