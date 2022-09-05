-module(ther).
-compile(export_all).

basis(Start, DX, End) when is_number(Start), is_float(DX), is_number(End) ->
	
	Length = End - Start,
	FloatingDescreteNumber = Length / DX,
	DescreteNumber = round(FloatingDescreteNumber),

	[ Start + N * DX || N <- lists:seq(0, DescreteNumber) ]; %% a little conversion trick

basis(Start, DX, End) when is_integer(Start), is_integer(DX), is_integer(End) -> lists:seq(Start, End, DX);
basis(_, _, _) -> error(badarg).

beam(F, X) when is_function(F), is_list(X) -> lists:map(F, X);
beam(_, _) -> error(badarg).
