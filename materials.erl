-module(materials).
-compile(export_all).

%% coef units are in mm^2/s

coef(c) -> 216.6; %% c == carbon
coef(carbon) -> coef(c);

coef(ag) -> 165.63; %% ag == silver
coef(silver) -> coef(ag);

coef(au) -> 127; %% au == gold
coef(gold) -> coef(au);

coef(cu) -> 111; %% cu == copper
coef(copper) -> coef(cu);

coef(al) -> 97; %% al == aluminium
coef(aluminium) -> coef(al);

coef(si) -> 88; %% si == silicon
coef(silicon) -> coef(si);

coef(sn) -> 40; %% sn == tin
coef(tin) -> coef(sn);

coef(fe) -> 23; %% fe == iron
coef(iron) -> coef(fe);

coef(glass) -> 0.34;

coef(brick) -> 0.27;

coef(rubber) -> 0.11;

coef(Material) when is_atom(Material) ->

	Coef = 1,
	io:format("~p: ~p is an undefined material~nDiffusity is: ~p~n", [ ?FILE, Material, Coef ]),
	Coef;

coef(_) -> error(badarg).
