% Calculate if point A is within D distance of point B
distance([X1, Y1], [X2, Y2], D) :- X is X2 - X1, Y is Y2 - Y1,
								   XSQ is X * X, YSQ is Y * Y,
								   DSQ is XSQ + YSQ, D is sqrt(DSQ).

% Check if the location is 6 feet close to the park goer

socialDistancing(L, PG) :- distance(L, PG, Distance),
								  Distance >= 6.

% Check if socialDistancing from every park-goer
%% Base case
checkParkGoers(_, []).
%% Recursion
checkParkGoers(L, [PG | OPG]) :- socialDistancing(L, PG), checkParkGoers(L, OPG).

% Move one space West, North, or East
%% North
move([SX, SY], [_, GSY], [EX, EY]) :- SY < GSY, EX is SX, EY is SY + 1.
%% West
move([SX, SY], [_, _], [EX, EY]) :- SX > 0, EX is SX - 1, EY is SY.
%% East
move([SX, SY], [GSX, _], [EX, EY]) :- SX < GSX, EX is SX + 1, EY is SY.

% Checks if a given object is in a list
notMember(_, []).
notMember(O, [H | T]) :- O \= H, notMember(O, T).

% Helper function for solve
	%% SX:	Starting X
	%% SY:	Starting Y
	%% EY:	Ending Y
	%% GS:	Grid size
	%% PG:	Park-goers
	%% NL:	New location
	%% SP:	Subpath
	%% V:	Visited
%% Base case
subsolve([_, SY], EY, _, _, [], _) :- SY = EY.
%% Recursion
subsolve([SX, SY], EY, GS, PG, [NL | SP], V) :- SY \= EY, move([SX, SY], GS, NL),
												notMember(NL, V), checkParkGoers(NL, PG),
												subsolve(NL, EY, GS, PG, SP, [NL | V]).

% Find a path while avoiding people
solve(SL, EY, GS, PG, [SL | P]) :- subsolve(SL, EY, GS, PG, P, []).
