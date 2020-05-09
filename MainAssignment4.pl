% solve
    % [SX, SY] are at least 6 feet away from every element in Locations
    % [SX, SY], EndingY, and Locations are within range of [GX, GY] (the size of grid)
    % The starting point for Path is [SX, SY]
    % reverse the ReversePath since we add [PX,PY] as head
solve([SX,SY], EndingY, [GX, GY], Locations, Path) :- 
	safe(Locations, [SX, SY]), 
    legalStartEnding([SX, SY], EndingY, [GX, GY]), 
    legalLocations(Locations, [GX, GY]),
    travel([SX, SY], EndingY, [GX, GY], Locations, ReversePath, [[SX,SY]]), 
    reverse(ReversePath, Path).
    


% travel    
    % base case: if we are one step away from the goal (we only move one foot at a time)
               % and our movement/travel is satisfying both legal and safe   
travel(_, EndingY, [GX, GY], Locations, [[NX,NY],[PX,PY]|Visited], [[PX,PY]|Visited]) :- 
    NY is EndingY, NY is PY + 1, NX is PX,
    legal([GX,GY], [NX,NY], [[PX,PY]|Visited]),
    safe(Locations, [NX,NY]).
    
    % recursive case: (update Visted to [[NX,NY]|Visited])
travel([SX, SY], EndingY, [GX, GY], Locations, Path, Visited):- 
	legal([GX, GY], [NX, NY], Visited), 
	safe(Locations, [NX, NY]), 
    travel([SX, SY], EndingY, [GX, GY], Locations, Path, [[NX,NY]|Visited]).



% legal
    % 1 feet apart, within grid, not negative integer, no looping back (member)
    % legal for moving upward (north) 1 grid
legal([GX,GY],[NX,NY],[[PX,PY]|Visited]) :-
    NX is PX, NY is PY + 1,
    NX < GX, NY < GY,
    NX >= 0, NY >= 0,
    integer(NX), integer(NY),
    \+member([NX,NY],[[PX,PY]|Visited]).

    % legal for moving right (east) 1 grid
legal([GX,GY],[NX,NY],[[PX,PY]|Visited]) :-
    NX is PX + 1, NY is PY,
    NX < GX, NY < GY,
    NX >= 0, NY >= 0,
    integer(NX), integer(NY),
    \+member([NX,NY],[[PX,PY]|Visited]).

    % legal for moving left (west) 1 grid
legal([GX,GY],[NX,NY],[[PX,PY]|Visited]) :-
    NX is PX - 1, NY is PY,
    NX < GX, NY < GY,
    NX >= 0, NY >= 0,
    integer(NX), integer(NY),
    \+member([NX,NY],[[PX,PY]|Visited]).



% safe: 
    % next movement/travel is 6 feet away from every element in Locations
    % base case: 
safe([], _).

    % recursive case:
safe([[LX,LY]|Locations], [NX, NY]) :- sqrt((NX-LX)^2 + (NY-LY)^2) >=6, safe(Locations, [NX,NY]).
     


% legalStartEnding:
    % [SX, SY] and EndingY are within range of [GX, GY] (the size of grid) and are not negative integer
legalStartEnding([SX, SY], EndingY, [GX, GY]) :- 
	SX >= 0, SX < GX, integer(SX), 
    SY >= 0, SY < GY, integer(SY),
    EndingY >= 0, EndingY < GY, integer(EndingY).



% legalLocations
    % Locations are within range of [GX, GY] (the size of grid) and are not negative integer
    % base case:
legalLocations([], _).

    % recursive case:
legalLocations([[LX,LY]|Locations], [GX, GY]) :- 
    LX >= 0, LX < GX, integer(LX),
    LY >= 0, LY < GY, integer(LY),
	legalLocations(Locations, [GX, GY]).
