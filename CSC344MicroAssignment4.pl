% a Prolog rule which determines if a coordinate is within a certain specified distance of another coordinate on a grid.
coordinate([X, Y], [A, B], SD):- sqrt((A-X)^2 + (B-Y)^2) =< SD. 