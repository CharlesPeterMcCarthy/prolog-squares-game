% Direction facts
direction(north).
direction(south).
direction(east).
direction(west).

% The following 4 predicates allow the user to move around the board in certain directions
% Depending on the direction chose, the row or column is changed
% The square they are moving from is marked as empty (e)
% The square they move to is marked with the player symbol (P)
% If a desireable item is found, they pick it up
% If an undesireable item is found, they must drop a desireable item they hold in their inventory
% If a direction is not accessible, eg. If the player is at the very top of the board and they try to move North, they will receive a warning

move(north) :-
    square(R, C, 'P'),
    NR is R - 1,            % NR (New Row) is decreased to move upwards
    (
        square(NR, C, _) -> move_player(R, C, NR, C, 'North')
        ; alert_wrong_direction('North')
    ).

move(south) :-
    square(R, C, 'P'),
    NR is R + 1,            % NR (New Row) is increased to move downwards
    (
        square(NR, C, _) -> move_player(R, C, NR, C, 'South')
        ; alert_wrong_direction('South')
    ).

move(east) :-
    square(R, C, 'P'),
    NC is C + 1,            % NC (New Column) is increased to move to the right
    (
        square(R, NC, _) -> move_player(R, C, R, NC, 'East')
        ; alert_wrong_direction('East')
    ).

move(west) :-
    square(R, C, 'P'),
    NC is C - 1,            % NC (New Column) is decreased to move to the left
    (
        square(R, NC, _) -> move_player(R, C, R, NC, 'West')
        ; alert_wrong_direction('West')
    ).

move_player(R, C, NR, NC, D) :-
    format('Successfully moved ~w.', [D]), nl, nl,  % Print message to player
    empty_square(R, C),                             % Empty the square the player is moving from - Need to do this first before moving the Player symbol to prevent a bug
    check_for_dropped_item(NR, NC),                 % Looks for possible items that have been dropped
    move_player_to_square(NR, NC).                  % Move player to new square

alert_wrong_direction(D) :-
    format('You cannot move ~w right now!', [D]), nl.   % Notify user when they attempt to move to a direction that is not accessible
