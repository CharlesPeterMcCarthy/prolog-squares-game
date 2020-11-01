move(north) :-
    square(R, C, 'P'),
    NewRow is R - 1,
    (
        square(NewRow, C, _) -> move_player(R, C, NewRow, C, 'North')
        ; alert_wrong_direction('North')
    ).

move(south) :-
    square(R, C, 'P'),
    NewRow is R + 1,
    (
        square(NewRow, C, _) -> move_player(R, C, NewRow, C, 'South')
        ; alert_wrong_direction('South')
    ).

move(east) :-
    square(R, C, 'P'),
    NewCol is C + 1,
    (
        square(R, NewCol, _) -> move_player(R, C, R, NewCol, 'East')
        ; alert_wrong_direction('East')
    ).

move(west) :-
    square(R, C, 'P'),
    NewCol is C - 1,
    (
        square(R, NewCol, _) -> move_player(R, C, R, NewCol, 'West')
        ; alert_wrong_direction('West')
    ).

move_player(R, C, NR, NC, D) :-
    empty_square(R, C),
    check_for_dropped_item(NR, NC),
    move_player_to_square(NR, NC),
    format('Successfully moved ~w...', [D]), nl.

alert_wrong_direction(D) :-
    format('You cannot move ~w right now!', [D]), nl.
