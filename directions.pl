move(north) :-
    square(R, C, 'P'),
    NewRow is R - 1,
    NewRow > 0,
    empty_square(R, C),
    check_for_dropped_item(NewRow, C),
    move_player_to_square(NewRow, C),
    write('Successfully moved North...'), nl.

move(south) :-
    square(R, C, 'P'),
    NewRow is R + 1,
    NewRow > 0,
    empty_square(R, C),
    check_for_dropped_item(NewRow, C),
    move_player_to_square(NewRow, C),
    write('Successfully moved South...'), nl.

move(east) :-
    square(R, C, 'P'),
    NewCol is C + 1,
    NewCol > 0,
    empty_square(R, C),
    check_for_dropped_item(R, NewCol),
    move_player_to_square(R, NewCol),
    write('Successfully moved East...'), nl.

move(west) :-
    square(R, C, 'P'),
    NewCol is C - 1,
    NewCol > 0,
    empty_square(R, C),
    check_for_dropped_item(R, NewCol),
    move_player_to_square(R, NewCol),
    write('Successfully moved West...'), nl.
