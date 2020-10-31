:- ensure_loaded('db.pl').

start :-
    write('Welcome to the Squares game!.'), nl,
    write('Type "print." to view the board.'), nl,
    write('Type "stop." to quit.'), nl,
    nl,
    process_option.

process_option :- write('Option? '),
    read(Option),
    write(Option).
