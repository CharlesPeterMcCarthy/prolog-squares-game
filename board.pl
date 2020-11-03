:- ensure_loaded('squares.pl').
:- ensure_loaded('board-size.pl').

% This is called when the player choses the size of the board (Between 3 and 8 inclusive)
organiseBoard(N) :-
    setBoardSize(N),        % Store the board size
    findall(square(R, Col, Item), square(R, Col, Item), Squares),   % Find all existing squares from previous game
    removeSquares(Squares),                                         % Remove all of saved squares from the previous game
    persistBoard,
    createBoard(N, N),
    persistBoard,
    write('Board has been created.'), nl,
    !.

% Create the board rows and columns
createBoard(1, Col) :-
    createCol(1, Col),      % Called on the last row
    !.
createBoard(R, C) :-
    createCol(R, C),
    NextRow is R - 1,     % Reduce row by one
    createBoard(NextRow, C).

% Recursively creating squares - create a single square at one time (column within the current row)
createCol(R, 1) :-                  % Called for the last column of the current R (R)
    assertz(square(R, 1, e)),       % Save empty square
    !.
createCol(R, C) :-
    NextCol is C - 1,               % Reduce column number by one
    assertz(square(R, C, e)),       % Save empty square
    createCol(R, NextCol).

% Set the board size (Player chooses size).
% Size can be between 3 and 8 inclusive.
% N passed in is the size. eg. If 5 is passed in, it created a board 5x5.
% Storing this number will be used for calculating other game details, such as the amount of random items to place on the board.
setBoardSize(N) :-
    (
        board_size(_) -> retract(board_size(_)) % Look for current saved board size from the previous game. Remove it if it exists.
        ; true
    ),
    assertz(board_size(N)),                     % Save the new game board size
    tell('board-size.pl'),
    write(':- dynamic(board_size/1).'), nl,     % Persist the board size data.
    listing(board_size),
    told.

% Persist the board - Save all of the current square facts
persistBoard :-
    tell('squares.pl'),
    write(':- dynamic(square/3).'), nl,
    listing(square),
    told.

% Remove all of the saved squares
% Called before a new game begins
clearBoard :-
    findall(square(R, Col, Item), square(R, Col, Item), Squares),
    removeSquares(Squares),
    persistBoard,
    write('Board has been destroyed.'), nl,
    !.

% Recursively remove each square
removeSquares([]).
removeSquares([S|T]) :-
    retract(S),
    removeSquares(T).
