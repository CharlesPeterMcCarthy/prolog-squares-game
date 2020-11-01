:- ensure_loaded('inventory.pl').
:- ensure_loaded('original-item-positions.pl').

desired_items(['G', 'S', 'B']).
undesired_items(['X', 'Y', 'Z']).

check_for_dropped_item(R, C) :-
    square(R, C, Item),
    desired_items(DI),
    undesired_items(UI),
    (
        member(Item, DI) -> format('You found a desired item: ~w', [Item]), save_item_to_inventory(Item)
        ; member(Item, UI) -> format('You found an undesired item: ~w', [Item])
        ; write('You moved to an empty square')
    ),
    findall(inventory_item(_), inventory_item(_), Items),
    write(Items), nl,
    nl.

save_item_to_inventory(I) :-
    assertz(inventory_item(I)),
    persist_inventory.

empty_inventory :-
    findall(inventory_item(I), inventory_item(I), Items),
    remove_inventory_items(Items),
    persist_inventory.

remove_inventory_items([]).
remove_inventory_items([I|T]) :-
    retract(I),
    remove_inventory_items(T).

clear_original_item_positions :-
    findall(original_item_position(R, C, I), original_item_position(R, C, I), Positions),
    remove_inventory_items(Positions),
    persist_original_item_positions.

remove_original_item_position([]).
remove_original_item_position([P|T]) :-
    retract(P),
    remove_original_item_position(T).

persist_inventory :-
    tell('inventory.pl'),
    write(':- dynamic(inventory_item/1).'), nl,
    listing(inventory_item),
    told.

persist_original_item_positions :-
    tell('original-item-positions.pl'),
    write(':- dynamic(original_item_position/3).'), nl,
    listing(original_item_position),
    told.
