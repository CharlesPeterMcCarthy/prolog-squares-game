### Squares Game

The program will be a board with a set of squares NxN in size.

The player can move around the board.

The aim of the game is to collect "desired items" such as Gold, Silver and Bronze by moving to squares containing those items.

The player must avoid squares containing "undesirable items" such as Glass, Rock and Wood. 
Landing on a square containing one of these items will cause the player to lose a desired item contained within their inventory.

The game ends when the player has collected all desired items.


### How To Play

- Start Swi-Prolog `swipl`
- Compile program `[game].`
- Start the game `start.`
- Follow the on-screen instructions.

### dynamic(taken_item_position/3)
- These are used to track where items were picked up by the user during the game.
- If the player finds an undsireable item, they must drop one of their desired items contained in their inventory.
- The item dropped must be returned to its original position.
- These taken_item_position/3 facts will keep a track of those locations.

### dynamic(board_size/1)
- This will be a single fact to keep the size of the board.
- This will be used to calculate other details throughout the game such as the amount of items that can be dropped.
- This is reset when a player chooses the size of the board at the start of a new game.

### dynamic(inventory_item/1)
- This is a desired item the player has picked up from a square on the board.
- The item is kept in their inventory until they are forced to "drop" the item when they encounter an undesired item.
- All inventory items are removed from the player's inventory when a new game begins.
