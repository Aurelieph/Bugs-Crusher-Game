# Bugs Crusher

---

## 1. Description

Bugs Crusher is a fun and strategic puzzle game with the following features:

- **Objective:** Align 3 or more bugs (vertically or horizontally) to score points. Complete the level by reaching the target score within the allowed moves.
- **Levels:** The game consists of 5 progressively challenging levels.
- **Bonus Mechanics:** Aligning 4 or more bugs in a single line generates a special bonus item:
  - The bonus item can be moved to an adjacent tile.
  - When activated, it destroys all bugs in the same vertical and horizontal lines, providing a significant boost to your score.
- **Shuffling:** If no valid moves are available on the board, the game automatically shuffles all elements, ensuring the game continues without interruption.

---

## 2. How to Play

### Game Rules:

1. **Matching Bugs:**
   - Swap adjacent bugs to create a match of 3 or more of the same type.
   - Matches can be vertical or horizontal.
2. **Creating Bonuses:**
   - Match 4 or more bugs in a single line to create a bonus item.
   - Move the bonus item to an adjacent position to activate it.
   - Activation destroys the entire row and column, earning additional points.
3. **Score and Moves:**
   - Achieve the target score for each level within the given number of moves.
   - If the moves run out before reaching the target score, the game ends.

### Steps to Play:

1. **Start the game**.
2. **Observe the board** and plan your moves strategically.
3. **Swap tiles** to create matches and aim for larger combinations to generate bonuses.
4. **Use bonuses effectively** to clear more tiles and maximize your score.
5. **Continue** until you complete all 5 levels or exhaust your moves.
6. **Restart the game** if you want to keep having fun.

Enjoy crushing bugs and advancing through levels in this dynamic tile-matching experience!

---

## 3. Code Structure

### `Element.scala`

Defines the `Element` class, which represents individual tiles (bugs) on the game grid.

#### Key Properties:
- `value`: The type of bug.
- `isPartOfMatch`: Indicates if the tile is part of a current match.
- `bonus` and `bonusIsActivated`: Manage the bonus creation and activation mechanics.

#### Key Methods:
- `updateValue`: Updates the bug type and refreshes its display.
- `copy`: Creates a deep copy of the element.

### `Game.scala`

Acts as the entry point for the application.

- Initializes game dimensions and graphical display using the `FunGraphics` library.
- Sets up mouse event handling to process user interactions such as tile selection and moves.
- Links the game logic by instantiating the `Grid` class and managing restart actions.

### `Grid.scala`

Implements the main game logic and grid management.

#### Key Components:
- `box`: A 2D array representing the grid of bugs.
- `initializeElements`: Populates the grid with randomized bug types.
- `resolveGrid`: Handles matching, cascading, and shuffling to ensure smooth gameplay.
- `select`: Manages the selection of tiles for swapping and validates moves.
- `drawElements`: Handles rendering of the grid and animations.

#### Additional Functionalities:
- Shuffling the grid when no moves are available.
- Creating bonuses for larger matches and handling their effects.
- Managing level progression and game states.

### `Scoring.scala`

Manages the score and level mechanics.

#### Tracks:
- `score`: The player's current score.
- `goal`: The score target for the current level.
- `movesLeft`: Remaining moves for the player.

### External Resource: `fungraphics-1.5.15.jar`

- Provides graphical utilities for rendering the game interface and animations.
- Used extensively across the project for drawing elements, backgrounds, and UI components.

---

## Screenshots

![BugsCrusher1](https://github.com/user-attachments/assets/15bf33e5-7b17-43cc-8d84-dc6a68755986)

<sup><sub>
**Angry Birds Bird Image:** Used in this project for educational purposes only. The image is a property of [Rovio Entertainment](https://www.rovio.com). All rights reserved.</sub></sup>

![gameoverBugsCrusher](https://github.com/user-attachments/assets/ec1ff21d-d926-45a2-987d-718e8da1d01d)
![levelupBugsCrusher](https://github.com/user-attachments/assets/6c76ba9f-9041-4e35-93df-09e89acb09af)
![winBugsCrusher](https://github.com/user-attachments/assets/08ec4967-f611-4426-8564-62ec63dd5b0f)

