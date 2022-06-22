# Yesterday's Mazes

Yesterday's Mazes is a maze race game for the original Nintendo Entertainment System (NES) written in 6502 Assembly.  It uses a randomized depth-first search algorithm, and supports up to 4 simultaneous players racing to escape the maze.

The assembled file YesterdaysMazes.nes included in the repo should run in any NES emulator.

Each of the (up to) four players controls a different colored sphere through the maze using the direction pad.  The first player to escape the maze wins, and the maze screen palette changes to match that player's color.  They may then press Start to generate a new random maze and begin a new race.

To build the .nes file from source on macOS you'll need to have installed the CC65 toolchain from https://cc65.github.io/ and then simply run  
`./buildit.sh`  
in the root directory of the project.


### Acknowledgements
Many thanks to Brad Smith (http://rainwarrior.ca/) for his extensive contributions to the NES Dev community which were instrumental to these efforts.


### Recommended Reading:
bbbradsmith/NES-ca65-example: A minimal NES example using ca65  
https://github.com/bbbradsmith/NES-ca65-example

NES Assembly BitMask Collision Detection  
https://www.youtube.com/watch?v=RdFoYswNOpI

NES Controller Polling  
https://wiki.nesdev.com/w/index.php?title=Controller_reading_code

Maze Generation Algorithms  
https://en.wikipedia.org/wiki/Maze_generation_algorithm