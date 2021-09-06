# mazegen3array.py implements the maze generation algorithm in python as an
# example and sandbox because 6502 assembly can be a beast sometimes.
# Specifically, it uses 3 separate arrays for the cells, the vertical walls,
# and the horizontal walls because using a single combined array yields an array
# size of over 700 cells.  We want to implement on the NES, and array sizes
# above 256 excessively complicate matters.  Here, the largest of our 3 arrays
# (the cells array) consumes only 195 bytes, fitting easily on an NES memory
# page.
# The NES screen is 32 8x8 tiles wide, and 30 tiles tall, but visibility of the
# top and bottom lines varies by screen and emulator, so we throw out the top
# two lines, and the bottom one line, leaving us with 27 rows to deal with, and
# we spend two of those on the walls immediately above and below the maze.

import random
import math

# Array size constants; we use one-dimensional arrays to hold two-dimensional
# data.  These numbers represent the x & y sizes of the implied two-dimensional
# arrays.
xSizeFinal = 29
ySizeFinal = 25
xSizeCells = 15
ySizeCells = 13
xSizeVertWalls = 15
ySizeVertWalls = 12
xSizeHorizWalls = 14
ySizeHorizWalls = 13

# coordinates returns x and y coordinates for the Nth element in a
# one-dimensional array if it were instead a two-dimensional array of width
# x_size.
def coordinates(number, x_size):
    x_value = number % x_size
    y_value = math.floor(number/x_size)
    return x_value, y_value

# drawer is used in the map after the maze has been generated to draw it; paths
# get represented by `.`, while walls get represented by `▓`.
def drawer(cell):
    if cell > 1:
        return "."
    else:
        return "▓"


# Step 1: Make & initialize working arrays:
theStack = []
cellsArray = [0 for i in range(xSizeCells * ySizeCells)]
vertWallsArray = [1 for j in range(xSizeVertWalls * ySizeVertWalls)]
horizWallsArray = [1 for k in range(xSizeHorizWalls * ySizeHorizWalls)]
finalArray = [
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
]


# Step 2: Pick a random cell, mark it part of the maze, and add it to the stack:
currentCell = random.randint(0, 191)
xPos, yPos = coordinates(currentCell, xSizeCells)
count = 2
cellsArray[currentCell] = count
theStack.append(currentCell)


# Step 3: While there's a stack, pop a cell off the stack as the current cell;
# if there's at least one unvisited neighbor, pick an unvisited neighbor at
# random, remove the wall between it and the current cell, mark it as visited,
# and push it to the stack:
while len(theStack) > 0:
    currentCell = theStack.pop()
    xPos, yPos = coordinates(currentCell, xSizeCells)
    direction = random.randint(0, 3)
    print("Current cell: ", currentCell)
    print("Direction: ", direction)
    print("xPos: ", xPos)
    print("yPos: ", yPos)
    for i in range(0, 4):
        if direction == 0:
            # Can we extend the maze upward?  If so, do it.
            if currentCell > (xSizeCells - 1) and cellsArray[currentCell - xSizeCells] == 0:
                count += 1
                theStack.append(currentCell)
                # remove walls
                vertWallsArray[((yPos-1) * xSizeVertWalls) + xPos] = count
                # mark cells
                cellsArray[currentCell - xSizeCells] = count
                currentCell = currentCell - xSizeCells
                theStack.append(currentCell)
                print("Picked Up")
                break
        if direction == 1:
            # Can we extend the maze downward?  If so, do it.
            if currentCell < ((xSizeCells * ySizeCells) - xSizeCells) and cellsArray[currentCell + xSizeCells] == 0:
                count += 1
                theStack.append(currentCell)
                # remove walls
                vertWallsArray[(yPos * xSizeVertWalls) + xPos] = count
                # mark cells
                cellsArray[currentCell + xSizeCells] = count
                currentCell = currentCell + xSizeCells
                theStack.append(currentCell)
                print("Picked Down")
                break
        if direction == 2:
            # Can we extend the maze leftward?  If so, do it.
            if xPos > 0 and cellsArray[currentCell - 1] == 0:
                count += 1
                theStack.append(currentCell)
                # remove walls
                horizWallsArray[(yPos * xSizeHorizWalls) + xPos - 1] = count
                # mark cells
                cellsArray[currentCell - 1] = count
                currentCell = currentCell - 1
                theStack.append(currentCell)
                print("Picked Left")
                break
        if direction == 3:
            # Can we extend the maze rightward?  If so, do it.
            if xPos < (xSizeCells - 1) and cellsArray[currentCell + 1] == 0:
                count += 1
                theStack.append(currentCell)
                # remove walls
                horizWallsArray[(yPos * xSizeHorizWalls) + xPos] = count
                # mark cells
                cellsArray[currentCell + 1] = count
                currentCell = currentCell + 1
                theStack.append(currentCell)
                print("Picked Right")
                break
        direction += 1
        if direction > 3:
            direction = 0
        print("New Direction: ", direction)
    print("The Stack: ")
    print(theStack)
    print("Cells Array:")
    padArray = [str(item).zfill(3) for item in cellsArray]
    for i in range(0, ySizeCells):
        print(padArray[i*xSizeCells: i*xSizeCells + xSizeCells])
    # input() # Uncomment this line to step through one tick at a time.


# Step 4: Assemble and display the results:
# Populate Final Array:
for index, cell in enumerate(cellsArray):
    xPos, yPos = coordinates(index, xSizeCells)
    finalArray[(xPos * 2) + (yPos * xSizeFinal * 2)] = cell
for index, cell in enumerate(vertWallsArray):
    xPos, yPos = coordinates(index, xSizeVertWalls)
    finalArray[(xPos * 2) + (yPos * xSizeFinal * 2) + xSizeFinal] = cell
for index, cell in enumerate(horizWallsArray):
    xPos, yPos = coordinates(index, xSizeHorizWalls)
    finalArray[(xPos * 2) + (yPos * xSizeFinal * 2) + 1] = cell
# Display the final results in various forms:
print("*** FINAL ARRAY ***")
padFinalArray = [str(item).zfill(3) for item in finalArray]
for i in range(0, ySizeFinal):
    print(padFinalArray[i*xSizeFinal: i*xSizeFinal + xSizeFinal])
print()
print("*** Copyable Final Array ***")
for i in range(0, ySizeFinal):
    print(*finalArray[i*xSizeFinal: i*xSizeFinal + xSizeFinal])
print()
print("Final Maze: ")
draw = list(map(drawer, finalArray))
for i in range(0, ySizeFinal):
    print(*draw[i*xSizeFinal: i*xSizeFinal + xSizeFinal])
