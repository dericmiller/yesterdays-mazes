# mazegen.py implements the maze generation algorithm in python as an example
# and sandbox because 6502 assembly can be a beast sometimes.
# The NES screen is 32 8x8 tiles wide, and 30 tiles tall, but visibility of the
# top and bottom lines varies by screen and emulator, so we throw out the top
# two lines, and the bottom one line, leaving us with 27 lines to deal with, and
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

# Step 2: pick a random cell, mark it part of the maze, and add it to the stack:
currentCell = random.randint(0, 191)
xPos, yPos = coordinates(currentCell, xSizeCells)
count = 2
finalArray[(xPos * 2) + (yPos * xSizeFinal * 2)] = count
theStack.append(currentCell)

# Step 3: While there's a stack, pop a cell off the stack as the current cell;
# if there's at least one unvisited neighbor, pick an unvisited neighbor at
# random, remove the wall between it and the current cell, mark it as visited,
# and push it to the stack:
while len(theStack) > 0:
    currentCell = theStack.pop()
    xPos, yPos = coordinates(currentCell, xSizeCells)
    finalArray[(xPos * 2) + (yPos * xSizeFinal * 2)] = count
    direction = random.randint(0, 3)
    print("Current cell: ", currentCell)
    print("Direction: ", direction)
    print("xPos: ", xPos)
    print("yPos: ", yPos)
    for i in range(0, 4):
        if direction == 0:
            # Can we extend the maze upward?  If so, do it.
            if currentCell > (xSizeCells - 1) and finalArray[(xPos * 2) + ((yPos-1) * xSizeFinal * 2)] == 0:
                count += 1
                theStack.append(currentCell)
                # remove walls
                finalArray[(xPos * 2) + (yPos * xSizeFinal * 2) - xSizeFinal] = count
                # mark cells
                finalArray[(xPos * 2) + (yPos * xSizeFinal * 2)] = count
                currentCell = currentCell - xSizeCells
                theStack.append(currentCell)
                print("Picked Up")
                break
        if direction == 1:
            # Can we extend the maze downward?  If so, do it.
            if currentCell < ((xSizeCells * ySizeCells) - xSizeCells) and finalArray[(xPos * 2) + ((yPos + 1) * xSizeFinal * 2)] == 0:
                count += 1
                theStack.append(currentCell)
                # remove walls
                finalArray[(xPos * 2) + (yPos * xSizeFinal * 2) + xSizeFinal] = count
                # mark cells
                finalArray[(xPos * 2) + (yPos * xSizeFinal * 2)] = count
                currentCell = currentCell + xSizeCells
                theStack.append(currentCell)
                print("Picked Down")
                break
        if direction == 2:
            # Can we extend the maze leftward?  If so, do it.
            if xPos > 0 and finalArray[((xPos - 1) * 2) + (yPos * xSizeFinal * 2)] == 0:
                count += 1
                theStack.append(currentCell)
                # remove walls
                finalArray[(xPos * 2) + (yPos * xSizeFinal * 2) - 1] = count
                # mark cells
                finalArray[(xPos * 2) + (yPos * xSizeFinal * 2)] = count
                currentCell = currentCell - 1
                theStack.append(currentCell)
                print("Picked Left")
                break
        if direction == 3:
            # Can we extend the maze rightward?  If so, do it.
            if xPos < (xSizeCells - 1) and finalArray[((xPos + 1) * 2) + (yPos * xSizeFinal * 2)] == 0:
                count += 1
                theStack.append(currentCell)
                # remove walls
                finalArray[(xPos * 2) + (yPos * xSizeFinal * 2) + 1] = count
                # mark cells
                finalArray[(xPos * 2) + (yPos * xSizeFinal * 2)] = count
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
    draw = list(map(drawer, finalArray))
    print("Current Maze: ")
    for i in range(0, ySizeFinal):
        print(*draw[i*xSizeFinal: i*xSizeFinal + xSizeFinal])
    input() # Uncomment this line to step through one tick at a time.

# Step 4: Assemble and display the results:
print("*** FINAL ARRAY ***")
padFinalArray = [str(item).zfill(3) for item in finalArray]
for i in range(0, ySizeFinal):
    print(padFinalArray[i*xSizeFinal: i*xSizeFinal + xSizeFinal])
print()
print("*** Copyable Final Array ***")
for i in range(0, ySizeFinal):
    print(*finalArray[i*xSizeFinal: i*xSizeFinal + xSizeFinal])
