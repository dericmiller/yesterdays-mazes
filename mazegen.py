# mazegen.py implements the maze generation algorithm in python as an example
# and sandbox because 6502 assembly can be a beast sometimes.
# The NES screen is 32 8x8 tiles wide, and 30 tiles tall, but visibility of the
# top and bottom lines varies by screen and emulator, so we throw out the top
# two lines, and the bottom one line, leaving us with 27 lines to deal with.

import random
import math

xSizeFinal = 29
ySizeFinal = 25
xSizeCells = 15
ySizeCells = 13

def coordinates(number, x_size):
    x_value = number % x_size
    y_value = math.floor(number/x_size)
    return x_value, y_value

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
    padFinalArray = [str(item).zfill(3) for item in finalArray]
    draw = list(map(drawer, finalArray))
    print("Current Maze: ")
    for i in range(0, ySizeFinal):
        print(*draw[i*xSizeFinal: i*xSizeFinal + xSizeFinal])
        # print(draw)
    input()

print("***FINAL ARRAY ***")
padFinalArray = [str(item).zfill(3) for item in finalArray]
for i in range(0, ySizeFinal):
    print(padFinalArray[i*xSizeFinal: i*xSizeFinal + xSizeFinal])
print()
print("*** Copyable Final Array ***")
for i in range(0, ySizeFinal):
    print(*finalArray[i*xSizeFinal: i*xSizeFinal + xSizeFinal])
