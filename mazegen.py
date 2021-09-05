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
xSizeVertWalls = 15
ySizeVertWalls = 12
xSizeHorizWalls = 14
ySizeHorizWalls = 13


def coordinates(number, x_size):
    x_value = number % x_size
    y_value = math.floor(number/x_size)
    return x_value, y_value


# Step 1: Make & initialize working arrays:
theStack = []
cellsArray = [0 for i in range(xSizeCells * ySizeCells)]
vertWallsArray = [1 for j in range(xSizeVertWalls * ySizeVertWalls)]
horizWallsArray = [1 for k in range(xSizeHorizWalls * ySizeHorizWalls)]
finalArray = [1 for i in range(xSizeFinal * ySizeFinal)]

# Step 2: pick a random cell, mark it part of the maze, and add it to the stack:
currentCell = random.randint(0, 191)
xPos, yPos = coordinates(currentCell, xSizeCells)
count = 1
cellsArray[currentCell] = count
finalArray[(xPos * 2) + (yPos * xSizeFinal * 2)] = count
theStack.append(currentCell)

while len(theStack) > 0:
    currentCell = theStack.pop()
    xPos, yPos = coordinates(currentCell, xSizeCells)
    direction = random.randint(0, 3)
    print("Current cell: ", currentCell)
    print("Direction: ", direction)
    print("xPos: ", xPos)
    print("yPos: ", yPos)
    for i in range(0, 3):
        if direction == 0:
            # Can we extend the maze upward?  If so, do it.
            if currentCell > (xSizeCells - 1) and cellsArray[currentCell - xSizeCells] == 0:
                count += 1
                theStack.append(currentCell)
                # walls
                vertWallsArray[((yPos-1) * xSizeVertWalls) + xPos] = 0
                finalArray[(xPos * 2) + (yPos * xSizeFinal * 2) - xSizeFinal] = 0
                # cells
                cellsArray[currentCell - xSizeCells] = count
                finalArray[(xPos * 2) + (yPos * xSizeFinal * 2)] = 0
                currentCell = currentCell - xSizeCells
                theStack.append(currentCell)
                print("Picked Up")
                break
        if direction == 1:
            # Can we extend the maze downward?  If so, do it.
            if currentCell < ((xSizeCells * ySizeCells) - xSizeCells) and cellsArray[currentCell + xSizeCells] == 0:
                count += 1
                theStack.append(currentCell)
                # walls
                vertWallsArray[((yPos) * xSizeVertWalls) + xPos] = 0
                finalArray[(xPos * 2) + (yPos * xSizeFinal * 2) + xSizeFinal] = 0
                # cells
                cellsArray[currentCell + xSizeCells] = count
                finalArray[(xPos * 2) + (yPos * xSizeFinal * 2)] = 0
                currentCell = currentCell + xSizeCells
                theStack.append(currentCell)
                print("Picked Down")
                break
        if direction == 2:
            # Can we extend the maze leftward?  If so, do it.
            if xPos > 0 and cellsArray[currentCell - 1] == 0:
                count += 1
                theStack.append(currentCell)
                #walls
                horizWallsArray[(yPos * xSizeHorizWalls) + xPos - 1] = 0
                finalArray[(xPos * 2) + (yPos * xSizeFinal * 2) - 1] = 0
                #cells
                cellsArray[currentCell - 1] = count
                finalArray[(xPos * 2) + (yPos * xSizeFinal * 2)] = 0
                currentCell = currentCell - 1
                theStack.append(currentCell)
                print("Picked Left")
                break
        if direction == 3:
            # Can we extend the maze rightward?  If so, do it.
            if xPos < (xSizeCells - 1) and cellsArray[currentCell + 1] == 0:
                count += 1
                theStack.append(currentCell)
                # walls
                horizWallsArray[(yPos * xSizeHorizWalls) + xPos] = 0
                finalArray[(xPos * 2) + (yPos * xSizeFinal * 2) + 1] = 0
                # cells
                cellsArray[currentCell + 1] = count
                finalArray[(xPos * 2) + (yPos * xSizeFinal * 2)] = 0
                currentCell = currentCell + 1
                theStack.append(currentCell)
                print("Picked Right")
                break
        direction += 1
        if direction > 3:
            direction = 0
        print("New Direction: ", direction)
    print("The Stack:")
    print(theStack)
    padArray = [str(item).zfill(3) for item in cellsArray]
    print("Cells array:")
    for i in range(0, ySizeCells):
        print(padArray[i*xSizeCells: i*xSizeCells + xSizeCells])

#for idx, val in enumerate(vertWallsArray):
#    xPos, yPos = coordinates(idx, xSizeVertWalls)
#    finalArray[((yPos + 1) * xSizeFinal)+(xPos * 2)] = val
#for idx, val in enumerate(horizWallsArray):
#    xPos, yPos = coordinates(idx, xSizeHorizWalls)
#    finalArray[(yPos * xSizeFinal) + ((xPos * 2) + 1)] = val

print()
print("***VERT WALLS ARRAY ***")
for i in range(0, ySizeVertWalls):
    print(vertWallsArray[i*xSizeVertWalls: i*xSizeVertWalls + xSizeVertWalls])
print()
print("***HORIZ WALLS ARRAY ***")
for i in range(0, ySizeHorizWalls):
    print(horizWallsArray[i*xSizeHorizWalls: i*xSizeHorizWalls + xSizeHorizWalls])
print()
print("***FINAL ARRAY ***")
padFinalArray = [str(item).zfill(3) for item in finalArray]
for i in range(0, xSizeCells * ySizeCells):
    xPos, yPos = coordinates(i, xSizeCells)
    finalArray[(xPos * 2) + (yPos * xSizeFinal * 2)] = 0
for i in range(0, ySizeFinal):
    print(padFinalArray[i*xSizeFinal: i*xSizeFinal + xSizeFinal])
print()
for i in range(0, ySizeFinal):
    print(finalArray[i*xSizeFinal: i*xSizeFinal + xSizeFinal])
