"""
K06 FPV array python script. 

This python script builds a rectangular array from a source model with a single modular FPV unit. A further explanation of the script can be found in 'K06 FPV array.pdf' in the same folder as this file. For further information on the functions used, see the OrcFxAPI documentation.

"""

import OrcFxAPI

OrcFxAPI.SetLibraryPolicy("EnableBooleanDataType") # This allows us to use boolean values True and False instead of the default "Yes" and "No"

sourceFileName = "K06 FPV module.dat"  # The file we take our single unit raft design from

# Array parameters - these variables can be edited by the user to adjust the size of the array.
rowCount = 8  # number of rows in the array
colCount = 8  # number of columns in the array
rowGap = 0.4  # gap between columns
colGap = 0.6  # gap between rows

# Load the source model
sourceModel = OrcFxAPI.Model(sourceFileName)

tags = sourceModel.general.tags

# Define the reference object to be used in MoveObjects - these are defined in model tags in general
refObj = sourceModel[tags.refObj]

# Get edges of rafts to connect constraints to - also defined in model's general data tags
top = sourceModel[tags.top]
bottom = sourceModel[tags.bottom]
left = sourceModel[tags.left]
right = sourceModel[tags.right]

# Define height and module info for setting raft position & position of constraints
refObj.EndAConnection = "Free" # refObj reference point is End A
heightZ = refObj.EndAZ
moduleSizeX = top.CumulativeLength[-1]
moduleSizeY = left.CumulativeLength[-1]

# Define 'reference position' of our reference object so we can centre the array on global origin.
arrayExtentX = colCount * moduleSizeX + (colCount - 1) * rowGap
arrayExtentY = rowCount * moduleSizeY + (rowCount - 1) * colGap
refObjPositionX = -0.5 * arrayExtentX - (moduleSizeX + rowGap)
refObjPositionY = 0.5 * arrayExtentY + (moduleSizeY + colGap)


# Below functions are used for the array building
def CloneAndMoveGroup(row, col):
    """Take a model, find a group, rename group objects, move them, and clone group into another model.
    row, col -- row and column numbers, used for the naming of the objects and determining position in array
    """
    print(f"creating module {row}-{col}")
    sourceModel = OrcFxAPI.Model(sourceFileName)  # load in fresh version of source model 

    # Find the objects to clone - these are in one main browser group with all objects and subgroups to be cloned
    sourceGroup = sourceModel.groupFirstChild
    sourceObjects = [obj for obj in sourceGroup.GroupChildren(True)]
    sourceObjects.insert(0, sourceGroup)

    # Rename objects in the source model
    for obj in sourceObjects:
        obj.name += f"{row}-{col}"
        if obj.type == OrcFxAPI.ObjectType.Shape and obj.ShapeType == "Label":
            obj.LabelText[0] += f"{row}-{col}"

    # Define the new position of End A of the reference object
    newX = refObjPositionX + (moduleSizeX + rowGap) * col
    newY = refObjPositionY - (moduleSizeY + colGap) * row
    newZ = heightZ
    newPosition = (newX, newY, newZ)

    # Move them within the source model
    
    currentRefObj = sourceModel[f"{refObj.name}{row}-{col}"]
    # Input the move position - takes arguments (referenceObj, referencePointIndex, newPosition)
    specification = OrcFxAPI.MoveObjectNewPositionSpecification(
        currentRefObj, 1, newPosition
    )  # refPointIndex = 1 here indicates that we move with respect to End A of the refObj line.

    # Define the four float buoys that we want to move 
    moveObjectsPrefix = sourceModel.general.tags.move # "Float" naming prefix defined in the model tags
    moveObjects = [
        obj for obj in sourceObjects if obj.name.startswith(moveObjectsPrefix)
    ]
    # Define move points for the four float buoys
    points = [OrcFxAPI.MoveObjectPoint(obj, 1) for obj in moveObjects]
    # Define move points for our refObj and add this to the points sequence. As this is a line, we use both pointIndex = 1, 2 to move both end A and end B.
    points.extend([OrcFxAPI.MoveObjectPoint(currentRefObj, i) for i in (1, 2)])
    # Perform the move operation
    OrcFxAPI.MoveObjects(specification, points)

    # Clone these objects from source model into new model
    sourceModel.CreateClones([sourceGroup], model=newModel)


def CreateRowConstraint(row, col):
    """Take row and col of module to the right of rowGap and create a constraint between columns of the array."""

    print(f"creating row constraint {row}-{col-1}/{col}")
    colPrev = col - 1
    con = newModel.CreateObject(
        OrcFxAPI.ObjectType.Constraint, f"{row}-{colPrev}/{col}"
    )
    con.SolutionMethod = "Indirect"
    con.DoubleSidedConnection = True

    # These co-ordinates are wrt local line axes
    con.InFrameConnection = newModel[f"{right.name}{row}-{colPrev}"].name
    con.InFrameInitialX = 0
    con.InFrameInitialY = 0.5 * rowGap
    con.InFrameInitialZ = (
        0.5 * newModel[f"{right.name}{row}-{colPrev}"].CumulativeLength[-1]
    )
    con.OutFrameConnection = newModel[f"{left.name}{row}-{col}"].name
    con.OutFrameInitialX = 0
    con.OutFrameInitialY = -0.5 * rowGap
    con.OutFrameInitialZ = (
        0.5 * newModel[f"{left.name}{row}-{col}"].CumulativeLength[-1]
    )
    # We add this constraint into the group for the module to the right
    con.groupParent = newModel[f"{sourceModel.groupFirstChild.name}{row}-{col}"]
    con.DOFFree[4] = True  # Ry - python has 0 based index and OrcaFlex has 1 based index.


def CreateColConstraint(row, col):
    """Take row and col of module below colGap and create a constraint between rows of the array."""

    print(f"creating col constraint {row-1}/{row}-{col}")
    rowAbove = row - 1
    con = newModel.CreateObject(
        OrcFxAPI.ObjectType.Constraint, f"{rowAbove}/{row}-{col}"
    )
    con.SolutionMethod = "Indirect"
    con.DoubleSidedConnection = True

    # These co-ordinates are wrt local line axes
    con.InFrameConnection = newModel[f"{bottom.name}{rowAbove}-{col}"].name
    con.InFrameInitialX = 0
    con.InFrameInitialY = -0.5 * colGap
    con.InFrameInitialZ = (
        0.5 * newModel[f"{bottom.name}{rowAbove}-{col}"].CumulativeLength[-1]
    )
    con.OutFrameConnection = newModel[f"{top.name}{row}-{col}"].name
    con.OutFrameInitialX = 0
    con.OutFrameInitialY = 0.5 * colGap
    con.OutFrameInitialZ = 0.5 * newModel[f"{top.name}{row}-{col}"].CumulativeLength[-1]

    con.groupParent = newModel[f"{sourceModel.groupFirstChild.name}{row}-{col}"]
    con.DOFFree[3] = True  # Rx


# Now we build the array
newModel = OrcFxAPI.Model()  # Create a new model

# Iterate across the rows and columns
for row in range(1, rowCount + 1):
    for col in range(1, colCount + 1):
        CloneAndMoveGroup(row, col)
        if col > 1:
            CreateRowConstraint(row, col)
        if row > 1:
            CreateColConstraint(row, col)

# Add these groups to a parent group for the array system
arrayGroup = newModel.CreateObject(OrcFxAPI.ObjectType.BrowserGroup, "Array System")
raftGroups = [
    obj for obj in newModel.objects if obj.type == OrcFxAPI.ObjectType.BrowserGroup and obj.name.startswith("Raft")
]
for group in raftGroups:
    group.groupParent = arrayGroup

# We can hide all constraint objects to make the array easier to see in 3D view
for obj in newModel.objects:
    if obj.type == OrcFxAPI.ObjectType.Constraint:
        obj.hidden = True

# Save the model
newModel.SaveData(f"K06 FPV array {rowCount}x{colCount}.dat")
