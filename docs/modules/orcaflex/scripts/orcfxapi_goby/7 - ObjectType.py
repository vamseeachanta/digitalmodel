import OrcFxAPI

model = OrcFxAPI.Model()
# create two vessels ...
for i in range(2):
    model.CreateObject(OrcFxAPI.ObjectType.Vessel)
# ... and three lines
for i in range(3):
    model.CreateObject(OrcFxAPI.ObjectType.Line)

# for loop to perform action on all lines
for obj in model.objects:
    if obj.type == OrcFxAPI.ObjectType.Line:
        print(f"(1) {obj.name}")

# for loop to create list of vessels
vessels = []
for obj in model.objects:
    if obj.type == OrcFxAPI.ObjectType.Vessel:
        vessels.append(obj)
print(f"(2) {vessels}")

# list comprehension to do the same more concisely
vessels = [obj for obj in model.objects if obj.type ==
    OrcFxAPI.ObjectType.Vessel]
print(f"(3) {vessels}")

# The type attribute returns an integer value
print(f"(4) {vessels[0].type} {OrcFxAPI.ObjectType.Vessel}")

# do not confuse type attribute with built-in type() function
print(f"(5) {type(vessels[0])}")
