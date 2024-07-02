import OrcFxAPI

model = OrcFxAPI.Model()
line = model.CreateObject(OrcFxAPI.ObjectType.Line)

# set the line's name and some section data
line.Name = "Riser"
line.Length = 87.0, 45.0
line.TargetSegmentLength = 5.0, 2.0

# hard-coded name
print(f"Riser length = {model['Riser'].Length}")
print(f"Riser segment length = {model['Riser'].TargetSegmentLength}")

# line type name read from section data
lineTypeName = line.LineType[0]
lineType = model[lineTypeName]
print(f"{lineTypeName} EA = {lineType.EA}")
