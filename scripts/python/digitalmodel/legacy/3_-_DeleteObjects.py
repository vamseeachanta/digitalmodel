import OrcFxAPI

model = OrcFxAPI.Model()
line = model.CreateObject(OrcFxAPI.ObjectType.Line)

# destroy object by passing reference to DestroyObject method ...
model.DestroyObject(line)

# ... or by passing the object name if we don't have a reference
# available, for example the automatically created line type
model.DestroyObject("Line Type1")
