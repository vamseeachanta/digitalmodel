import OrcFxAPI

model = OrcFxAPI.Model()
lineType1 = model.CreateObject(OrcFxAPI.ObjectType.LineType)
lineType2 = model.CreateObject(OrcFxAPI.ObjectType.LineType)
print(lineType1.Name)
print(lineType2.Name)
