import OrcFxAPI

model = OrcFxAPI.Model()
vessel = model.CreateObject(OrcFxAPI.ObjectType.Vessel)
line = model.CreateObject(OrcFxAPI.ObjectType.Line)
for obj in model.objects:
    print(obj)
