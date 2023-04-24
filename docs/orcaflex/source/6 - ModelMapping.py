import OrcFxAPI

model = OrcFxAPI.Model()
line = model.CreateObject(OrcFxAPI.ObjectType.Line, "MyLine")
print("MyLine" in model)
print("foo" in model)
print(model.get("MyLine"))
print(model.get("foo"))
for obj in model:
    print(obj)
