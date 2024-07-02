import OrcFxAPI

model = OrcFxAPI.Model()
line = model.CreateObject(OrcFxAPI.ObjectType.Line)

model.CalculateStatics()
print(f"(1) Model state = {model.state}")

model.RunSimulation()
print(f"(2) Model state = {model.state}")

model.Reset()
print(f"(3) Model state = {model.state}")

model.RunSimulation()
print(f"(4) Model state = {model.state}")

line.Length = 120.0,
print(f"(5) Model state = {model.state}")
