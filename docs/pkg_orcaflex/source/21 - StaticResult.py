import OrcFxAPI

model = OrcFxAPI.Model()
line = model.CreateObject(OrcFxAPI.ObjectType.Line)
model.CalculateStatics()

# static result available when model in static state ...
print(f"(1) {line.StaticResult('Effective tension', OrcFxAPI.oeEndA)}")

# ... and after dynamic simulation
model.RunSimulation()
print(f"(2) {line.StaticResult('Effective tension', OrcFxAPI.oeEndA)}")
