import OrcFxAPI

model = OrcFxAPI.Model()
line = model.CreateObject(OrcFxAPI.ObjectType.Line)
model.CalculateStatics()
modes = OrcFxAPI.Modes(line)
print(f"{modes.modeCount} modes, {modes.dofCount} dofs")
print(modes.period[:5])
