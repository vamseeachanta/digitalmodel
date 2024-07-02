import OrcFxAPI

model = OrcFxAPI.Model()
line = model.CreateObject(OrcFxAPI.ObjectType.Line)
model.CalculateStatics()
spec = OrcFxAPI.ModalAnalysisSpecification(calculateShapes=False, lastMode=5)
modes = OrcFxAPI.Modes(line, spec)
print(f"{modes.modeCount} modes, {modes.dofCount} dofs")
print(modes.period)
