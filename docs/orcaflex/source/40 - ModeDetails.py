import OrcFxAPI

model = OrcFxAPI.Model()
line = model.CreateObject(OrcFxAPI.ObjectType.Line)
model.CalculateStatics()
spec = OrcFxAPI.ModalAnalysisSpecification(calculateShapes=True, firstMode = 4,
    lastMode=5)
modes = OrcFxAPI.Modes(line, spec)
for modeIndex in range(modes.modeCount):
    details = modes.modeDetails(modeIndex)
    print(details.modeNumber)
    print(details.period)
    print(details.shapeWrtGlobal)
