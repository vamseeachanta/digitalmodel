import OrcFxAPI

model = OrcFxAPI.Model()
line = model.CreateObject(OrcFxAPI.ObjectType.Line)
model.CalculateStatics()
spec = OrcFxAPI.ModalAnalysisSpecification(calculateShapes=True, firstMode = 1,
    lastMode=1)
modes = OrcFxAPI.Modes(line, spec)
details = modes.modeDetails(0)
for dofIndex in range(modes.dofCount):
    print("{modes.owner[dofIndex].name} node {modes.nodeNumber[dofIndex]:2} " \
    f"dof {modes.dof[dofIndex]}: {details.shapeWrtGlobal[dofIndex]}")
