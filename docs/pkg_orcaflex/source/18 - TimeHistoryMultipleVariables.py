import OrcFxAPI

model = OrcFxAPI.Model()
vessel = model.CreateObject(OrcFxAPI.ObjectType.Vessel)
model.RunSimulation()

period = OrcFxAPI.SpecifiedPeriod(1.0, 2.0)
varNames = "X", "Y", "Z"
pos = vessel.TimeHistory(varNames, period)
print(pos)
