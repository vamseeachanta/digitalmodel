import OrcFxAPI

model = OrcFxAPI.Model()
vessel = model.CreateObject(OrcFxAPI.ObjectType.Vessel)
line = model.CreateObject(OrcFxAPI.ObjectType.Line)
line.EndAConnection = vessel.Name
line.EndAX, line.EndAY, line.EndAZ = 40.0, 0.0, 5.0
line.EndBX, line.EndBY, line.EndBZ = 70.0, 0.0, -25.0
model.RunSimulation()

period = OrcFxAPI.PeriodNum.LatestWave
rg = line.RangeGraph("Effective tension", period)
print("{:>5} {:>7} {:>7} {:>7}".format("z", "min", "max", "mean"))
for z, minTe, maxTe, meanTe in zip(rg.X, rg.Min, rg.Max, rg.Mean):
    print(f"{z:5.1f} {minTe:7.3f} {maxTe:7.3f} {meanTe:7.3f}")
