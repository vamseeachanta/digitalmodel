import OrcFxAPI

model = OrcFxAPI.Model()
vessel = model.CreateObject(OrcFxAPI.ObjectType.Vessel)
model.RunSimulation()

period = OrcFxAPI.SpecifiedPeriod(1.0, 2.0)
times = model.SampleTimes(period)
X = vessel.TimeHistory("X", period)
for t, x in zip(times, X):
    print(f"t={t:4.1f}s, X={x:7.4f}m")
