import OrcFxAPI

model = OrcFxAPI.Model()
model.general.DynamicsSolutionMethod = "Frequency domain"
model.environment.WaveType = "JONSWAP" # use irregular wave
vessel = model.CreateObject(OrcFxAPI.ObjectType.Vessel)
line = model.CreateObject(OrcFxAPI.ObjectType.Line)
line.EndAConnection = vessel.Name
line.EndAX = 50.0
line.EndBX = 90.0
model.RunSimulation()

rg = line.RangeGraph("Effective tension",
    arclengthRange=OrcFxAPI.arSpecifiedArclengths(15.0, 45.0),
    stormDurationHours=3.0)
print("{:>5}   {:>9}   {:>9}   {:>9}".format("z", "Static", "Std. dev.", "MPM"))
for z, Static, StdDev, MPM in zip(rg.X, rg.StaticValue, rg.StdDev, rg.MPM):
    print(f"{z:5.1f}   {Static:9.3f}   {StdDev:9.3f}   {MPM:9.3f}")
