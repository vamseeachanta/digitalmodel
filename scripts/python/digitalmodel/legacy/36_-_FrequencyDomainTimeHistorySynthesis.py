import matplotlib.pyplot as pyplot
import OrcFxAPI

model = OrcFxAPI.Model()
model.general.DynamicsSolutionMethod = "Frequency domain"
model.environment.WaveType = "JONSWAP" # use irregular wave
vessel = model.CreateObject(OrcFxAPI.ObjectType.Vessel)
model.RunSimulation()

t = vessel.SampleTimes(OrcFxAPI.SpecifiedPeriod(0.0, 3600.0))
Z = vessel.TimeHistory("Z", OrcFxAPI.SpecifiedPeriod(0.0, 3600.0))
pyplot.plot(t, Z)
pyplot.xlabel("t")
pyplot.ylabel("Z")
pyplot.show()
