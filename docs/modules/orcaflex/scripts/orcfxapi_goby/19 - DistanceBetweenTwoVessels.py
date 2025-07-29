import numpy
import OrcFxAPI

model = OrcFxAPI.Model()
model.environment.WaveType = "JONSWAP" # use irregular wave
model.environment.WaveDirection = 130.0
vessel1 = model.CreateObject(OrcFxAPI.ObjectType.Vessel)
vessel1.InitialX, vessel1.InitialY = 0.0, 0.0
vessel2 = model.CreateObject(OrcFxAPI.ObjectType.Vessel)
vessel2.InitialX, vessel2.InitialY = 80.0, -25.0
model.RunSimulation()

period = OrcFxAPI.SpecifiedPeriod(1.0, 2.0)
varNames = "X", "Y", "Z"
pos1 = vessel1.TimeHistory(varNames, period)
pos2 = vessel2.TimeHistory(varNames, period)
distance = numpy.linalg.norm(pos1 - pos2, axis=1)
times = model.SampleTimes(period)
for t, d in zip(times, distance):
    print(f"t={t:4.1f}s, d={d:7.3f}m")
