import OrcFxAPI

model = OrcFxAPI.Model()
model.general.StageDuration = 10.0, 50.0
model.environment.WaveType = "JONSWAP" # use irregular wave
vessel = model.CreateObject(OrcFxAPI.ObjectType.Vessel)
model.RunSimulation()

period = 1
halfCycleRanges = vessel.RainflowHalfCycles("X", period)
for halfCycleRange in halfCycleRanges:
    print(halfCycleRange)
