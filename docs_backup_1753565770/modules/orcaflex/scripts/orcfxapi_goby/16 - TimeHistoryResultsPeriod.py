import OrcFxAPI

model = OrcFxAPI.Model()
vessel = model.CreateObject(OrcFxAPI.ObjectType.Vessel)
model.RunSimulation()

# no period specified, defaults to whole simulation
X = vessel.TimeHistory("X")
print(f"(1) no period specified {min(X)} {max(X)}")

# specify a period, whole simulation, equivalent to previous call
X = vessel.TimeHistory("X", OrcFxAPI.PeriodNum.WholeSimulation)
print(f"(2) whole simulation {min(X)} {max(X)}")

# build-up, stage 0
X = vessel.TimeHistory("X", 0)
print(f"(3) build-up {min(X)} {max(X)}")

# stage 1
X = vessel.TimeHistory("X", 1)
print(f"(4) stage 1 {min(X)} {max(X)}")

# latest wave
X = vessel.TimeHistory("X", OrcFxAPI.PeriodNum.LatestWave)
print(f"(5) latest wave {min(X)} {max(X)}")

# period specified by time
X = vessel.TimeHistory("X", OrcFxAPI.SpecifiedPeriod(1.0, 2.0))
print(f"(6) 1.0s to 2.0s {min(X)} {max(X)}")
print(f"(7) 1.0s to 2.0s {X}")
