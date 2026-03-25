import OrcFxAPI

model = OrcFxAPI.Model()
environment = model.environment
vessel = model.CreateObject(OrcFxAPI.ObjectType.Vessel)
line = model.CreateObject(OrcFxAPI.ObjectType.Line)
line.EndAConnection = vessel.Name
line.EndAX, line.EndAY, line.EndAZ = 40.0, 0.0, 5.0
line.EndBX, line.EndBY, line.EndBZ = 70.0, 0.0, -25.0
model.RunSimulation()

period = OrcFxAPI.PeriodNum.LatestWave

# sea elevation at position 5, 0, -10
tmp = environment.TimeHistory("Elevation", period,
    OrcFxAPI.oeEnvironment(5.0, 0.0, -10.0))

# vessel velocity at a specified point in local vessel axes
tmp = vessel.TimeHistory("Velocity", period,
    OrcFxAPI.oeVessel(-10.0, 0.0, 3.0))

# line results at End A and End B ...
tmp = line.TimeHistory("Effective tension", period, OrcFxAPI.oeEndA)
tmp = line.TimeHistory("Effective tension", period, OrcFxAPI.oeEndB)

# ... at specified arc length or node number
tmp = line.TimeHistory("Effective tension", period,
    OrcFxAPI.oeArcLength(15.0))
tmp = line.TimeHistory("X", period, OrcFxAPI.oeNodeNum(3))

# if you wish to omit period and default to whole simulation, you
# can use positional parameters, pass None for the period ...
tmp = line.TimeHistory("Effective tension", None, OrcFxAPI.oeEndA)
# ... or you can use named parameters
tmp = line.TimeHistory("Effective tension",
    objectExtra=OrcFxAPI.oeEndA)
