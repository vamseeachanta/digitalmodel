import OrcFxAPI

model = OrcFxAPI.Model()
model.general.StageDuration = 10.0, 200.0
model.environment.WaveType = "JONSWAP" # use irregular wave
vessel = model.CreateObject(OrcFxAPI.ObjectType.Vessel)
line = model.CreateObject(OrcFxAPI.ObjectType.Line)
line.EndAConnection = vessel.Name
line.EndAX, line.EndAY, line.EndAZ = 40.0, 0.0, 5.0
line.EndBX, line.EndBY, line.EndBZ = 70.0, 0.0, -25.0
model.RunSimulation()

varNames = "Effective tension", "Bend moment", "Curvature"
period = 1
objectExtra = OrcFxAPI.oeArcLength(25.0)
stats = line.LinkedStatistics(varNames, period, objectExtra)

# query tension and bend moment
query = stats.Query("Effective tension", "Bend moment")
print(f" (1) max tension = {query.ValueAtMax}")
print(f" (2) time of max tension = {query.TimeOfMax}")
print(f" (3) bend moment at this time = {query.LinkedValueAtMax}")
print(f" (4) min tension = {query.ValueAtMin}")
print(f" (5) time of min tension = {query.TimeOfMin}")
print(f" (6) bend moment at this time = {query.LinkedValueAtMin}")

# query tension and curvature
query = stats.Query("Effective tension", "Curvature")
print(f" (7) time of max tension = {query.TimeOfMax}")
print(f" (8) curvature at this time = {query.LinkedValueAtMax}")
print(f" (9) time of min tension = {query.TimeOfMin}")
print(f"(10) curvature at this time = {query.LinkedValueAtMin}")

# can also extract time series statistics
tss = stats.TimeSeriesStatistics("Effective tension")
print(f"(11) mean = {tss.Mean}")
print(f"(12) stddev = {tss.StdDev}")
print(f"(13) m0 = {tss.m0}")
print(f"(14) m2 = {tss.m2}")
print(f"(15) m4 = {tss.m4}")
print(f"(16) Tz = {tss.Tz}")
print(f"(17) Tc = {tss.Tc}")
print(f"(18) Bandwidth = {tss.Bandwidth}")
