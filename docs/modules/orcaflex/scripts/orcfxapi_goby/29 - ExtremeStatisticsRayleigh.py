import OrcFxAPI

stormDuration = 1 # in hours
riskFactor = 3 # percentage

model = OrcFxAPI.Model()
model.general.StageDuration = 10.0, 3600.0
model.environment.WaveType = "JONSWAP" # use irregular wave
vessel = model.CreateObject(OrcFxAPI.ObjectType.Vessel)
model.RunSimulation()

# obtain the results object
period = 1
stats = vessel.ExtremeStatistics("Z", period)

# fit the distribution, for upper tail, e.g. max values
spec = OrcFxAPI.RayleighStatisticsSpecification(OrcFxAPI.DistributionTail.Upper)
stats.Fit(spec)

# query results
query = OrcFxAPI.RayleighStatisticsQuery(stormDuration, riskFactor)
extremes = stats.Query(query)
print(f"(1) {stormDuration} hour MPM = {extremes.MostProbableExtremeValue}")
print(f"(2) {stormDuration} hour max, risk factor {riskFactor}% = " \
    f"{extremes.ExtremeValueWithRiskFactor}")

# fit the distribution, for lower tail, e.g. min values
spec = OrcFxAPI.RayleighStatisticsSpecification(OrcFxAPI.DistributionTail.Lower)
stats.Fit(spec)

# query results
query = OrcFxAPI.RayleighStatisticsQuery(stormDuration,riskFactor)
extremes = stats.Query(query)
print(f"(3) {stormDuration} hour MPM = {extremes.MostProbableExtremeValue}")
print(f"(4) {stormDuration} hour min, risk factor {riskFactor}% = " \
    f"{extremes.ExtremeValueWithRiskFactor}")
