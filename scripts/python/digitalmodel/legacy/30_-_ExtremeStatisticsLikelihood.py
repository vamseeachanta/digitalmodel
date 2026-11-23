import OrcFxAPI

distribution = OrcFxAPI.ExtremeValueDistribution.GPD
# alternatively OrcFxAPI.ExtremeValueDistribution.Weibull
threshold = 2.0 # vessel Z measured in m
declusterPeriod = OrcFxAPI.OrcinaDefaultReal()
stormDuration = 3 # in hours
confidenceLevel = 95 # percentage

model = OrcFxAPI.Model()
model.general.StageDuration = 10.0, 3600.0
model.environment.WaveType = "JONSWAP" # use irregular wave
vessel = model.CreateObject(OrcFxAPI.ObjectType.Vessel)
model.RunSimulation()

# obtain the results object
period = 1
stats = vessel.ExtremeStatistics("Z", period)

# fit the distribution, for upper tail, e.g. max values
spec = OrcFxAPI.LikelihoodStatisticsSpecification(distribution, threshold,
    declusterPeriod, OrcFxAPI.DistributionTail.Upper)
stats.Fit(spec)

# query results
query = OrcFxAPI.LikelihoodStatisticsQuery(stormDuration, confidenceLevel)
extremes = stats.Query(query)
print(f"(1) {stormDuration} hour return level = {extremes.ReturnLevel}")
print(f"(2) {confidenceLevel}% confidence limits = " \
    f"[{extremes.ConfidenceInterval.Lower}, {extremes.ConfidenceInterval.Upper}]")
print(f"(3) sigma = {extremes.Sigma} (se = {extremes.SigmaStdError})")
print(f"(4) xi = {extremes.Xi} (se = {extremes.XiStdError})")
