import random
import numpy
import OrcFxAPI

period = 1
distribution = OrcFxAPI.ExtremeValueDistribution.GPD
threshold = 2.0 # vessel Z measured in m
declusterPeriod = OrcFxAPI.OrcinaDefaultReal()
stormDuration = 3 # in hours
confidenceLevel = 95 # percentage

rng = random.Random()
rng.seed(42)

model = OrcFxAPI.Model()
model.general.StageDuration = 10.0, 3600.0
model.environment.WaveType = "JONSWAP" # use irregular wave
model.environment.UserSpecifiedRandomWaveSeeds = "Yes"
vessel = model.CreateObject(OrcFxAPI.ObjectType.Vessel)

Z = numpy.array([])
for index in range(5):
    model.environment.WaveSeed = rng.randint(-2**31, 2**31 - 1)
    model.RunSimulation()
    Z = numpy.concatenate([Z, vessel.TimeHistory("Z", period)])

# obtain the results object
stats = OrcFxAPI.ExtremeStatistics(Z, model.general.ActualLogSampleInterval)

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
print("f(3) sigma = {extremes.Sigma} (se = {extremes.SigmaStdError})")
print("f(4) xi = {extremes.Xi} (se = {extremes.XiStdError})")
