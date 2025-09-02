import OrcFxAPI

model = OrcFxAPI.Model()
model.general.DynamicsSolutionMethod = "Frequency domain"
model.environment.WaveType = "JONSWAP" # use irregular wave
vessel = model.CreateObject(OrcFxAPI.ObjectType.Vessel)
model.RunSimulation()

fdr = vessel.FrequencyDomainResults("Z")
print(f" (1) Static value  = {fdr.StaticValue}")
print(f" (2) Std. dev.  = {fdr.StdDev}")
print(f" (3) Tz  = {fdr.Tz}")
print(f" (4) Tc  = {fdr.Tc}")
print(f" (5) m0  = {fdr.m0}")
print(f" (6) m1  = {fdr.m1}")
print(f" (7) m2  = {fdr.m2}")
print(f" (8) m3  = {fdr.m3}")
print(f" (9) m4  = {fdr.m4}")
print(f"(10) Bandwidth  = {fdr.Bandwidth}")
