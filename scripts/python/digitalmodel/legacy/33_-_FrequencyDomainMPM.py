import OrcFxAPI

model = OrcFxAPI.Model()
model.general.DynamicsSolutionMethod = "Frequency domain"
model.environment.WaveType = "JONSWAP" # use irregular wave
vessel = model.CreateObject(OrcFxAPI.ObjectType.Vessel)
model.RunSimulation()

MPM = vessel.FrequencyDomainMPM("Z", 3)
print(f"(1) MPM = {MPM}")
