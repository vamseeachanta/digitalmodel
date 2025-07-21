import OrcFxAPI

model = OrcFxAPI.Model()
print(model.environment.WaveHeight)
model.environment.WaveHeight = 2.5
print(model.environment.WaveHeight)
