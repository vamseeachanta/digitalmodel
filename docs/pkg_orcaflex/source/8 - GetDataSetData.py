import OrcFxAPI

model = OrcFxAPI.Model()
print(model.environment.GetData("WaveHeight", -1))
model.environment.SetData("WaveHeight", -1, 2.5)
print(model.environment.GetData("WaveHeight", -1))
