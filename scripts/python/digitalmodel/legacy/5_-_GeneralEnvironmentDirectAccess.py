import OrcFxAPI

model = OrcFxAPI.Model()

# we can access the general data and environment data by name
print(model["General"].ImplicitConstantTimeStep)
print(model["Environment"].WaveHeight)

# but it is more convenient to do it this way
print(model.general.ImplicitConstantTimeStep)
print(model.environment.WaveHeight)
