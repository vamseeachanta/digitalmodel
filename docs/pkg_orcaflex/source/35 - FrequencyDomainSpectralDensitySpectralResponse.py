import matplotlib.pyplot as pyplot
import OrcFxAPI

model = OrcFxAPI.Model()
model.general.DynamicsSolutionMethod = "Frequency domain"
model.environment.WaveType = "JONSWAP" # use irregular wave
vessel = model.CreateObject(OrcFxAPI.ObjectType.Vessel)
model.RunSimulation()

spectralDensity = vessel.SpectralDensity("Z")
spectralResponseRAO = vessel.SpectralResponseRAO("Z")

pyplot.subplot(1, 2, 1)
pyplot.plot(spectralDensity.X, spectralDensity.Y)
pyplot.title("Spectral density")
pyplot.xlabel("f")
pyplot.ylabel("S")
pyplot.xlim(0.0, 0.25)

pyplot.subplot(1, 2, 2)
pyplot.plot(spectralResponseRAO.X, spectralResponseRAO.Y)
pyplot.title("Spectral response RAO")
pyplot.xlabel("f")
pyplot.ylabel("RAO")
pyplot.xlim(0.0, 0.25)

pyplot.tight_layout() # avoid label overlapping
pyplot.show()
