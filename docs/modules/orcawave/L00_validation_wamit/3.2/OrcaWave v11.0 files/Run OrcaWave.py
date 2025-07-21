import numpy
import OrcFxAPI

OrcaWave = OrcFxAPI.Diffraction()
OrcaWave.LoadData('Sphere.owd')

omegas = numpy.concatenate((numpy.linspace(0.5, 4.5, 21), [2.8, 3.2, 3.4]))

for omega in omegas:
    OrcaWave.Reset()
    OrcaWave.PeriodOrFrequency = (omega, omega + 0.1)
    OrcaWave.Calculate()
    OrcaWave.SaveResults('Hemisphere {:0=5.3f}.owr'.format(omega))
    print('OrcaWave run complete for omega = {:.3f}'.format(omega))
