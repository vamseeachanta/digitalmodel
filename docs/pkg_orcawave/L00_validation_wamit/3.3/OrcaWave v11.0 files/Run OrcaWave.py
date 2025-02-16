import numpy
import OrcFxAPI

OrcaWave = OrcFxAPI.Diffraction()
OrcaWave.LoadData('test.owd')

for omega in numpy.linspace(1, 4, 21):
    OrcaWave.Reset()
    OrcaWave.PeriodOrFrequency = (omega, omega + 2.5)
    OrcaWave.Calculate()
    OrcaWave.SaveResults('test {:0=5.3f}.owr'.format(omega))
    print('OrcaWave run complete for omega = {:.3f}'.format(omega))
