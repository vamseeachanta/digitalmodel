import numpy
import OrcFxAPI

OrcaWave = OrcFxAPI.Diffraction()
OrcaWave.LoadData('Bottom mounted cylinder.owd')

for omega in numpy.linspace(0.5, 3.0, 26):
    OrcaWave.Reset()
    OrcaWave.PeriodOrFrequency = (omega, )
    OrcaWave.Calculate()
    OrcaWave.SaveResults('BMC {:0=5.3f}.owr'.format(omega))
    print('OrcaWave run complete for omega = {:.3f}'.format(omega))
