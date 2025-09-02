import OrcFxAPI

OrcaWave = OrcFxAPI.Diffraction()
OrcaWave.LoadData('Moonpool Body.owd')

for DampingFactor in [0.0, 0.016, 0.05]:
    OrcaWave.DampingFactorEpsilon = DampingFactor
    OrcaWave.Calculate()
    OrcaWave.SaveResults('Moonpool Body damping {:.3f}.owr'.format(DampingFactor))
    OrcaWave.Reset()
    print('OrcaWave run complete for DampingFactor={:.3f}'.format(DampingFactor))

OrcaWave.HasResonanceDampingLid = 'No'
OrcaWave.Calculate()
OrcaWave.SaveResults('Moonpool Body no damping lid.owr')
