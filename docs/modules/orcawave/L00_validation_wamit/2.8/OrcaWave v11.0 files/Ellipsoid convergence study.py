import OrcFxAPI

OrcaWave = OrcFxAPI.Diffraction()
OrcaWave.LoadData('Ellipsoid.owd')

for PanelCount in [96, 216, 384, 600, 864, 1350, 1944, 2904, 4056]:
    basefilename = 'Ellipsoid{:04d}'.format(PanelCount)
    OrcaWave.Reset()
    OrcaWave.BodyMeshFileName = basefilename + '.gdf'
    OrcaWave.BodyControlSurfaceMeshFileName = basefilename + '.csf'
    OrcaWave.PeriodOrFrequency = (2, )
    OrcaWave.Calculate()
    OrcaWave.SaveResults(basefilename + '.owr')
    print('OrcaWave run complete for N={:d}'.format(PanelCount))
