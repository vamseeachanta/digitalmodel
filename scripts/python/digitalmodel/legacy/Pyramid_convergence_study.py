import OrcFxAPI

OrcaWave = OrcFxAPI.Diffraction()
OrcaWave.LoadData('Pyramid.owd')

for zCount in range(6, 30, 2):
    basefilename = 'PyramidZC{:02d}'.format(zCount)
    OrcaWave.Reset()
    OrcaWave.BodyMeshFileName = basefilename + '.gdf'
    OrcaWave.BodyControlSurfaceMeshFileName = basefilename + '.csf'
    OrcaWave.PeriodOrFrequency = (5, )
    OrcaWave.Calculate()
    OrcaWave.SaveResults(basefilename + '.owr')
    print('OrcaWave run complete for zCount={:d}'.format(zCount))
