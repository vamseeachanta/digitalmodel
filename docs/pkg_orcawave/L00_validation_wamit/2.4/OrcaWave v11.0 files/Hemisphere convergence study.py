import os
import OrcFxAPI

WorkingDirectory = os.path.split(__file__)[0]

OrcaWave = OrcFxAPI.Diffraction()
OrcaWave.LoadData('Hemisphere.owd')

for gdf in [f for f in os.listdir(WorkingDirectory) if f.endswith('.gdf')]:
    OrcaWave.Reset()
    OrcaWave.BodyMeshFileName = gdf
    OrcaWave.PeriodOrFrequency = (6.2631142413339385, )
    OrcaWave.Calculate()
    basefilename = gdf[:-4]
    OrcaWave.SaveResults(basefilename + '.owr')
    print('OrcaWave run complete for {:s}'.format(basefilename))
