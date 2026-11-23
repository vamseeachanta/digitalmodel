import OrcFxAPI
import matplotlib.pyplot as plt

#Reference OrcaFlex simulation and OrcaWave results
model = OrcFxAPI.Model('L05 Panel pressures.sim')
diff = OrcFxAPI.Diffraction('L05 Panel pressures.owr')

#Assemble list of panel indices
panel_IDs = [0]

#Define period and pressure parameters    
period = OrcFxAPI.SpecifiedPeriod(0, 20)
parameters = OrcFxAPI.PanelPressureParameters(True, True, True) #Components of water pressure to be included (Hydrostatic, Diffraction, Radiation)

#Report time histories
PPTH = model['Keystone'].PanelPressureTimeHistory(diff, panel_IDs, period, parameters)
time = model.SampleTimes(period)

#Plot time history at panel
plt.plot(time, PPTH)
plt.title('Panel #{} pressure'.format(panel_IDs[0]+1))
plt.xlabel('Time (s)')
plt.ylabel('Pressure (kN/m^2)')
plt.show()