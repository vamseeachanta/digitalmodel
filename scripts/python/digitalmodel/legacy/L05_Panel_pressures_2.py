import OrcFxAPI
import matplotlib.pyplot as plt

panel_IDs = []
x = []
y = []
z = []

#Reference OrcaFlex simulation and OrcaWave results
model = OrcFxAPI.Model('L05 Panel pressures.sim')
diff = OrcFxAPI.Diffraction('L05 Panel pressures.owr')

#Assemble list of panel indices
panelGeo = diff.panelGeometry

for i in range(len(panelGeo)):
    if panelGeo['objectName'][i] == 'Keystone':
        panel_IDs.append(i)
        x.append(panelGeo['centroid'][i][0])
        y.append(panelGeo['centroid'][i][1])
        z.append(panelGeo['centroid'][i][2])


#Define period and pressure parameters 
period = OrcFxAPI.SpecifiedPeriod(5, 5) #FromTime, ToTime
parameters = OrcFxAPI.PanelPressureParameters(True, True, True) #Components of water pressure to be included (Hydrostatic, Diffraction, Radiation)

#Report time histories
PPTH = model['Keystone'].PanelPressureTimeHistory(diff, panel_IDs, period, parameters)

#3D scatter plot of pressure reported at a simulation time of 5s.
ax = plt.axes(projection='3d')
scatter = ax.scatter3D(x, y, z, c=PPTH[0], marker='.')
ax.set_aspect('equal')
ax.set_axis_off()
ax.set_title('Panel pressure at {}s'.format(period.FromTime))
plt.colorbar(scatter).ax.set_ylabel('Pressure kN/m^2') 
plt.show()