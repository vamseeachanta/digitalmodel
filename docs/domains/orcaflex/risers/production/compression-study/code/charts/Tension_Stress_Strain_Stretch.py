
python
import matplotlib.pyplot as pyplot  
import matplotlib.pyplot as plt  
from matplotlib import rc
rc('mathtext', default='regular')
import numpy as np 
import csv     
import OrcFxAPI                            
#Importing 8 point Mooring Static File
model = OrcFxAPI.Model('EL.sim')          
#Declaring Line6   
line = model['ProductionRiser'] 
ArcLengthTop = OrcFxAPI.oeArcLength(20)
#Time Period Defination
TimePeriod = OrcFxAPI.SpecifiedPeriod(0, 200) 
#Range Graph for this simulation      
TensionEL = line.TimeHistory('Effective Tension', TimePeriod, ArcLengthTop)
Times = model.SampleTimes(TimePeriod)
EL_Stress = line.TimeHistory('Max von Mises Stress', TimePeriod, ArcLengthTop)
EL_Strain = line.TimeHistory('Max pipelay von Mises Strain', TimePeriod, ArcLengthTop)
model = OrcFxAPI.Model('RO.sim')
line = model['ProductionRiser'] 
ArcLengthTop = OrcFxAPI.oeArcLength(20)
ArcLength2 = OrcFxAPI.oeArcLength(1842.5)
#Time Period Defination
TimePeriod = OrcFxAPI.SpecifiedPeriod(0, 200) 
#Range Graph for this simulation  
RO_Stress = line.TimeHistory('Max von Mises Stress', TimePeriod, ArcLengthTop)
RO_Strain = line.TimeHistory('Max pipelay von Mises Strain', TimePeriod, ArcLengthTop)
fig = plt.figure()
fig.suptitle('Strain Response of Riser')
ax = fig.add_subplot(111)
ax.plot(Times, EL_Strain, '-g', label='Strain (Elastic)')
ax.plot(Times, RO_Strain, '--g', label='Strain (Elastic-Plastic)')
ax2 = ax.twinx()
ax2.plot(Times, TensionEL/9.81/0.4536, '--b', label='Tension ')
ax.grid()
ax.set_xlabel("Time (s)")
ax.set_ylabel(r"Strain (%)")
ax2.set_ylabel(r"Cart Tension for 1 Riser")
ax2.set_ylim(0,20000,2000 )
ax.set_ylim(0,1)
ax.legend(loc=2)
ax2.legend(loc=1)
plt.savefig("Strain - Tension_Stretch.png",dpi=800)
fig = plt.figure()
fig.suptitle('Stress Response of Riser')
ax = fig.add_subplot(111)
ax.plot(Times, EL_Stress/1000*0.145, '-g', label='Stress (Elastic)')
ax.plot(Times, RO_Stress/1000*0.145, '--g', label='Stress (Elastic-Plastic)')
ax2 = ax.twinx()
ax2.plot(Times, TensionEL/9.81/0.4536, '--b', label='Tension ')
ax.grid()
ax.set_xlabel("Time (s)")
ax.set_ylabel(r"Stress (ksi)")
ax2.set_ylabel(r"Cart Tension for 1 Riser")
ax2.set_ylim(0,20000 )
ax.set_ylim(0,160)
ax.legend(loc=2)
ax2.legend(loc=1)
plt.savefig("Stress - Tension_Stretch.png",dpi=800)
plt.show()

























