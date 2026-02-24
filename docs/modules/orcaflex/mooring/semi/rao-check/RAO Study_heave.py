python
import matplotlib.pyplot as pyplot    
import numpy as np 
import matplotlib.pyplot as plt  
from matplotlib import rc
rc('mathtext', default='regular')
import csv     
import OrcFxAPI                            
#Importing 1000yr WH no riser model
model = OrcFxAPI.Model('LR_1000yr_WH_0R.sim')            
Vessel = model['SemiSub']
TimePeriod0 = OrcFxAPI.SpecifiedPeriod(250, 300)
Time = model.SampleTimes(TimePeriod0) 
#Declaring Z position of Vessel in static condition
Z_OR=Vessel.TimeHistory('Z', TimePeriod0)
#Importing 1000yr WH 1 riser model file
model = OrcFxAPI.Model('LR_1000yr_WH_1R.sim')          
#Declaring Line   
line = model['Production Riser'] 
Vessel = model['SemiSub']
TimePeriod1 = OrcFxAPI.SpecifiedPeriod(250, 300) 
#Declaring Z position of Vessel in static condition
Z_1R=Vessel.TimeHistory('Z', TimePeriod1)
#Importing 1000yr WH 2 riser model File
model = OrcFxAPI.Model('LR_1000yr_WH_2R.sim')
line = model['Production Riser1'] 
Vessel = model['SemiSub']
TimePeriod2 = OrcFxAPI.SpecifiedPeriod(250, 300) 
#Declaring Z position of Vessel in static condition
Z_2R=Vessel.TimeHistory('Z', TimePeriod2)
pyplot.title('Vessel Position-Heave(Z)', fontsize=14, fontweight='bold', color='black')
#It creates plot X-axis name, fontsize and colour
pyplot.xlabel('Time  (s)', fontsize=12, fontweight='bold', color='black')  
#it creates plot Y-axis name, fontsize and colour            
pyplot.ylabel('Heave (m)', fontsize=12, fontweight='bold', color='black')
#it converted Static max.effective tension value KN to Te and it plots these values along with arc length
pyplot.plot(Time, Z_OR, label='NoRiser')
pyplot.plot(Time, Z_1R, label='1Riser')
pyplot.plot(Time, Z_2R, label='2Risers')
pyplot.grid(True)
pyplot.legend()
#It save the plot with 800 pixel
pyplot.savefig("RAoStudyVessel_Z.png",dpi=800)
pyplot.show()


#Itcreates .csv file
OpenCSV = open('Vessel Heave_RAoStudy.csv', 'w')
#It allows writing on csv file
WriteCSV = csv.writer(OpenCSV)
#It creates row on that data
WriteCSV.writerow(Time)
WriteCSV.writerow(Z_OR)
WriteCSV.writerow(Z_1R)
WriteCSV.writerow(Z_2R)
OpenCSV.close()




