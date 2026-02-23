exit()
python
import matplotlib.pyplot as pyplot  
import numpy as np 
import csv     
import OrcFxAPI                            
#Importing 8 point Mooring Static File
model = OrcFxAPI.Model('EL_Slack.sim')          
#Declaring Line6   
line = model['Production Riser']
vessel = model['Ship']
link = model['Tensioner 1'] 
#Time Period Defination
TimePeriod1 = OrcFxAPI.SpecifiedPeriod(0, 200) 
TimePeriod2 = OrcFxAPI.SpecifiedPeriod(0, 0.1)
TimePeriod3 = OrcFxAPI.SpecifiedPeriod(100, 200)
TimePeriod4 = OrcFxAPI.SpecifiedPeriod(100, 100.1) 
#Time History for Complete Simulation     
TensionerEndAZComplete = link.TimeHistory('End A Z', TimePeriod1)
TensionerEndBZComplete = link.TimeHistory('End B Z', TimePeriod1)
#Time History for 100 to 200 seconds
TensionerEndAZHalf = link.TimeHistory('End A Z', TimePeriod3)
TensionerEndBZHalf = link.TimeHistory('End B Z', TimePeriod3)
#Time History on X axis
Times1 = model.SampleTimes(TimePeriod1)
Times3 = model.SampleTimes(TimePeriod3)
#Intermediate Calculations for Complete Simulation
TensionerLengthComplete = TensionerEndAZComplete - TensionerEndBZComplete
#Intermediate Calculations for 100 to 200 seconds
TensionerLengthHalf = TensionerEndAZHalf - TensionerEndBZHalf
#Intermediate Calculations for 0 second time period
TensionerEndAZConstant1 = link.TimeHistory('End A Z', TimePeriod2)
TensionerEndBZConstant1 = link.TimeHistory('End B Z', TimePeriod2)
TensionerLengthConstant1 = TensionerEndAZConstant1 - TensionerEndBZConstant1
#Intermediate Calculations for 100 second time period
TensionerEndAZConstant2 = link.TimeHistory('End A Z', TimePeriod4)
TensionerEndBZConstant2 = link.TimeHistory('End B Z', TimePeriod4)
TensionerLengthConstant2 = TensionerEndAZConstant2 - TensionerEndBZConstant2
#Tensioner Stroke on Y axis
TensionerStrokeComplete = (TensionerLengthComplete-max(TensionerLengthConstant1))/0.3048
#Riser Slack on Y axis
TensionerStrokeHalf = (TensionerLengthHalf-max(TensionerLengthConstant1))/0.3048
ShipZ = vessel.TimeHistory('Z', TimePeriod1)
ShipZConstant =  vessel.TimeHistory('Z', TimePeriod2)
VesselHeave = (ShipZ-max(ShipZConstant))/0.3048
RiserSlack=(TensionerEndBZHalf-min(TensionerEndBZConstant1))/0.3048+max(TensionerLengthConstant2-TensionerLengthConstant1)/0.3048
#It creates plot tittle, color and size
pyplot.title('Tensioner Stroke and Riser Slack', fontsize=14, fontweight='bold', color='black')
#It creates plot X-axis name, fontsize and colour
pyplot.xlabel('Time (s)', fontsize=12, fontweight='bold', color='black')              
#It creates plot Y-axis name, fontsize and colour
pyplot.ylabel('Distance  (m)', fontsize=12, fontweight='bold', color='black')
#It converted Static max.effective tension value KN to Te and it plots these values along with arc length 
pyplot.plot(Times1, TensionerStrokeComplete, label='Tension Stroke') 
#It converted Wind max.effective tension value KN to Te and it plots these values along with arc length
pyplot.plot(Times1, VesselHeave, label='Vessel Heave') 
#It converted Current max.effective tension value KN to Te and it plots these values along with arc length
pyplot.plot(Times3, RiserSlack, label='Rser Slack')
#It creates legend of plot 
pyplot.legend()  
#It adjusts the legend place and size         
pyplot.legend(loc="upper right", markerscale=0.7, fontsize=10)
pyplot.axvline(x=100, ymin=-70,ymax=20, linewidth=1, linestyle='--', color="black")
#It defines range of y axis         
pyplot.yticks(range(-80, 30, 10))
#It defines range of X axis    
pyplot.xticks(range(0, 200, 20)) 
#It creates axis lines      
pyplot.grid(True)
#It save the plot with 800 pixel 
pyplot.savefig("Riser slack & Tensioner stroke.png",dpi=800) 
#It shows the plot on screen   
pyplot.show()



