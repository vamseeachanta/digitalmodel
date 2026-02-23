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
#It creates new csv file
b = open('RiserSlack_TensionStroke.csv', 'w', newline='')
#It allows writing on csv file
a = csv.writer(b)
#It gives data
TensionStroke = Times1, TensionerStrokeComplete
Vessel_Heave = VesselHeave
Riser_Slack = Times3, RiserSlack 
#It creates row on that data
a.writerows(TensionStroke)
a.writerow(Vessel_Heave)
a.writerows(Riser_Slack)
b.close()


