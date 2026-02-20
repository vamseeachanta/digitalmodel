
python
import matplotlib.pyplot as pyplot    
import numpy as np
import csv      
import OrcFxAPI    
#Importing 0 offset Static File                        
model = OrcFxAPI.Model('SS_Off_0_Static_8P.sim')
#Declaring Stroke Tensioner              
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
StaticLength=Link.TimeHistory('Length', Period)
#Importing No Load  2% offset File 
model = OrcFxAPI.Model('SS_NL_2F.sim')
#Declaring Stroke Tensioner              
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
Length2POffset=Link.TimeHistory('Length', Period)
#Importing no Load  4% offset File 
model = OrcFxAPI.Model('SS_NL_4F.sim')
#Declaring Stroke Tensioner              
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
Length4POffset=Link.TimeHistory('Length', Period)
#Importing no Load  6% offset File  
model = OrcFxAPI.Model('SS_NL_6F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
Length6POffset=Link.TimeHistory('Length', Period)
#Importing no Load  8% offset File  
model = OrcFxAPI.Model('SS_NL_8F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
Length8POffset=Link.TimeHistory('Length', Period)
#0% Offset
Offset_0 = max(StaticLength)-max(StaticLength)
YaxisOffset_0 = 0
#2% Offset
Offset_2 = max(StaticLength)-max(Length2POffset)
YaxisOffset_2 = 2
#4% Offset
Offset_4 = max(StaticLength)-max(Length4POffset)
YaxisOffset_4 = 4
#6% Offset
Offset_6 = max(StaticLength)-max(Length6POffset)
YaxisOffset_6 = 6
#8% Offset
Offset_8 = max(StaticLength)-max(Length8POffset)
YaxisOffset_8 = 8
#X Axis 
XOffset = (YaxisOffset_0, YaxisOffset_2, YaxisOffset_4, YaxisOffset_6, YaxisOffset_8) 
#Y Axis
YStrokes = Offset_0/0.308,Offset_2/0.308, Offset_4/0.308, Offset_6/0.308, Offset_8/0.308
#It creates new csv file
b = open('Offset.csv', 'w', newline='')
#It allows writing on csv file
a = csv.writer(b)
#It gives data
Offset = XOffset
Stroke = YStrokes
#It creates row on that data
a.writerow(Offset)
a.writerow(Stroke)
#It closes the csv file
b.close()


