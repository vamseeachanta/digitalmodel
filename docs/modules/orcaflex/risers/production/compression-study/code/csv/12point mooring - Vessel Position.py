
python
import matplotlib.pyplot as pyplot    
import numpy as np
import csv      
import OrcFxAPI      
#Importing 12 point Mooring Static File                       
model = OrcFxAPI.Model('SS_Off_0_None_12P.sim') 
#Declaring Ship              
Vessel = model['Ship']
#Period Defination
Period = OrcFxAPI.pnLatestWave
#Declaring X position of Vessel in static condition
StaticX=Vessel.TimeHistory('X', Period)
#Declaring Y position of Vessel in static condition
StaticY=Vessel.TimeHistory('Y', Period)
#Importing 12 point Mooring Wind File 
model = OrcFxAPI.Model('SS_Off_0_Wind_12P.sim') 
#Declaring Ship              
Vessel = model['Ship']
#Period Defination
Period = OrcFxAPI.pnLatestWave
#Declaring X position of Vessel in Wind condition
WindX=Vessel.TimeHistory('X', Period)
#Declaring Y position of Vessel in Wind condition
WindY=Vessel.TimeHistory('Y', Period)
#Importing 12 point Mooring Current File
model = OrcFxAPI.Model('SS_Off_0_Current_12P.sim')   
#Declaring Ship            
Vessel = model['Ship']
#Period Defination
Period = OrcFxAPI.pnLatestWave
#Declaring X position of Vessel in Current condition
CurrentX=Vessel.TimeHistory('X', Period)
#Declaring Y position of Vessel in Current condition
CurrentY=Vessel.TimeHistory('Y', Period)
#Importing 12 point Mooring All File
model = OrcFxAPI.Model('SS_Off_0_All_12P.sim')  
#Declaring Ship             
Vessel = model['Ship']
#Period Defination
Period = OrcFxAPI.pnLatestWave
#Declaring X position of Vessel in All condition
AllX=Vessel.TimeHistory('X', Period)
#Declaring Y position of Vessel in All condition
AllY=Vessel.TimeHistory('Y', Period)
#Defines X axis values
XRange=max(StaticX), max(WindX), max(CurrentX), max(AllX)
#Defines Y axis values
YRange=max(StaticY), max(WindY), max(CurrentY), max(AllY)
#It creates new csv file
b = open('12P_vessel position.csv', 'w', newline='')
#It allows writing on csv file
a = csv.writer(b)
#It gives data
data = [XRange, YRange]
#It creates row on that data
a.writerows(data)
#It closes the csv file
b.close()
