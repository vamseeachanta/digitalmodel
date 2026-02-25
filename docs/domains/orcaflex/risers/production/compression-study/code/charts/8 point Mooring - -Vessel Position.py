
python
import matplotlib.pyplot as pyplot    
import numpy as np      
import OrcFxAPI    
#Importing 8 point Mooring Static File                        
model = OrcFxAPI.Model('SS_Off_0_None_8P.sim')
#Declaring Ship              
Vessel = model['Ship']
#Period Defination
Period = OrcFxAPI.pnLatestWave
#Declaring X position of Vessel in static condition
StaticX=Vessel.TimeHistory('X', Period)
#Declaring Y position of Vessel in static condition
StaticY=Vessel.TimeHistory('Y', Period)
#Importing 8 point Mooring Wind File 
model = OrcFxAPI.Model('SS_Off_0_Wind_8P.sim')
#Declaring Ship              
Vessel = model['Ship']
#Period Defination
Period = OrcFxAPI.pnLatestWave
#Declaring X position of Vessel in Wind condition
WindX=Vessel.TimeHistory('X', Period)
#Declaring Y position of Vessel in Wind condition
WindY=Vessel.TimeHistory('Y', Period)
#Importing 8 point Mooring Current File  
model = OrcFxAPI.Model('SS_Off_0_Current_8P.sim')
#Declaring Ship                
Vessel = model['Ship']
#Period Defination
Period = OrcFxAPI.pnLatestWave
#Declaring X position of Vessel in Current condition
CurrentX=Vessel.TimeHistory('X', Period)
#Declaring Y position of Vessel in Current condition
CurrentY=Vessel.TimeHistory('Y', Period)
#Importing 8 point Mooring All File 
model = OrcFxAPI.Model('SS_Off_0_All_8P.sim')
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
#It creates plot tittle, color and size
pyplot.title('8Point Mooring - Vessel Position', fontsize=14, fontweight='bold', color='black')
#It creates plot X-axis name, fontsize and colour
pyplot.xlabel('Surge  (m)', fontsize=12, fontweight='bold', color='black')  
#it creates plot Y-axis name, fontsize and colour            
pyplot.ylabel('Sway (m)', fontsize=12, fontweight='bold', color='black')
#It plots X,y range vlues
pyplot.plot(XRange, YRange, 'bs')
#It creates axis lines 
pyplot.grid(True)
#It defines range of Y axis  
pyplot.yticks(range(-10, 110, 10))
#It defines range of X axis      
pyplot.xticks(range(-10, 110, 10))
#It write text on plot in static point
pyplot.text(-6, -6, r'Static')
#It write text on plot in Wind point
pyplot.text(45, 30, r'Wind')
#It write text on plot in Current point
pyplot.text(25, 45, r'Current')
#It write text on plot in All point
pyplot.text(70, 75, r'100 yr Hurricane')
#It save the plot with 800 pixel
pyplot.savefig("VesselPosition_8Point.png",dpi=800)
#It shows the plot on screen 
pyplot.show()


