
python
import matplotlib.pyplot as pyplot    
import numpy as np      
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
#It creates plot tittle, color and size
pyplot.title('Stroke (ft) vs. Offset', fontsize=14, fontweight='bold', color='black')
#It creates plot X-axis name, fontsize and colour
pyplot.xlabel('Offset (%)', fontsize=12, fontweight='bold', color='black')  
#it creates plot Y-axis name, fontsize and colour            
pyplot.ylabel('Stroke (ft)', fontsize=12, fontweight='bold', color='black')
#It plots X,y range vlues
pyplot.plot(XOffset, YStrokes, marker='o')
#It write text on plot
pyplot.text(6.2, -2, r'Max expected offset')
#It write text on plot
pyplot.text(6.2, -3, r'for all conditions')
#It creates axis lines 
pyplot.grid(True)
#It creates vertical line 
pyplot.axvline(x=6, ymin=0,ymax=20, linewidth=1, linestyle='--', color="red")
#It defines range of Y axis  
pyplot.yticks(range(-22, 2, 2))
#It defines range of X axis      
pyplot.xticks(range(-2, 12, 2))
#It save the plot with 800 pixel
pyplot.savefig("Offset.png",dpi=800)
#It shows the plot on screen 
pyplot.show()
