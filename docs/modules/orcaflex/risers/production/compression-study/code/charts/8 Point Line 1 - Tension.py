
python
import matplotlib.pyplot as pyplot    
import numpy as np      
import OrcFxAPI      
#Importing 8 point Mooring Static File                      
model = OrcFxAPI.Model('SS_Off_0_None_8P.sim')
#Declaring Line8               
line = model['Line1'] 
#Time Period Defination
TimePeriod = OrcFxAPI.SpecifiedPeriod(9, 10)
#Range Graph for this simulation            
Static8PL1 = line.RangeGraph('Effective Tension', TimePeriod)
#Importing 8 point Mooring Wind File 
model = OrcFxAPI.Model('SS_Off_0_Wind_8P.sim') 
#Declaring Line8             
line = model['Line1']
#Time Period Defination
TimePeriod = OrcFxAPI.SpecifiedPeriod(9, 10)  
#Range Graph for this simulation         
Wind8PL1 = line.RangeGraph('Effective Tension', TimePeriod)
#Importing 8 point Mooring Current File
model = OrcFxAPI.Model('SS_Off_0_Current_8P.sim')
#Declaring Line8             
line = model['Line1'] 
#Time Period Defination
TimePeriod = OrcFxAPI.SpecifiedPeriod(9, 10)  
#Range Graph for this simulation          
Current8PL1 = line.RangeGraph('Effective Tension', TimePeriod)
#Importing 8 point Mooring All File
model = OrcFxAPI.Model('SS_Off_0_All_8P.sim')
#Declaring Line8               
line = model['Line1'] 
#Time Period Defination
TimePeriod = OrcFxAPI.SpecifiedPeriod(9, 10)
#Range Graph for this simulation            
All8PL1 = line.RangeGraph('Effective Tension', TimePeriod)
#It creates plot tittle, color and size
pyplot.title('Effective Tension Along Lowest Tension Mooring (Line1)', fontsize=14, fontweight='bold', color='black')
#It creates plot X-axis name, fontsize and colour
pyplot.xlabel('Mooring Line Length (m)', fontsize=12, fontweight='bold', color='black')              
#It creates plot Y-axis name, fontsize and colour
pyplot.ylabel('Effective Tension  (mT)', fontsize=12, fontweight='bold', color='black')
#It converted Static max.effective tension value KN to Te and it plots these values along with arc length 
pyplot.plot(Static8PL1.X, Static8PL1.Max/9.81, label='Static') 
#It converted Wind max.effective tension value KN to Te and it plots these values along with arc length
pyplot.plot(Wind8PL1.X, Wind8PL1.Max/9.81, label='Wind') 
#It converted Current max.effective tension value KN to Te and it plots these values along with arc length
pyplot.plot(Current8PL1.X, Current8PL1.Max/9.81, label='Current')
#It converted all max.effective tension value KN to Te and it plots these values along with arc length 
pyplot.plot(All8PL1.X, All8PL1.Max/9.81, label='All')
#It creates legend of plot 
pyplot.legend()  
#It adjusts the legend place and size         
pyplot.legend(loc="lower center", markerscale=0.7, fontsize=10)
#It defines range of y axis         
pyplot.yticks(range(0, 900, 100))
#It defines range of X axis    
pyplot.xticks(range(0, 3500, 250)) 
#It creates axis lines      
pyplot.grid(True)
#It save the plot with 800 pixel 
pyplot.savefig("8P_ET along high Tension Line1.png",dpi=800) 
#It shows the plot on screen   
pyplot.show()







