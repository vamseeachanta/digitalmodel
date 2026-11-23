
python
import matplotlib.pyplot as pyplot    
import numpy as np      
import OrcFxAPI                            
#Importing 8 point Mooring Static File
model = OrcFxAPI.Model('SS_Off_0_None_12P.sim')          
#Declaring Line8    
line = model['Line8'] 
#Time Period Defination
TimePeriod = OrcFxAPI.SpecifiedPeriod(9, 10)  
#Range Graph for this simulation         
Static12PL8 = line.RangeGraph('Effective Tension', TimePeriod) 
#Importing 8 point Mooring Wind File
model = OrcFxAPI.Model('SS_Off_0_Wind_12P.sim')     
#Declaring Line8          
line = model['Line8'] 
#Time Period Defination
TimePeriod = OrcFxAPI.SpecifiedPeriod(9, 10) 
#Range Graph for this simulation           
Wind12PL8 = line.RangeGraph('Effective Tension', TimePeriod)
#Importing 8 point Mooring Current File
model = OrcFxAPI.Model('SS_Off_0_Current_12P.sim') 
#Declaring Line8          
line = model['Line8'] 
#Time Period Defination
TimePeriod = OrcFxAPI.SpecifiedPeriod(9, 10)  
#Range Graph for this simulation       
Current12PL8 = line.RangeGraph('Effective Tension', TimePeriod)
#Importing 8 point Mooring All File
model = OrcFxAPI.Model('SS_Off_0_All_12P.sim') 
#Declaring Line8             
line = model['Line8'] 
#Time Period Defination
TimePeriod = OrcFxAPI.SpecifiedPeriod(9, 10) 
#Range Graph for this simulation                 
AlL82PL8 = line.RangeGraph('Effective Tension', TimePeriod)
# it creates plot tittle, color and size
pyplot.title('Effective Tension Along Highest Tension Mooring (Line8)', fontsize=14, fontweight='bold', color='black')
#it creates plot X-axis name, fontsize and colour
pyplot.xlabel('Mooring Line Length (m)', fontsize=12, fontweight='bold', color='black') 
#it creates plot Y-axis name, fontsize and colour             
pyplot.ylabel('Effective Tension  (mT)', fontsize=12, fontweight='bold', color='black')
#it converted Static max.effective tension value KN to Te and it plots these values along with arc length
pyplot.plot(Static12PL8.X, Static12PL8.Max/9.81, label='Static') 
#it converted Wind max.effective tension value KN to Te and it plots these values along with arc length
pyplot.plot(Wind12PL8.X, Wind12PL8.Max/9.81, label='Wind') 
#it converted Current max.effective tension value KN to Te and it plots these values along with arc length
pyplot.plot(Current12PL8.X, Current12PL8.Max/9.81, label='Current') 
#it converted all max.effective tension value KN to Te and it plots these values along with arc length
pyplot.plot(AlL82PL8.X, AlL82PL8.Max/9.81, label='All')
# it creates legend of plot 
pyplot.legend()  
# It adjusts the legend place and size        
pyplot.legend(loc="lower center", markerscale=0.7, fontsize=10) 
# it defines range of y axis        
pyplot.yticks(range(0, 1300, 100)) 
# it defines range of  axis   
pyplot.xticks(range(0, 3500, 250))
#it creates axis lines       
pyplot.grid(True)
# it save the plot with 800 pixel 
pyplot.savefig("12P_ET along high Tension Line8.png",dpi=800) 
# it shows the plot on screen  
pyplot.show()