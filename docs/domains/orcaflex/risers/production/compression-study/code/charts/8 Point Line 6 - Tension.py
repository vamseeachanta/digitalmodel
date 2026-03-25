
python
import matplotlib.pyplot as pyplot    
import numpy as np      
import OrcFxAPI                            
#Importing 8 point Mooring Static File
model = OrcFxAPI.Model('SS_Off_0_None_8P.sim')          
#Declaring Line6    
line = model['Line6'] 
#Time Period Defination
TimePeriod = OrcFxAPI.SpecifiedPeriod(9, 10)  
#Range Graph for this simulation         
Static8PL6 = line.RangeGraph('Effective Tension', TimePeriod) 
#Importing 8 point Mooring Static File
model = OrcFxAPI.Model('SS_Off_0_Wind_8P.sim')     
#Declaring Line6          
line = model['Line6'] 
#Time Period Defination
TimePeriod = OrcFxAPI.SpecifiedPeriod(9, 10) 
#Range Graph for this simulation           
Wind8PL6 = line.RangeGraph('Effective Tension', TimePeriod)
#Importing 8 point Mooring Static File
model = OrcFxAPI.Model('SS_Off_0_Current_8P.sim') 
#Declaring Line6          
line = model['Line6'] 
#Time Period Defination
TimePeriod = OrcFxAPI.SpecifiedPeriod(9, 10)  
#Range Graph for this simulation       
Current8PL6 = line.RangeGraph('Effective Tension', TimePeriod)
#Importing 8 point Mooring Static File
model = OrcFxAPI.Model('SS_Off_0_All_8P.sim') 
#Declaring Line6             
line = model['Line6'] 
#Time Period Defination
TimePeriod = OrcFxAPI.SpecifiedPeriod(9, 10) 
#Range Graph for this simulation                 
All8PL6 = line.RangeGraph('Effective Tension', TimePeriod)
# it creates plot tittle, color and size
pyplot.title('Effective Tension Along Highest Tension Mooring (Line6)', fontsize=14, fontweight='bold', color='black')
#it creates plot X-axis name, fontsize and colour
pyplot.xlabel('Mooring Line Length (m)', fontsize=12, fontweight='bold', color='black') 
#it creates plot Y-axis name, fontsize and colour             
pyplot.ylabel('Effective Tension  (mT)', fontsize=12, fontweight='bold', color='black')
#it converted Static max.effective tension value KN to Te and it plots these values along with arc length
pyplot.plot(Static8PL6.X, Static8PL6.Max/9.81, label='Static') 
#it converted Wind max.effective tension value KN to Te and it plots these values along with arc length
pyplot.plot(Wind8PL6.X, Wind8PL6.Max/9.81, label='Wind') 
#it converted Current max.effective tension value KN to Te and it plots these values along with arc length
pyplot.plot(Current8PL6.X, Current8PL6.Max/9.81, label='Current') 
#it converted all max.effective tension value KN to Te and it plots these values along with arc length
pyplot.plot(All8PL6.X, All8PL6.Max/9.81, label='All')
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
pyplot.savefig("8P_ET along high Tension Line6.png",dpi=800) 
# it shows the plot on screen  
pyplot.show()







