
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
#It creates new csv file
b = open('8P_L1Tension.csv', 'w', newline='')
#It allows writing on csv file
a = csv.writer(b)
#It gives data
Staticdata = Static8PL1.X, Static8PL1.Max/9.81
Winddata = Wind8PL1.Max/9.81
Currentdata = Current8PL1.Max/9.81
Alldata = All8PL1.Max/9.81
#It creates row on that data
a.writerows(Staticdata)
a.writerow(Winddata)
a.writerow(Currentdata)
a.writerow(Alldata)
b.close()








