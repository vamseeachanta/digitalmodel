
python
import matplotlib.pyplot as pyplot    
import numpy as np
import csv      
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
#It creates new csv file
b = open('12P_L8Tension.csv', 'w', newline='')
#It allows writing on csv file
a = csv.writer(b)
#It gives data
Staticdata = Static12PL8.X, Static12PL8.Max/9.81
Winddata = Wind12PL8.Max/9.81
Currentdata = Current12PL8.Max/9.81
Alldata = All12PL8.Max/9.81
#It creates row on that data
a.writerows(Staticdata)
a.writerow(Winddata)
a.writerow(Currentdata)
a.writerow(Alldata)
b.close()