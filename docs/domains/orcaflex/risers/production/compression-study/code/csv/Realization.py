
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
Static_0F=Link.TimeHistory('Length', Period)
Static_0Fm = max(Static_0F)/0.3048
#Importing No Load  2% offset File 
model = OrcFxAPI.Model('SS_NL_2F.sim')
#Declaring Stroke Tensioner              
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
NL_2F=Link.TimeHistory('Length', Period)
DS_NL_2F = (max(NL_2F)-min(Static_0F))/0.3048
#Importing no Load  4% offset File 
model = OrcFxAPI.Model('SS_NL_4F.sim')
#Declaring Stroke Tensioner              
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
NL_4F=Link.TimeHistory('Length', Period)
DS_NL_4F = (max(NL_4F)-min(Static_0F))/0.3048
#Importing no Load  6% offset File  
model = OrcFxAPI.Model('SS_NL_6F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
NL_6F=Link.TimeHistory('Length', Period)
DS_NL_6F = (max(NL_6F)-min(Static_0F))/0.3048
#Importing no Load  8% offset File  
model = OrcFxAPI.Model('SS_NL_8F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
NL_8F=Link.TimeHistory('Length', Period)
#Importing no Load  8% offset File  
model = OrcFxAPI.Model('SS_NL_10F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
NL_10F=Link.TimeHistory('Length', Period)
#Importing Current  4% offset File  
model = OrcFxAPI.Model('SS_Current_4F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
Current_4F=Link.TimeHistory('Length', Period)
#Importing 100yr seed1  4% offset File  
model = OrcFxAPI.Model('SS_100yr_S1_4F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
S1_100yr_4F=Link.TimeHistory('Length', Period)
US_S1_100yr_4F = (min(Static_0F)-min(S1_100yr_4F))/0.3048
DS_S1_100yr_4F = (max(S1_100yr_4F)-min(Static_0F))/0.3048
#Importing 100yr seed2  4% offset File  
model = OrcFxAPI.Model('SS_100yr_S2_4F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
S2_100yr_4F=Link.TimeHistory('Length', Period)
US_S2_100yr_4F = (min(Static_0F)-min(S2_100yr_4F))/0.3048
DS_S2_100yr_4F = (max(S2_100yr_4F)-min(Static_0F))/0.3048
#Importing 100yr seed3  4% offset File  
model = OrcFxAPI.Model('SS_100yr_S3_4F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
S3_100yr_4F=Link.TimeHistory('Length', Period)
US_S3_100yr_4F = (min(Static_0F)-min(S3_100yr_4F))/0.3048
DS_S3_100yr_4F = (max(S3_100yr_4F)-min(Static_0F))/0.3048
#Importing 100yr seed4  4% offset File  
model = OrcFxAPI.Model('SS_100yr_S4_4F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
S4_100yr_4F=Link.TimeHistory('Length', Period)
US_S4_100yr_4F = (min(Static_0F)-min(S4_100yr_4F))/0.3048
DS_S4_100yr_4F = (max(S4_100yr_4F)-min(Static_0F))/0.3048
#Importing 100yr seed5  4% offset File  
model = OrcFxAPI.Model('SS_100yr_S5_4F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
S5_100yr_4F=Link.TimeHistory('Length', Period)
US_S5_100yr_4F = (min(Static_0F)-min(S5_100yr_4F))/0.3048
DS_S5_100yr_4F = (max(S5_100yr_4F)-min(Static_0F))/0.3048
#Importing 100yr seed6  4% offset File  
model = OrcFxAPI.Model('SS_100yr_S6_4F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
S6_100yr_4F=Link.TimeHistory('Length', Period)
US_S6_100yr_4F = (min(Static_0F)-min(S6_100yr_4F))/0.3048
DS_S6_100yr_4F = (max(S6_100yr_4F)-min(Static_0F))/0.3048
#Importing 100yr seed7  4% offset File  
model = OrcFxAPI.Model('SS_100yr_S7_4F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
S7_100yr_4F=Link.TimeHistory('Length', Period)
US_S7_100yr_4F = (min(Static_0F)-min(S7_100yr_4F))/0.3048
DS_S7_100yr_4F = (max(S7_100yr_4F)-min(Static_0F))/0.3048
#Importing 100yr seed8  4% offset File  
model = OrcFxAPI.Model('SS_100yr_S8_4F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
S8_100yr_4F=Link.TimeHistory('Length', Period)
US_S8_100yr_4F = (min(Static_0F)-min(S8_100yr_4F))/0.3048
DS_S8_100yr_4F = (max(S8_100yr_4F)-min(Static_0F))/0.3048
#Importing 100yr seed9  4% offset File  
model = OrcFxAPI.Model('SS_100yr_S9_4F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
S9_100yr_4F=Link.TimeHistory('Length', Period)
US_S9_100yr_4F = (min(Static_0F)-min(S9_100yr_4F))/0.3048
DS_S9_100yr_4F = (max(S9_100yr_4F)-min(Static_0F))/0.3048
#Importing 100yr seed10  4% offset File  
model = OrcFxAPI.Model('SS_100yr_S10_4F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
S10_100yr_4F=Link.TimeHistory('Length', Period)
US_S10_100yr_4F = (min(Static_0F)-min(S10_100yr_4F))/0.3048
DS_S10_100yr_4F = (max(S10_100yr_4F)-min(Static_0F))/0.3048
#Importing 100yr seed1  6% offset File  
model = OrcFxAPI.Model('SS_1000yr_s1_6F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
S1_1000yr_6F=Link.TimeHistory('Length', Period)
US_S1_1000yr_6F = (min(Static_0F)-min(S1_1000yr_6F))/0.3048
DS_S1_1000yr_6F = (max(S1_1000yr_6F)-min(Static_0F))/0.3048
#Importing 100yr seed2  6% offset File  
model = OrcFxAPI.Model('SS_1000yr_s2_6F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
S2_1000yr_6F=Link.TimeHistory('Length', Period)
US_S2_1000yr_6F = (min(Static_0F)-min(S2_1000yr_6F))/0.3048
DS_S2_1000yr_6F = (max(S2_1000yr_6F)-min(Static_0F))/0.3048
#Importing 100yr seed3  6% offset File  
model = OrcFxAPI.Model('SS_1000yr_s3_6F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
S3_1000yr_6F=Link.TimeHistory('Length', Period)
US_S3_1000yr_6F = (min(Static_0F)-min(S3_1000yr_6F))/0.3048
DS_S3_1000yr_6F = (max(S3_1000yr_6F)-min(Static_0F))/0.3048
#Importing 100yr seed4  6% offset File  
model = OrcFxAPI.Model('SS_1000yr_s4_6F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
S4_1000yr_6F=Link.TimeHistory('Length', Period)
US_S4_1000yr_6F = (min(Static_0F)-min(S4_1000yr_6F))/0.3048
DS_S4_1000yr_6F = (max(S4_1000yr_6F)-min(Static_0F))/0.3048
#Importing 100yr seed5  6% offset File  
model = OrcFxAPI.Model('SS_1000yr_s5_6F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
S5_1000yr_6F=Link.TimeHistory('Length', Period)
US_S5_1000yr_6F = (min(Static_0F)-min(S5_1000yr_6F))/0.3048
DS_S5_1000yr_6F = (max(S5_1000yr_6F)-min(Static_0F))/0.3048
#Importing 100yr seed6  6% offset File  
model = OrcFxAPI.Model('SS_1000yr_s6_6F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
S6_1000yr_6F=Link.TimeHistory('Length', Period)
US_S6_1000yr_6F = (min(Static_0F)-min(S6_1000yr_6F))/0.3048
DS_S6_1000yr_6F = (max(S6_1000yr_6F)-min(Static_0F))/0.3048
#Importing 100yr seed7  6% offset File  
model = OrcFxAPI.Model('SS_1000yr_s7_6F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
S7_1000yr_6F=Link.TimeHistory('Length', Period)
US_S7_1000yr_6F = (min(Static_0F)-min(S7_1000yr_6F))/0.3048
DS_S7_1000yr_6F = (max(S7_1000yr_6F)-min(Static_0F))/0.3048
#Importing 100yr seed8  6% offset File  
model = OrcFxAPI.Model('SS_1000yr_s8_6F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
S8_1000yr_6F=Link.TimeHistory('Length', Period)
US_S8_1000yr_6F = (min(Static_0F)-min(S8_1000yr_6F))/0.3048
DS_S8_1000yr_6F = (max(S8_1000yr_6F)-min(Static_0F))/0.3048
#Importing 100yr seed9  6% offset File  
model = OrcFxAPI.Model('SS_1000yr_s9_6F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
S9_1000yr_6F=Link.TimeHistory('Length', Period)
US_S9_1000yr_6F = (min(Static_0F)-min(S9_1000yr_6F))/0.3048
DS_S9_1000yr_6F = (max(S9_1000yr_6F)-min(Static_0F))/0.3048
#Importing 100yr seed10  6% offset File  
model = OrcFxAPI.Model('SS_1000yr_s10_6F.sim')
#Declaring Stroke Tensioner             
Link = model['Stroke Tensioner']
#Period Defination
Period = OrcFxAPI.pnWholeSimulation
#Declaring stroke length of stroke tensioner
S10_1000yr_6F=Link.TimeHistory('Length', Period)
US_S10_1000yr_6F = (min(Static_0F)-min(S10_1000yr_6F))/0.3048
DS_S10_1000yr_6F = (max(S10_1000yr_6F)-min(Static_0F))/0.3048
US_100yrHurricane = US_S1_100yr_4F + DS_NL_4F,  US_S2_100yr_4F + DS_NL_4F, US_S3_100yr_4F + DS_NL_4F, US_S6_100yr_4F + DS_NL_4F, US_S5_100yr_4F + DS_NL_4F, US_S7_100yr_4F + DS_NL_4F, US_S8_100yr_4F + DS_NL_4F, US_S9_100yr_4F + DS_NL_4F, US_S10_100yr_4F + DS_NL_4F, 
US_1000yrHurricane = US_S1_1000yr_6F + DS_NL_6F,  US_S2_1000yr_6F + DS_NL_6F, US_S3_1000yr_6F + DS_NL_6F, US_S4_1000yr_6F + DS_NL_6F, US_S5_1000yr_6F + DS_NL_6F, US_S6_1000yr_6F + DS_NL_6F, US_S7_1000yr_6F + DS_NL_6F, US_S8_1000yr_6F + DS_NL_6F, US_S9_1000yr_6F + DS_NL_6F, US_S10_1000yr_6F + DS_NL_6F
DS_100yrHurricane = -(DS_S1_100yr_4F - DS_NL_4F),  -(DS_S2_100yr_4F - DS_NL_4F), -(DS_S3_100yr_4F - DS_NL_4F), -(DS_S6_100yr_4F - DS_NL_4F), -(DS_S5_100yr_4F - DS_NL_4F), -(DS_S7_100yr_4F - DS_NL_4F), -(DS_S8_100yr_4F - DS_NL_4F), -(DS_S9_100yr_4F - DS_NL_4F), -(DS_S10_100yr_4F - DS_NL_4F), 
DS_1000yrHurricane = -(DS_S1_1000yr_6F - DS_NL_6F),  -(DS_S2_1000yr_6F - DS_NL_6F), -(DS_S3_1000yr_6F - DS_NL_6F), -(DS_S4_1000yr_6F - DS_NL_6F), -(DS_S5_1000yr_6F - DS_NL_6F), -(DS_S6_1000yr_6F - DS_NL_6F), -(DS_S7_1000yr_6F - DS_NL_6F), -(DS_S8_1000yr_6F - DS_NL_6F), -(DS_S9_1000yr_6F - DS_NL_6F), -(DS_S10_1000yr_6F - DS_NL_6F)
Realization100yr = 1, 2, 3, 4, 5, 6, 7, 8, 9
Realization1000yr = 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
#It creates new csv file
b = open('Realization.csv', 'w', newline='')
#It allows writing on csv file
a = csv.writer(b)
#It gives data
US100yr = Realization100yr, US_100yrHurricane
US1000yr = US_1000yrHurricane
DS100yr = DS_100yrHurricane
DS1000yr = DS_1000yrHurricane
#It creates row on that data
a.writerows(US100yr)
a.writerow(US1000yr)
a.writerow(DS100yr)
a.writerow(DS1000yr)
b.close()

