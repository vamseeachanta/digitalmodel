
#import scipy as spy
#import numpy as npy
from math import *
from DataProvision.burst_DNVOSF201 import *

fytemp =1.14-T/850  #Temperature Derating Factor for the Yield Stress

if(T>E):
    print(fytemp)
else:
    print(i)

futemp = i*1.25     #Temperature Derating Factor for the Tensile Stress (fu,temp)

fy = (SMYS-fytemp)*αu  #Yield Strength
fu = (SMTS-futemp)*αu	#Tensile Strength

###equation 5.9
##fcb=(min(fy,fu/1.15))
##print (fcb)

#equation 5.15 pressure  resistance pb(t) # Equation 5.15

pbt=2/sqrt(3)*(2*t)/(D-t)*min(fy,fu/1.15)
print("pressure  resistance pb(t)=",pbt)

pld=Pd+r1*g*h  # r1= ρi #Local internal design pressure

pli=pld+0.1*Pd #Local incidental pressure,# Equation 5.16

condition1=pli-Pe  # # Equation 5.14 Bursting
condition2=pbt/(ϒm*ϒSC)
print (condition1,condition2)
if condition1==condition2:
    print("Pressure Containment is correct")
elif condition1<condition2:
    print("Pressure Containment is correct")

#collapse
pp=2*(t/D)*fy*αfab   #Equation 5.20

pel=2*E*((t/D)**3)/(1-V**2) #Equation 5.19



