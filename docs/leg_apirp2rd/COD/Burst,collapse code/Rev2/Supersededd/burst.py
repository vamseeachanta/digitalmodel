import math

from DataProvision.burst_DNVOSF101 import *


fytemp =1.14-T/850  #Temperature Derating Factor for the Yield Stress

if(T>E):
    print(fytemp)
else:
    print(i)

futemp = i*1.25     #Temperature Derating Factor for the Tensile Stress (fu,temp)

fy = (SMYS-fytemp)*αu  #Yield Strength
fu = (SMTS-futemp)*αu	#Tensile Strength

msd = (Mf*ϒF*ϒc)+(ME*ϒE)+(Mf1*ϒF1*αu)+(MA*ϒA*ϒc)

αc = (1-β)+(β*(fu/fy))
mp =(fy*((D-Ot2)**2)*Ot2) #Plastic Bending Moment Resistance (Mp(t2))

Ssd= (TeF*ϒF*ϒc)+(TeE*ϒE)+(TeF1*ϒF1*ϒc)+(TeA*ϒA*ϒc)

sp= fy*3.14*(D-Ot2)*Ot2 #Plastic Axial Force Resistance (Sp)


b=(((ϒm*ϒSC*(msd)/(αc*mp))+(ϒm*ϒSC*Ssd/(αc*sp))**2)**2+(αp*(Pi-Pe)/αc*Pd)**2)
print (b)
                                                       


