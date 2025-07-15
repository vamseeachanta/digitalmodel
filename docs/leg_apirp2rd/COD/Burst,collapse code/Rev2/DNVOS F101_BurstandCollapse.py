
#import scipy as spy
from math import *
from DataProvision.burst_DNVOSF101 import *

fytemp =1.14-T/850  #Temperature Derating Factor for the Yield Stress

if(T>e):
    print(fytemp)
else:
    print(i)

futemp = i*1.25     #Temperature Derating Factor for the Tensile Stress (fu,temp)

fy = (SMYS-fytemp)*αu  #Yield Strength
fu = (SMTS-futemp)*αu	#Tensile Strength

#equation 5.9
fcb=(min(fy,fu/1.15))
print (fcb)

#equation 5.8 pressure containment resistance pb(t)

pbt=2*t/D-t*fcb*2/sqrt(3)
print(pbt)

#equation 5.6
condition1=pbt/(ϒm*ϒSC)
condition2=((plt/αspt)-Pe)
condition3=((ph*αu )/αmpt)
rhs=(min(condition1, condition2, condition3))
lhs=pli-Pe
print(rhs, lhs)
if lhs==rhs:
    print("Pressure Containment is correct")
elif lhs<rhs:
    print("Pressure Containment is correct")
elif lhs>rhs:
    print("Please check the inpts, the calculation failed at pressure containment for burst pressure")

#equation 5.7
lhs1=pli-Pe
rhs1=(min(condition1,ph))
print(rhs1, lhs1)
if lhs1==rhs1:
    print("Pressure Containment is correct")
elif lhs1<rhs1:
    print("Pressure Containment is correct")
elif lhs1>rhs1:
    print("Please check the inpts, the calculation failed at pressure containment for burst pressure")


    
k=D/It1 #Ratio Of Effective Thickness and Diameter of Pipe where k=(D/t1)- Installation
h=1/k #Ratio Of Thickness and Diameter of Pipe h=(t1/D) - Installation

#Elastic Collapse Pressure with thickness t1 (Pel)
pel=2*E*h**3/(1-V**2)

#Plastic Collapse Pressue with wall thickness t1 (Pp)
pp=2*h*fy*αfab

#Containment Resistance or Burst Pressure (Pb)
pb=(2/sqrt(3))*(2*Ot2/(D-Ot2)*min(fy,fu/1.15))

#For collapse pressure t1 wall thickness
b=-pel
c=-(pp**2+pp*pel*(fo/100)*k)
u= 1/3*(-(1/3)*b**2+c)
d=(pel*pp**2)
v=1/2*(2/27*b**3-1/3*b*c+d)
φ= acos(-v/sqrt(-u**3))
y=-2*sqrt(-u)*cos((φ/3)+(60*3.14/180))

#Collapse Pressure for(Pct1)
pc=y-1/3*b

#Equation 5.10
#Local Buckling – external over pressure only (system collapse)
X1=Pe-pmin
X2=pc/ϒm*ϒSC
if(X1<X2):
    print("Accepted for Calculation")
if(X1==X2):
    print("Accepted for Calculation")

#Equation 5.11 The characteristic resistance for external pressure (pc) (collapse) shall be calculated as:

condition4=pc-pel*(pc**2-pp**2)
condition5=pc*pel*pp*fo*k
if (condition4==condition5):
    print(condition4)

# Load controlled condition
msd = (Mf*ϒF*ϒc)+(ME*ϒE)+(Mf1*ϒF1*αu)+(MA*ϒA*ϒc)

αc = (1-β)+(β*(fu/fy))
mp =(fy*((D-Ot2)**2)*Ot2) #Plastic Bending Moment Resistance (Mp(t2))

Ssd= (TeF*ϒF*ϒc)+(TeE*ϒE)+(TeF1*ϒF1*ϒc)+(TeA*ϒA*ϒc)

sp= fy*3.14*(D-Ot2)*Ot2 #Plastic Axial Force Resistance (Sp)


LC=(((ϒm*ϒSC*(msd)/(αc*mp))+(ϒm*ϒSC*Ssd/(αc*sp))**2)**2+(αp*(Pi-Pe)/αc*Pd)**2)
print (LC)

