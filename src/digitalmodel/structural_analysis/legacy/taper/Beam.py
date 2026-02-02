import math

import matplotlib.pyplot as plt

#Beam Inputs

L=200 #Length of beam in mm
W=20  #Width of beam in mm
H=10  #Height of beam in mm
l1=1  #length increment to be used in finite difference calculation
E=200028 #Modulus Of Elasticity (N/mm^2)

I=1/12*W*H**3 #Moment Of Inertia
print("Moment Of Inertia I",I)
cc=H/2 #Distance farthest fiber to nuetral axis in mm

#Loading

F=0     #amplitude of the Point Load in N
af=50   #location of the point load, in mm
wa=0.5  #amplitude of the distributed load near the cantilever end, in N/mm
wL=0.5  #amplitude of the distributed load at the clamped end, in N/mm
aw=0    #location where the distributed load starts, in mm
M=0     #amplitude of the applied moment, in N*mm
am=25   #location of the moment load, in mm


dl=(L-aw)*((wa+wL)/2)#Resulted total distributed load

#Reactions at Beam Ends

Ra=F*(L-af)/L+(L-aw)**2*(wL+2*wa)/(6*L)-M/L #Reaction force at A
Rb=F*af/L+(L-aw)*(wL+wa-(L-aw)*(wL+2*wa)/(3*L))/2+M/L #Reaction force at B
Ma=0 #Reaction moment at A
MA=Ma/(I/cc)#Max bending stress due to MA (N/mm^2)
Mb=0 #Reaction moment at B
MB=Mb/(I/cc) #Max bending stress due to MB (N/mm^2)

ta=-F*af*(L-af)*(2*L-af)/(6*E*I*L)-(L-aw)**2*(wa*(L**2+2*aw*L-aw**2)+(wL-wa)*(7*L**2+6*aw*L-3*aw**2)/15)/(24*E*I*L)-M*(2*L**2-6*am*L+3*am**2)/(6*E*I*L) #Rotation at A (radians)
tb=F*af*(L**2-af**2)/(6*E*I*L)+(wa*(L**2-aw**2)**2+(wL-wa)*(L-aw)**2*(8*L**2+9*aw*L+3*aw**2)/15)/(24*E*I*L)+M*(L**2-3*am**2)/(6*E*I*L) #Rotation at B in (radians)
da=0 #Deflection at A
db=0 #Deflection at B


