from math import sqrt

from DataProvision.burst_APIST2RD import *


#Burst Pressure Check

t= tfab-tca #To account for localized corrosion and wear, the minimum burst pressure capacity
                                                       

#the minimum burst pressure of outer riser pipe is:

x1=(D/(D-2*t))
pb = k*(S+U)*math.log(x1)

print ("the minimum burst pressure of outer riser pipe pb=",pb)


#Collapse Pressure Check

#Yield collapse pressure Py
py= 2*S*(t/D)
print ("yield collapse pressure Py=",py)

#elastic collapse pressure Pel
pel=2*E*(t/D)**3/(1-V**2)
print ("elastic collapse pressure Pel=",pel)

#plastic collapse pressure Pp

pp=2*t/D*S*αfab

print ("plastic collapse pressure Pp=",pp)

#collapse pressure Pc

pc=py*pel/sqrt(py**2+pel**2)
print ("collapse pressure pc=",pc)

#collapse pressure may alternatively be calculated as a function of the elastic capacity, plastic capacity and the ovality of the pipe as

condition1=(pc-pel)*(pc**2-pp**2)
condition2=pc*pel*pp*2*δ0*(D/t)

if (condition1==condition2):
    print (condition1)

#Collapse Due to Pure Bending
ϵb=t/2*D

print ("Collapse Due to Pure Bending ϵb=",ϵb)






    
