# Collapse Code API STD 2RD
from math import sqrt

# Collapse Due To External Pressure
# inputs

s=80      # Maximum Yield Strength 80 ksi
d=2.375   # OD
t=0.1     # Wall Thickness Assumed
E=205     # Young's Modulus Gpa
v=0.3     # Poisson's ratio
Dmax=10   # assumed initial ovality
Dmin=5    # assumed initial ovality
αtab=1    # Seamless pipe



py=2*s*(t/d)

pel=(2*E*((t/d)**3)/(1-v**2))

pc=(py*pel)/sqrt(py**2+pel**2)

print (pc, " pc =Collapse Due To External Pressure")

#The collapse pressure may alternatively be calculated as a function of the elastic capacity,plastic capacity and the ovality of the pipe

pp=2*(t/d)*s*αtab

δ0=(Dmax-Dmin)/(Dmax+Dmin)

##x=(pc-pel)(pc**2-pp**2)
##y= float(pc)*float(pel)*pp*2*float(δ0)*(d/float(t))
##
##if x==y:
##    print (true)
##else:
##    print (flase)

# Collapse Due to Pure Bending

ϵb=t/(2*d)

print (ϵb,"ϵb=Collapse Due to Pure Bending" )

   
