'''
Author: Sai Venkatesh
Date Updated: 2018-05-23
Objective: Plate Buckling as per DNV ??
Outputs: yes
Running Program: Yes
python plateBuckling.py 
'''

from DataProvision.parameters_Buckling212_All import *
from math import sqrt


##E=30000000
##v=0.3
##k_1=0.425 #(1.277,4.00,5.42,6.97)
##t_p=0.552
##b_p=27.6
##l1_p=104.04
##lam_2=1
##E_t=25000000
##sig_y=33000
##l2_p=1
##k_3=4
##l3_p=0.8
##k_5=0.425
##x2=40.0
##sig_1=33000
##sig_2=12280



# Buckling of plates in edge compression.

sig_cr1=(k_1*3.14159**2*E/12*(1-v**2))*(t_p/b_p)**2

# If crictical stress is below the proportional limit if(sig_cr>p),Then

lam_1=E_t/E

sig_cr2=(3.14159**2*E*lam_1/12*(1-v**2))*((t_p/b_p)**2)*k_1

sig_cr3=(3.14159**2*E*sqrt(lam_1)/12*(1-v**2))*((t_p/b_p)**2)*k_1 # For better result

sig_cr4=(2.710*10**7*sqrt(lam_1))*((t_p/b_p)**2)*k_1              # For steel

# If crictical stress is above the proportional limit if(sig_cr<p),then

lam_2=E_t/E

sig_cr5=(2.710*10**7*sqrt(lam_2))*((t_p/b_p)**2)*k_1 # caanot be used directly

sig_cr_5=sig_cr5/sqrt(lam_2)                           # Multiply by sqrt(lam_2),then

sig1_cr_5=(2.710*10**7)*((t_p/b_p)**2)*k_1         # from this,we get

sig_cr6=(sig_y*(sig_cr5/sqrt(lam_2))**2)/(0.1836*sig_y**2+(sig_cr5/sqrt(lam_2))**2)

# Buckling of plates under shear

k_2=5.34+(4/sqrt(l1_p))    # from Table2         #length of plate

t_cr1=(k_2*3.14159**2*E/12*(1-v**2))*(t_p/b_p)**2

sig_cr7=(sqrt(3)*k_2*3.14159**2*E/12*(1-v**2))*(t_p/b_p)**2

t_cr2=sig_cr7/sqrt(3)        # Usually considered   # From Table 2

k_4=(l3_p+(1/l3_p))**2 

 # Considering table 3

sig_cr8=(sqrt(3)*k_3*3.14159**2*E/12*(1-v**2))*(t_p/b_p)**2

sig_cr9=(sqrt(3)*k_4*3.14159**2*E/12*(1-v**2))*(t_p/b_p)**2

if(l2_p>=1):
    print("the value of the critical stress",sig_cr8)

if(l3_p<=1):
    print("the value of the critical stress",sig_cr9)

# Buckling stress curves

n=sqrt(sig_y**2)/4770
                 
sig_cr10=1.8*sig_y-n*((b_p/t_p)/sqrt(k_5))

x1=((b_p/t_p)/sqrt(k_5))                 

sig_cr11=(4434/x1)**2

#The compressive load at stage

sig_cr12=(4434/x2)**2  # x2=((b_p/t_p)/sqrt(k_5))
    
a_1=20*0.25

p_1=a_1*sig_cr12

# The total compressive load at this atage of loading

a_2=5*0.25

a_3=4.5*0.25

P_2=a_2*sig_1+a_3*sig_2

p_3=a_2*sig_1
