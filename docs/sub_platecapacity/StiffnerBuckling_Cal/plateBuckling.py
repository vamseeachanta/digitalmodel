'''
Author: Sai Venkatesh
Date Updated: 2018-05-15
Objective: Stiffner Buckling Calculation
Outputs: yes
Running Program: Yes
python plateBuckling.py 
'''

from DataProvision.parameters_stiffnerbuckling import *
from math import sqrt
import matplotlib.pyplot as plt

Ik1=L_G*1 # Assuming no Lateral Load
Ik2=LG_G

# FEA Panel Response

σx_Gsd=2.28*(Ksi_G) #Mpa

σy_Gsd=7.04*(Ksi_G)

τ_Gsd=0.138*(Ksi_G)

# Loads in Addition to FEA

q_Gsd=0

Z_G=0

# Stiffener - T Section

c_G=(B_G/2)-(TF_G/2) #50

YG_COG_plate=(t_G/2) #51

YG_COG_Web=t_G+(HW_G/2) #52

Y_COG_Flange=t_G+HW_G+(TF_G/2)  #53

A_Gp=t_G*s_G   #54

A_Gw=TW_G*HW_G  #55

A_Gf=TF_G*B_G   #56

A_Ge=A_Gp+A_Gw+A_Gf #57

X_GCOG=0   #58

Y_GA=(A_Gp*YG_COG_plate+A_Gw*YG_COG_Web+A_Gf*Y_COG_Flange)/A_Ge #59

eY_GB=(A_Gw*YG_COG_Web+A_Gf*Y_COG_Flange)/(A_Gw+A_Gf) #60

ef_G=0 #61

Z_Gp=Y_GA-(t_G/2)

Z_Gt=HW_G+TF_G-(Z_Gp-t_G/2)

EIe_Gx=(TW_G*HW_G**3/12)+(B_G*TF_G**3/12)+(s_G*t_G**3/12)+A_Gp*(YG_COG_plate-Y_GA)**2+A_Gw*(YG_COG_Web-Y_GA)**2+A_Gf*(Y_COG_Flange-Z_Gp)**2

Ie_Gx=1/12*A_Gf*B_G**2+ef_G*(A_Gf/(1+A_Gf/A_Gw))

Ie_G=sqrt(EIe_Gx/A_Ge)

M_Gx=EIe_Gx


# Girder - T Section

Cx_G=b_G/2-tf_G/2         #70

YGx_COG_plate=t_G/2       #71
 
YGx_COG_Web=t_G+hwG_G/2   #72

Yx_COG_Flange=t_G+hwG_G+(tf_G)/2  #73

Ax_Gp=t_G*s_G                     #74

Ax_Gw=hwG_G*tw_G                  #75

Ax_Gf=tf_G*b_G                    #76

Ax_Gs=Ax_Gw+Ax_Gf                 #77      

Ax_Ge=Ax_Gp+Ax_Gw+Ax_Gf           #78

Xx_GCOG=0                         #79

# (G75*G71+G75*G72+G76*G73)/G78
Y_GGA=(Ax_Gw*YGx_COG_plate+Ax_Gw*YGx_COG_Web+Ax_Gf*Yx_COG_Flange)/Ax_Ge

eY_GGB=(Ax_Gw*YGx_COG_Web+Ax_Gf*Yx_COG_Flange)/(Ax_Gw+Ax_Gf)

ef_GG=0

z_GGp=Y_GGA-t_G/2

z_GGt=hwG_G+tf_G-(z_GGp-t_G/2)

EIe_GGx=(tw_G*hwG_G**3/12)+(tf_G*b_G**3/12)+(t_G*s_G**3/12)+Ax_Gp*(YGx_COG_plate-Y_GGA)**2+Ax_Gw*(YGx_COG_Web-Y_GGA)**2+Ax_Gf*(Yx_COG_Flange-Y_GGA)**2

Ie_GGx=1/12*A_Gf*b_G**2+ef_GG*(A_Gf/(1+A_Gf/Ax_Gw))

Ie_GG=sqrt(EIe_GGx/Ax_Ge)

M_GGx=EIe_GGx

# Stiffener - Structural Properties

x1="T"  #g19
x2="L"
if(x1==x2):
    print("Ref")
else:
    print(c_G)

if(x1==x2):
    print("Ref")
else:
    print(X_GCOG)

if(x1==x2):
    print("Ref")
else:
    print(Y_GA)

if(x1==x2):
    print("Ref")
else:
    print(eY_GB)  

if(x1==x2):
    print("Ref")
else:
    print(ef_G,Z_Gp,Z_Gt,A_Gw,A_Gf,A_Ge,EIe_Gx,Ie_Gx,Ie_G,M_Gx)

M_GIPY=(t_G**3*s_G)/10.9

# Girder - Structural Properties 


x1="T"  #g19
x2="L"

A1_GG=Ax_Gs    #Area of grider #g117

A1_GS=Ax_Gs


if(x1==x2):
    print("Ref")
else:
    print(Cx_G)

if(x1==x2):
    print("Ref")
else:
    print(Xx_GCOG,Y_GGA,eY_GGB,ef_GG,z_GGp,z_GGt,A1_GG,A1_GS,Ax_Gf,Ax_Ge,EIe_GGx,Ie_GGx,Ie_GG,M_GGx)

M_GGIPY=(t_G**3*s_G)/10.9



                         # Stiffener - Parameters & Factors

σj_Gsd=sqrt(σx_Gsd**2+σy_Gsd**2-σx_Gsd*τ_Gsd+3*τ_Gsd**2)

c129_G=2-(s_G/L_G)

##λe_G=fy_G*Ksi_G/σj_Gsd*((σx_Gsd/)**+(σy_Gsd/)**+(τ_Gsd/)**)**(1/)

##fe_Gp=fy_G*Ksi_G/sqrt(1+λe_G**2)
##
##
##x3=σj_Gsd/fe_Gp
##
##x4=1

##if(x3<1):
##    print(x3)
##else:
##    print(x4)

##C133=HW_G/s_G*(t_G/TW_G)**3*sqrt(1-x3)

##Beta_G=(3*C133+0.2)/(C133+0.2)


##λ_bar1=sqrt(/)
##
##λ_bar2=sqrt(/)



                            # Girder - Parameters & Factors


## fe_GGp=fy_G*Ksi_G/sqrt(1+λe**2)
##
## x5=σj_Gsd/fe_Gp
##
## x6=1

##if(x5<1):
##    print(x5)
##else:
##    print(x6)

##C146=hwG_G/s_G*(t_G/tw_G)**3*sqrt(1-x5)

##Beta_GG=(3*C146+0.2)/(C146+0.2)
##
##λ_bar1=sqrt(/)
##
##λ_bar2=sqrt(/)


                   # Stiffener - Euler Buckling Strength (Theoretical)

fE_G=(3.14**2)*E_G*Ksi_G*(Ie_G/Ik1)**2

fE_Gpx=(3.62)*E_G*Ksi_G*(t_G/s_G)**2

fE_Gpy=(0.9)*E_G*Ksi_G*(t_G/s_G)**2

fE_Gpt=(5)*E_G*Ksi_G*(t_G/s_G)**2


                  #Girder - Euler Buckling Strength (Theoretical)

fE_GG=(3.14**2)*E_G*Ksi_G*(Ie_GG/Ik2)**2

fE_GGpx=(3.62)*E_G*Ksi_G*(t_G/s_G)**2

fE_GGpy=(0.9)*E_G*Ksi_G*(t_G/s_G)**2

fE_GGpt=(5)*E_G*Ksi_G*(t_G/s_G)**2



                        #Plate Side - Stiffener Buckling Resistance
