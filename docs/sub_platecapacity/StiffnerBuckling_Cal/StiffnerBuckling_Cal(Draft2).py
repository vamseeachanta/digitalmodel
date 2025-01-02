'''
Author: Sai Venkatesh
Date Updated: 2018-06-27
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

Z_GGt=z_GGt

if(x1=="L"):      
    print("Ref")
else:
    print(z_GGt)




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

#------------  Stiffener - Euler Buckling Strength (Theoretical)# 
fE_G=(3.14**2)*E_G*Ksi_G*(Ie_G/Ik1)**2

fE_Gpx=(3.62)*E_G*Ksi_G*(t_G/s_G)**2

fE_Gpy=(0.9)*E_G*Ksi_G*(t_G/s_G)**2

fE_Gpt=(5)*E_G*Ksi_G*(t_G/s_G)**2

#------------  Stiffener - Euler Buckling Strength (Theoretical)#                     

σj_Gsd=sqrt(σx_Gsd**2+σy_Gsd**2-σx_Gsd*σy_Gsd+3*τ_Gsd**2)

c129_G=2-(s_G/L_G)

λe_G=fy_G*Ksi_G/σj_Gsd*((σx_Gsd/fE_Gpx)**c129_G+(σy_Gsd/fE_Gpy)**c129_G+(τ_Gsd/fE_Gpt)**c129_G)**(1/c129_G)

fe_Gp=fy_G*Ksi_G/(sqrt(1+λe_G**2))

x3=σj_Gsd/fe_Gp
x4=1

if(x3<1):
    print(x3)
else:
    print(x4)
    
C133_G=HW_G/s_G*(t_G/TW_G)**3*sqrt(1-x3)

Beta_G=(3*C133_G+0.2)/(C133_G+0.2)

x5=5.34*(L_G/LG_G)+4

x6=5.34+4*(L_G/LG_G)**2  #136

x7=5.34*(s_G/L_G)+4

x8=5.34+4*(s_G/L_G)**2  #137

if(L_G>L_G):
    print(x5)
else:
    print(x6)
    
if(L_G<s_G):
    print(x7)
else:
    print(x8)


fr_G=fy_G*Ksi_G

fr_GG=fy_G*Ksi_G

λ_Gbar1=sqrt(fr_G/fE_G)

λ_Gbar2=sqrt(fr_GG/fE_G)



                            # Girder - Parameters & Factors
                            
σj_GGsd=sqrt(σx_Gsd**2+σy_Gsd**2-σx_Gsd*σy_Gsd+3*τ_Gsd**2)

c142_G=2-(s_G/LG_G)

λe_GG=fy_G*Ksi_G/σj_Gsd*((σx_Gsd/fE_Gpx)**c142_G+(σy_Gsd/fE_Gpy)**σj_GGsd+(τ_Gsd/fE_Gpt)**σj_GGsd)**(1/σj_GGsd)

fe_GGp=fy_G*Ksi_G/sqrt(1+λe_GG**2)

x9=σj_GGsd/fe_GGp #c145

C146_G=hwG_G/s_G*(t_G/tw_G)**3*sqrt(1-x9)

Beta_GG=(3*C146_G+0.2)/(C146_G+0.2)


fr_GG=fy_G*Ksi_G

c148_GG=sqrt(fr_GG/fE_G)

#C149_GG=sqrt(fr1_GG/fE_G)





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


                #   Stiffener - Torsional Buckling Strength
      
fE_GT=(A_Gw+(TF_G/TW_G)**2*A_Gf)/(A_Gw+3*A_Gf)*Beta_G*(G_G*Ksi_G*(TW_G/HW_G)**2)+3.14**2*E_G*Ksi_G*Ie_Gx/((A_Gw/3+A_Gf)*Lt_G**2)

λT_G=sqrt(fy_G*Ksi_G/fE_GT) #165

μ_G=0.35*(λT_G-0.6) #166

ft1_G=(1+μ_G+λT_G**2-sqrt(((1+μ_G+λT_G**2)**2-4*λT_G**2)))/((2*λT_G**2))*fy_G*Ksi_G

ft2_G=fy_G*Ksi_G

ft3_G=ft1_G

if(λT_G>0.6):
    print(ft1_G)
else:
    print(ft2_G)


            # Girder - Torsional Buckling Strength
                                                                                            
fE_GGT=(A1_GG+(TF_G/tw_G)**2*Ax_Gf)/(A1_GG+3*A1_GG)*Beta_GG*(G_G*Ksi_G*(tw_G/hwG_G)**2)+3.14**2*E_G*Ksi_G*Ie_GGx/((A1_GG/3+Ax_Gf)*Lt_G**2)

λT_GG=sqrt(fy_G*Ksi_G/fE_GGT) #173

μ_GG=0.35*(λT_GG-0.6)

ft1_GG=(1+μ_GG+λT_GG**2-sqrt(((1+μ_GG+λT_GG**2)**2-4*λT_GG**2)))/((2*λT_GG**2))*fy_G*Ksi_G

ft2_GG=fy_G*Ksi_G

ft3_GG=ft1_GG

if(λT_GG>0.6):
    print(ft1_GG)
else:
    print(ft2_GG)


            # Plate Side - Stiffener Buckling Resistance

fr_G=fy_G*Ksi_G  # g180

μgp_G=(0.34+0.08*Z_Gp/Ie_G)*(λ_Gbar1-0.2)

fk1_G=fr_G

fk2_G=(1+μgp_G+λ_Gbar1**2-sqrt((1+μgp_G+λ_Gbar1**2)**2-4*λ_Gbar1**2))/(2*λ_Gbar1**2)*fr_G

fk3_G=fk1_G

if(λ_Gbar1>0.2):
    print(fk2_G)
else:
    print(fk1_G)

         # Plate Side - Girder Buckling Resistance

fr_GG=fy_G*Ksi_G  #g187

μgp_GG=(0.34+0.08*Z_Gp/Ie_G)*(λ_Gbar2-0.2)

fk1_GG=fr_GG

fk2_GG=(1+μgp_GG+λ_Gbar2**2-sqrt((1+μgp_GG+λ_Gbar2**2)**2-4*λ_Gbar2**2))/(2*λ_Gbar2**2)*fr_GG

fk3_GG=fk1_GG

if(λ_Gbar2>0.2):
    print(fk2_GG)
else:
    print(fk1_GG)

        # Stiffener Side - Stiffener Buckling Resistance

fr1_G=ft3_G #g194

μ1_G=(0.34+0.08*Z_Gt/Ie_G)*(λ_Gbar1-0.2)

f1k_G=fy_G*Ksi_G

f2k_G=(1+μ1_G+λ_Gbar2**2-sqrt((1+μ1_G+λ_Gbar2**2)**2-4*λ_Gbar2**2))/(2*λ_Gbar2**2)*fr1_G

f3k_G=f1k_G

if(λ_Gbar2>0.2):
    print(f2k_G)
else:
    print(f1k_G)



                  #Stiffener Side - Girder Buckling Resistance

fr1_GG=ft3_GG #g201

C149_GG=sqrt(fr1_GG/fE_G)

μ1_GG=(0.34+0.08*Z_Gt/Ie_G)*(λ_Gbar1-0.2)

f1k_GG=fy_G*Ksi_G

f2k_GG=(1+μ1_GG+C149_GG**2-sqrt((1+μ1_GG+C149_GG**2)**2-4*C149_GG**2))/(2*C149_GG**2)*fr1_GG

f3k_GG=f2k_GG

if(λ_Gbar2>0.2):
    print(f1k_GG)
else:
    print(f2k_GG)


                  # Girder Force Calculation

FeG_G=(3.14**2)*E_G*Ksi_G*(Ie_GG/LG_G)**2

τcel_G=18*E_G*Ksi_G/(t_G*L_G**2)*(t_G*EIe_GGx/s_G)**0.75

τceg_G=τcel_G*L_G**2/LP_G**2

λτ_G=sqrt(0.6*fy_G*Ksi_G/τcel_G)

τCrg1_G=0.6*fy_G*Ksi_G

τCrg2_G=(0.6/λτ_G**2)*fy_G*Ksi_G

if(λτ_G<1):
    print(τCrg1_G)
else:
    print(τCrg2_G)

λ1G_G=sqrt(fy_G*Ksi_G/FeG_G)

Q_G=0

if(λ1G_G-0.2<0):
    print(Q_G)

##C1_G=
##
##C2_G=
##
C3_G=0

                   # Stiffener Side - Stiffener Shear Resistance
s3=sqrt(3)

TRdy_G=(fy_G*Ksi_G)/ym_G/s3

Tcdl_G=x8*904*E_G*Ksi_G*(t_G/s_G)**2

TRdl_G=Tcdl_G/ym_G

Tcrs_G=36*E_G*Ksi_G/s_G/t_G/(L_G**2)*(M_GIPY*M_Gx**3)**0.25

TRds_G=Tcrs_G/ym_G

TRd=min(TRds_G,TRdl_G,TRdy_G)

               # Stiffener Resistance

NRd_G=((A_Ge+(Lt_G*t_G))*fy_G)*Ksi_G/ym_G

NRd1_G=(A_Ge+Lt_G*t_G)*(fk3_G/ym_G)

NRd2_G=(A_Ge+Lt_G*t_G)*(f3k_G/ym_G)

WeP_G=(EIe_Gx/Z_Gp)

Wes_G=(EIe_Gx/Z_Gt)

MpRd1_G=WeP_G*fy_G*Ksi_G/ym_G*0.000000001*1000000

Ms1Rd_G=Wes_G*fr1_G/ym_G*0.000000001*1000000

Ms2Rd_G=Wes_G*fr1_G/ym_G*0.000000001*1000000

MstRd_G=Wes_G*fy_G*Ksi_G/ym_G*0.000000001*1000000

NE_G=(3.14**2)*E_G*Ksi_G*A_Ge/(Ik1/Ie_G)**2


           #Stiffener Loading 

NSd_G=σxsd*(A_Ge+s_G*B_G)+τsd*s_G*B_G

M1sd_G=Qsd_G*((L_G/1000)**2)/12

M2sd_G=Qsd_G*((L_G/1000)**2)/24

U1_G=(τsd/TRd)**2

   # Stiffener Combined Loading Utilization, Lateral Pressure on Plate side

       #G242/G232+(G243-G242*G47)/(G236*(1-G242/G239))+G245
   
UT1_G=NSd_G/NRd2_G+(M1sd_G-NSd_G*Z1_G)/(Ms1Rd_G*(1-M1sd_G/NE_G))+U1_G

UT2_G=NSd_G/NRd1_G-2*NSd_G/NRd_G+(M1sd_G-NSd_G*Z1_G)/(MpRd1_G*(1-NSd_G/NE_G))+U1_G

UT3_G=NSd_G/NRd1_G-2*NSd_G/NRd_G+(M1sd_G-NSd_G*Z1_G)/(MpRd1_G*(1-NSd_G/NE_G))+U1_G

UT4_G=NSd_G/NRd2_G+(M1sd_G-NSd_G*Z1_G)/(Ms1Rd_G*(1-M1sd_G/NE_G))+U1_G

UMT1_G=max(UT1_G,UT4_G)


# Stiffener Combined Loading Utilization, Lateral Pressure on Stiffener side



U1T_G=NSd_G/NRd1_G-2*NSd_G/NRd_G+(M1sd_G-NSd_G*Z1_G)/(MpRd1_G*(1-NSd_G/NE_G))+U1_G

U2T_G=NSd_G/NRd2_G+(M1sd_G-NSd_G*Z1_G)/(Ms1Rd_G*(1-M1sd_G/NE_G))+U1_G

U3T_G=NSd_G/NRd2_G+(M1sd_G-NSd_G*Z1_G)/(Ms1Rd_G*(1-M1sd_G/NE_G))+U1_G

U4T_G=NSd_G/NRd1_G-2*NSd_G/NRd_G+(M1sd_G-NSd_G*Z1_G)/(MpRd1_G*(1-NSd_G/NE_G))+U1_G

UMT2_G=max(U1T_G,U4T_G)


# Girder Resistance


NRd_GG=((A1_GG+(L_G*t_G))*fy_G)*Ksi_G/ym_G

NRd1_GG=(A1_GG+L_G*t_G)*(fk3_GG/ym_G)  #262

NRd2_GG=(A1_GG+L_G*t_G)*(f3k_GG/ym_G)

WeP_GG=EIe_GGx/z_GGp

Wes_GG=(EIe_GGx/Z_GGt)

MpRd1_GG=WeP_GG*fy_G*Ksi_G/ym_G*0.000000001*1000000

Ms1Rd_GG=Wes_GG*fr1_GG/ym_G*0.000000001*1000000

Ms2Rd_GG=Wes_GG*fr1_GG/ym_G*0.000000001*1000000

MstRd_GG=Wes_GG*fy_G*Ksi_G/ym_G*0.000000001*1000000

NE_GG=(3.14**2)*E_G*Ksi_G*Ax_Ge/(Ik2/Ie_GG)**2


# Girder Loading 

Psd_GG=0.02*σysd*(Ax_Gf+A1_GS/3)

Psd1_GG=Psd_GG/TF_G/Lt_G

pox_GG=0.4*(t_G+A1_GS/s_G)/(1-s_G/LG_G)*(fy_G/E_G)*(LG_G/L_G)**2*(σxsd+C3_G*τsd)

poy_GG=0.4*(t_G+A1_GS/s_G)/(hwG_G*(1-s_G/LG_G))*(fy_G/E_G)*(LG_G/L_G)**2*(C3_G*τsd)

G36="yes"

if(G36=="YES"):
    Print(pox_GG)
else:
    print(poy_GG)

qsd_GG=(Psd1_GG+pox_GG)*L_G

Z_GG=LG_G/2

NYSD_GG=σysd*(L_G*t_G+A1_GG)  #280

M1SD_GG=qsd_GG*((LG_G/1000)**2)/12

M2SD_GG=qsd_GG*((LG_G/1000)**2)/24

U12_GG=(τsd/Psd_GG)**2

# Girder Combined Loading Utilization, Lateral Pressure on Plate side
       
UT11_G=NYSD_GG/NRd2_GG+(M1SD_GG-NYSD_GG*Z_GG/1000)/(Ms1Rd_GG*(1-NYSD_GG/NE_GG))+U12_GG

UT21_G=NYSD_GG/NRd1_GG-2*NYSD_GG/NRd_GG+(M1SD_GG-NYSD_GG*Z_GG/1000)/(MpRd1_GG*(1-NYSD_GG/NE_GG))+U12_GG

UT31_G=NYSD_GG/NRd2_GG-2*NYSD_GG/NRd_GG+(M2SD_GG+NYSD_GG*Z_GG/1000)/(MstRd_GG*(1-NYSD_GG/NE_GG))+U12_GG

UT41_G=NYSD_GG/NRd2_GG+(M2SD_GG+NYSD_GG*Z_GG/1000)/(MpRd1_GG*(1-NYSD_GG/NE_GG))+U12_GG

UMT11_G=NYSD_GG/NRd2_GG+(M2SD_GG+NYSD_GG*Z_GG/1000)/(MpRd1_GG*(1-NYSD_GG/NE_GG))+U12_GG

# Girder Combined Loading Utilization, Lateral Pressure on Stiffener side

UT11_G=NYSD_GG/NRd2_GG-2*NYSD_GG/NRd_GG+(M2SD_GG+NYSD_GG*Z_GG/1000)/(MstRd_GG*(1-NYSD_GG/NE_GG))+U12_GG

UT11_G=NYSD_GG/NRd2_GG+(M2SD_GG+NYSD_GG*Z_GG/1000)/(MpRd1_GG*(1-NYSD_GG/NE_GG))+U12_GG

UT11_G=NYSD_GG/NRd2_GG+(M1SD_GG-NYSD_GG*Z_GG/1000)/(Ms1Rd_GG*(1-NYSD_GG/NE_GG))+U12_GG

UT11_G=NYSD_GG/NRd1_GG-2*NYSD_GG/NRd_GG+(M1SD_GG-NYSD_GG*Z_GG/1000)/(MpRd1_GG*(1-NYSD_GG/NE_GG))+U12_GG

UTM1_G=max(UT11_G,UT11_G)














































































