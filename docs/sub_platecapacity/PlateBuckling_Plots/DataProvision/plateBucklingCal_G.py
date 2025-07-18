'''
Author: Sai Venkatesh
Date Updated: 2018-04-16
Objective: Plate Buckling as per DNV ??
Outputs: yes
Running Program: Yes
python plateBuckling.py 
'''

from DataProvision.parameters_Col_All import *
from math import sqrt
# How to access objects from above dictionaries (also same for JSON format files)


# σG_xx,σG_yy,τ_G
x1=s_G/l_G
x2=l_G/s_G
c=(2-x1)
x3=t_G/s_G
x4=s_G/t_G
x5=l_G/t_G



# FEA Analysis Stress (No Reduction Factor is used in Spreadsheet)
σ_e1=sqrt(σG_xx**2+σG_yy**2-(σG_yy*σG_xx)+(3*τ_G**2))  # Vonmises Stress (σe)

# Characteristic Material Resistance, σk
σ_kx=f_I
σ_ky=f_I
τ_k=f_I/sqrt(3)
σ_e=f_I


# Edges Simply supported - Uniform Loading

c_yy=(1+x1**2)**2
c_τ=(5.34+4*x1**2)


# Elastic Buckling Resistance for each stress direction
x6=3.14159**2*E_I/12/(1-p_I**2) # PI()^2*G38/12/(1-G37^2)
σExx_Simp=x6*c_xx*x3**2
σEyy_Simp=x6*c_yy*x3**2
τE_simp=x6*c_τ*x3**2


# Reduced Slenders ratio # σG_xx,σG_yy,τ_G 
λx_simp=round(sqrt(σ_kx/σExx_Simp),2)
λy_simp=sqrt(σ_ky/σEyy_Simp)
λτ_simp=sqrt(τ_k/τE_simp)
λe_simp=sqrt(f_I/σ_e1*((σG_xx/σExx_Simp)**c+(σG_yy/σEyy_Simp)**c+(τ_G/τE_simp)**c)**(1/c))


# Characteristic Buckling Resistance for serviceability     21
σscrxG_simp=σ_kx/sqrt(1+λx_simp**4)
σscryG_simp=σ_ky/sqrt(1+λy_simp**4)
σscrzG_simp=τ_k/sqrt(1+λτ_simp**4)
σescrG_simp=f_I/sqrt(1+λe_simp**4)


# Usage factor for serviceability check, Simply Supported    1
ηsxG_simp=σG_xx/σscrxG_simp
FALSEG=σG_yy/σscryG_simp
ηszG_simp=τ_G/σscrzG_simp
ηseG_simp=σ_e1/σescrG_simp


# Characteristic Buckling Resistance for Ultimate check   22
σucrxG_simp1=(σ_kx/(sqrt(1+λx_simp**4)))
σucrx_simp2=σ_kx/sqrt(2)/λx_simp
if(λx_simp<1):
    print("The value of σucrx_simp1 is ",σucrxG_simp1)
else:
    print("The value of σucrx_simp2 is",σucrx_simp2)
σucry_simp1=(σ_ky/(sqrt(1+λy_simp**4)))
σucryG_simp2=σ_ky/sqrt(2)/λy_simp
if(λy_simp<1):
    print("The value of σucry_simp1 is ",σucry_simp1)
else:
    print("The value of σucry_simp2 is",σucryG_simp2)
σucrzG_simp1=(τ_k/(sqrt(1+λτ_simp**4)))
σucrz_simp2=τ_k/sqrt(2)/λτ_simp
if(λτ_simp<1):
    print("The value of σucrz_simp1 is ",σucrzG_simp1)
else:
    print("The value of σucrz_simp2 is",σucrz_simp2)
σeucr_simp1=(σ_e/(sqrt(1+λe_simp**4)))
σeucrG_simp2=σ_e/sqrt(2)/λe_simp
if(λe_simp<1):
    print("The value of σeucr_simp1 is ",σeucr_simp1)
else:
    print("The value of σeucr_simp2 is",σeucrG_simp2)

# Usage factor for ultimate check, , Simply Supported   2
ηuxG_simp=σG_xx/σucrxG_simp1
ηuyG_simp=σG_yy/σucryG_simp2
ηuzG_simp=τ_G/σucrzG_simp1
ηueG_simp=σ_e1/σeucrG_simp2


# Sides clamped - Uniform Loading

cyy=(1+2.5*x1**2+5*x1**4)
cτ=(9+5.6*x1**2)


# Elastic Buckling Resistance for each stress direction
σExx_Simp=x6*cxx*x3**2
σEyy_Simp=x6*cyy*x3**2
τE_Simp=x6*cτ*x3**2


# Reduced Slenders ratio
λx_side=sqrt(σ_kx/σExx_Simp)
λy_side=sqrt(σ_ky/σEyy_Simp)
λτ_side=sqrt(τ_k/τE_Simp)
λe_side=sqrt(f_I/σ_e1*((σG_xx/σExx_Simp)**c+(σG_yy/σEyy_Simp)**c+(τ_G/τE_Simp)**c)**(1/c))


# Characteristic Buckling Resistance for serviceability    23
σscrxG_side=σ_kx/sqrt(1+λx_side**4)
σscryG_side=σ_ky/sqrt(1+λy_side**4)
σscrzG_side=τ_k/sqrt(1+λτ_side**4)
σescrG_side=f_I/sqrt(1+λe_side**4)


# Usage factor for serviceability check, Sides Clamped
ηsxG_side=σG_xx/σscrxG_side
ηsyG_side=σG_yy/σscryG_side
ηszG_side=τ_G/σscrzG_side
ηseG_side=σ_e1/σescrG_side


# Characteristic Buckling Resistance for Ultimate Check   24
σucrxG_side1=σ_kx/(sqrt(1+λx_side**4))
σucrx_side2=σ_kx/sqrt(2)/λx_side
if(λx_side<1):
    print("The value of σucrx_side1 is ",σucrxG_side1)
else:
    print("The value of σucrx_side2 is",σucrx_side2)
σucry_side1=σ_ky/(sqrt(1+λy_side**4))
σucryG_side2=σ_ky/sqrt(2)/λy_side
if(λy_side<1):
    print("The value of σucry_side1 is ",σucry_side1)
else:
    print("The value of σucry_side2 is",σucryG_side2)
σucrzG_side1=τ_k/(sqrt(1+λτ_side**4))
σucrz_side2=τ_k/sqrt(2)/λτ_side
if(λτ_side<1):
    print("The value of σucrz_side1 is ",σucrzG_side1)
else:
    print("The value of σucrz_side2 is",σucrz_side2)
σeucr_side1=σ_e/(sqrt(1+λe_side**4))
σeucrG_side2=σ_e/sqrt(2)/λe_side
if(λe_side<1):
    print("The value of σeucr_side1 is",σeucr_side1)
else:
    print("The value of σeucr_side2 is",σeucrG_side2)


# Usage factor for ultimate check, Sides Clamped
ηuxG_side=σG_xx/σucrxG_side1
ηuyG_side=σG_yy/σucryG_side2
ηuzG_side=τ_G/σucrzG_side1
ηueG_side=σ_e1/σeucrG_side2


# Buckling resistance stress in longitudinal direction
λ_p=0.525*x4*sqrt(f_I/E_I)
Cx=(λ_p-0.22)/λ_p**2
if(λ_p>0.673):
    print("The value for slendrness grater than equal to (0.673)",Cx)
else:
    print("The value is",1)

σxrd=Cx*f_I/ϒ_M


# Buckling resistance stress in Transverse direction

λ_c=1.1*x4*sqrt(f_I/E_I)
µ=0.21*(λ_c-0.2)
k1=1 # if(l_c<=0.2): print("the value of k",k) 
k2=1/(2*λ_c**2)*((1+µ+λ_c**2)-sqrt((1+µ+λ_c**2)**2-4*λ_c**2))
k3=1/(2*λ_c**2)+0.07
p_Sd_pa=101325+1025*D_G*x9
p_Sd_ksi=p_Sd_pa*x10*x11
x12= 2*(x3**2)*f_I       #x7=2*(t_G/s_G)^2*f_y
#IF(0.05*G43-0.75<0,0,0.05*G43-0.75)
h_α1=0.05*x4-0.75
h_α2=0.05*x4-0.75
if(h_α1<0):
    print(" The value of h_α1 is",0)
else:
    print(" The value of h_α is",h_α2)
Kp1=1
Kp2=1-h_α2*((p_Sd_ksi/f_I)-2*x3**2)
if(p_Sd_ksi<=p_Sd_pa):
    print(" The value of Kp is",Kp1)
else:
    print(" The value of Kp is",Kp2)

σy_R=(1.3*t_G/l_G*sqrt(E_I/f_I)+k4_G*(1-1.3*t_G/l_G*sqrt(E_I/f_I)))*f_I*Kp1
σy_rd=σy_R/ϒ_M


# Buckling resistance stress in Shear direction
kl_1=5.34+4*(x1)**2
kl_2=5.34*x1**2+4
if(x1<1):
    print("The value of kl_1 is",kl_1)               # Please,kindly check this condition whether it is satifying this coditions.
else:
    print("The value of kl_2  is",kl_2)

λ_w=0.795*x4*sqrt(f_I/(E_I*kl_1))



if(λ_w>1.2):
    print(0.9/λ_w)                                   # Please,kindly check this condition whether it is satifying this coditions.
if(λ_w>0.8):
    print(1-0.625*(λ_w-0.8))
else:
    print("The value of C_τ is",C_τ)



τ_rd=C_τ/ϒ_M*f_I/sqrt(3)



# Buckling resistance stress in Bi-axial with Shear direction


ci_2=(1-s_G/(120*t_G))
if(x4>120):
    print("The value of ci_1",ci_1)
else:
    print("The value of ci_2",ci_2)

k_l=kl_1
λ_w=λ_w


C_τe1=(1-0.8*(λ_w-0.8))

if(λ_w>1.25):
    print(1/λ_w**2)                                 # Please,kindly check this condition whether it is satifying this coditions.
if(λ_w>0.8):
    print("The value of C_τe1 is",C_τe1)
else:
    print("The value of C_τe2 is",C_τe2)


      
τrd=C_τe2/ϒ_M*f_I/sqrt(3)


σG_xrd=σxrd   #25
σG_yrd=σy_rd
τ_rdG=τrd
x15=(σG_xx/σG_xrd)**2+(σG_yy/σG_yrd)**2-ci_2*(σG_yy/σG_xrd)*(σG_yy/σG_yrd)+(τ_G/τ_rdG)**2
     



# DNV-RP-C201 Usage factor

LongitudinalG=σG_xx/σxrd
TransverseG=σG_yy/σy_rd
ShearG=τ_G/τ_rd
BiaxialG=sqrt(x15)














    


    











