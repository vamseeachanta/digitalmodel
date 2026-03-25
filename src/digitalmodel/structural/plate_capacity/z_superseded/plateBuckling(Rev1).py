'''
Author: Sai Venkatesh
Date Updated: 2018-04-11
Objective: Plate Buckling as per DNV ??
Outputs: ??
Running Program:
python plateBuckling.py 
'''

from DataProvision.parameters import *
from math import sqrt

# How to access objects from above dictionaries (also same for JSON format files)
print(plateLoading["ShearStress"])

# σ_xx,σ_yy,τ
x1=s/l
x2=l/s
c=(2-x1)
x3=t/s
x4=s/t
x5=l/t

# FEA Analysis Stress (No Reduction Factor is used in Spreadsheet)
σ_e1=sqrt(σ_xx**2+σ_yy**2-(σ_yy*σ_xx)+(3*τ**2))  # Vonmises Stress (σe)

# Characteristic Material Resistance, σk
σ_kx=f
σ_ky=f
τ_k=f/sqrt(3)
σ_e=f
print(σ_kx,σ_kx,τ_k ,σ_e)

# Edges Simply supported - Uniform Loading
c_xx=4
c_yy=(1+x1**2)**2
c_τ=(5.34+4*x1**2)
print(c_xx,c_yy,c_τ)

# Elastic Buckling Resistance for each stress direction
x6=3.14159**2*E/12/(1-p**2) # PI()^2*G38/12/(1-G37^2)
σExx_Simp=x6*c_xx*x3**2
σEyy_Simp=x6*c_yy*x3**2
τE_simp=x6*c_τ*x3**2
print(x6,σExx_Simp,σEyy_Simp,τE_simp)

# Reduced Slenders ratio # σ_xx,σ_yy,τ 
λx_simp=round(sqrt(σ_kx/σExx_Simp),2)
λy_simp=sqrt(σ_ky/σEyy_Simp)
λτ_simp=sqrt(τ_k/τE_simp)
λe_simp=sqrt(f/σ_e1*((σ_xx/σExx_Simp)**c+(σ_yy/σEyy_Simp)**c+(τ/τE_simp)**c)**(1/c))
print(λx_simp,λy_simp,λτ_simp,λe_simp)

# Characteristic Buckling Resistance for serviceability
σscrx_simp=σ_kx/sqrt(1+λx_simp**4)
σscry_simp=σ_ky/sqrt(1+λy_simp**4)
σscrz_simp=τ_k/sqrt(1+λτ_simp**4)
σescr_simp=f/sqrt(1+λe_simp**4)
print(σscrx_simp,σscry_simp,σscrz_simp,σescr_simp)

# Usage factor for serviceability check, Simply Supported
ηsx_simp=σ_xx/σscrx_simp
FALSE=σ_yy/σscry_simp
ηsz_simp=τ/σscrz_simp
ηse_simp=σ_e1/σescr_simp
print(ηsx_simp,FALSE,ηsz_simp,ηse_simp)

# Characteristic Buckling Resistance for Ultimate check
σucrx_simp1=(σ_kx/(sqrt(1+λx_simp**4)))
σucrx_simp2=σ_kx/sqrt(2)/λx_simp
if(λx_simp<1):
    print("The value of σucrx_simp1 is ",σucrx_simp1)
else:
    print("The value of σucrx_simp2 is",σucrx_simp2)
σucry_simp1=(σ_ky/(sqrt(1+λy_simp**4)))
σucry_simp2=σ_ky/sqrt(2)/λy_simp
if(λy_simp<1):
    print("The value of σucry_simp1 is ",σucry_simp1)
else:
    print("The value of σucry_simp2 is",σucry_simp2)
σucrz_simp1=(τ_k/(sqrt(1+λτ_simp**4)))
σucrz_simp2=τ_k/sqrt(2)/λτ_simp
if(λτ_simp<1):
    print("The value of σucrz_simp1 is ",σucrz_simp1)
else:
    print("The value of σucrz_simp2 is",σucrz_simp2)
σeucr_simp1=(σ_e/(sqrt(1+λe_simp**4)))
σeucr_simp2=σ_e/sqrt(2)/λe_simp
if(λe_simp<1):
    print("The value of σeucr_simp1 is ",σeucr_simp1)
else:
    print("The value of σeucr_simp2 is",σeucr_simp2)

# Usage factor for ultimate check, , Simply Supported
ηux_simp=σ_xx/σucrx_simp1
ηuy_simp=σ_yy/σucry_simp2
ηuz_simp=τ/σucrz_simp1
ηue_simp=σ_e1/σeucr_simp2
print(ηux_simp,ηuy_simp,ηuz_simp,ηue_simp)

# Sides clamped - Uniform Loading
cxx=7.00
cyy=(1+2.5*x1**2+5*x1**4)
cτ=(9+5.6*x1**2)
print(cxx,cyy,cτ)

# Elastic Buckling Resistance for each stress direction
σExx_Simp=x6*cxx*x3**2
σEyy_Simp=x6*cyy*x3**2
τE_Simp=x6*cτ*x3**2
print(σExx_Simp,σEyy_Simp,τE_Simp)

# Reduced Slenders ratio
λx_side=sqrt(σ_kx/σExx_Simp)
λy_side=sqrt(σ_ky/σEyy_Simp)
λτ_side=sqrt(τ_k/τE_Simp)
λe_side=sqrt(f/σ_e1*((σ_xx/σExx_Simp)**c+(σ_yy/σEyy_Simp)**c+(τ/τE_Simp)**c)**(1/c))
print(λx_side,λy_side,λτ_side,λe_side)

# Characteristic Buckling Resistance for serviceability
σscrx_side=σ_kx/sqrt(1+λx_side**4)
σscry_side=σ_ky/sqrt(1+λy_side**4)
σscrz_side=τ_k/sqrt(1+λτ_side**4)
σescr_side=f/sqrt(1+λe_side**4)
print(σscrx_side,σscry_side,σscrz_side,σescr_side)

# Usage factor for serviceability check, Sides Clamped
ηsx_side=σ_xx/σscrx_side
ηsy_side=σ_yy/σscry_side
ηsz_side=τ/σscrz_side
ηse_side=σ_e1/σescr_side
print(ηsx_side,ηsy_side,ηsz_side,ηse_side)

# Characteristic Buckling Resistance for Ultimate Check
σucrx_side1=σ_kx/(sqrt(1+λx_side**4))
σucrx_side2=σ_kx/sqrt(2)/λx_side
if(λx_side<1):
    print("The value of σucrx_side1 is ",σucrx_side1)
else:
    print("The value of σucrx_side2 is",σucrx_side2)
σucry_side1=σ_ky/(sqrt(1+λy_side**4))
σucry_side2=σ_ky/sqrt(2)/λy_side
if(λy_side<1):
    print("The value of σucry_side1 is ",σucry_side1)
else:
    print("The value of σucry_side2 is",σucry_side2)
σucrz_side1=τ_k/(sqrt(1+λτ_side**4))
σucrz_side2=τ_k/sqrt(2)/λτ_side
if(λτ_side<1):
    print("The value of σucrz_side1 is ",σucrz_side1)
else:
    print("The value of σucrz_side2 is",σucrz_side2)
σeucr_side1=σ_e/(sqrt(1+λe_side**4))
σeucr_side2=σ_e/sqrt(2)/λe_side
if(λe_side<1):
    print("The value of σeucr_side1 is",σeucr_side1)
else:
    print("The value of σeucr_side2 is",σeucr_side2)


# Usage factor for ultimate check, Sides Clamped
ηux_side=σ_xx/σucrx_side1
ηuy_side=σ_yy/σucry_side2
ηuz_side=τ/σucrz_side1
ηue_side=σ_e1/σeucr_side2
print(ηux_side,ηuy_side,ηuz_side,ηue_side)










