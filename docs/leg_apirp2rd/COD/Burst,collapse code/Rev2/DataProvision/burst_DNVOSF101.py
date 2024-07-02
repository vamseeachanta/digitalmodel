# DNV OS F101

import math

# pipe Proprties

t    = 0.625  #Thickness of Pipe units: Inch
D    = 42     #Diameter of pipe (Units:Inch)
tfab = 0.1  #Fabrication Tolerance (Units:Inch)
tcorr = 0.200 #Corrosive tolerance (Units:Inch)
fo = 3.00  #Ovality, out of roundness (%)
It1 = 0.525 #(Thickness - Installation (Units:Inch)
Ot1 = 0.325 #Thickness,- Operation (Units:Inch)	
It2 = 0.625 #(Thickness - Installation (Units:Inch)
Ot2 = 0.425 #Thickness,- Operation (Units:Inch)
It3 = 0.625 #(Thickness - Installation (Units:Inch)
Ot3 = 0.525 #Thickness,- Operation (Units:Inch)

# parameters # DNV-OS-F101 section 5 Table 5.5,5.4,5.7,13.4

#SLS/ULS/ALS = 1.15 # ϒm
#FLS = 1            # ϒm

Normally = 0.96 # αu
Supplementary = 1.00 #αu

Low= 1.046 # ϒSC PressureCointainment
Medium= 1.138 # ϒSC PressureCointainment
High= 1.308 # ϒSC PressureCointainment(

FLS =1.0 #ϒF,ϒE,ϒF''
ALS =1.0 #ϒF,ϒE,ϒF'',ϒA
ULS =1.2 #ϒF (SystemCheck)
ULS =1.0 #ϒF (SystemCheck)
ULS =1.2 #ϒE (SystemCheck)
ULS =1.0 #ϒE (SystemCheck)

ϒF   = 1.2     # Functional Load factor  at (ULS (System Check)
ϒE   = 1.1     # Enviromental loads factor at (ULS (Local Check)
ϒF1 = 1.0     # Interference Load factor at (FLS) 
ϒA   = 1.0     # Accidental load factor  at (ALS)

ϒSC  = 1.046   # Safety Class Resistance Factor at Pressure Cointainment (Low)
ϒm   = 1.15    # Material Resistance Factor
ϒc   = 1.0     # Condition Factor at(Otherwise)
αu   = 0.96    # Material Strength Factor at (Normal)
αfab = 1.00    # Fabrication Factor at (Seamless Pipe)
αmpt = 0.75
αspt = 1


# Loading Bending Moment # Assumed

Mf = -639.5 #Bending Moment from Functional loads (Units:KN-M)
ME = -639.5 #Bending Moment from Enviromntal loads (Units:KN-M)
Mf1= -639.5 #Bending Moment from Functional loads(Units:KN-M)
MA = 0 # Bending Moment from Accidental loads(Units:KN-M)

#Loading - Effective Tension # Assumed

TeF  = 0 # Effective Tension from Functional loads(Units:KN)
TeE  = 0 # Effective Tension from Enviromental loads(Units:KN)
TeF1 = 0 # Effective Tension from Functional loads(Units:KN) 
TeA  = 0 # Effective Tension from Accidental loads(Units:KN)


# Loading Pressure

Pd =3 #Design Pressure assumed
T = 25          # Temperature of Internal fluid
Pi = 3          #Internal Pressure
e=120
i=0
Pe=0.5 #External Pressure assumed
pbt1=1
plt=1.5
ph=1.1
pli=1.75
pmin=0 #Minimum Internal Pressure at Reference point (Pmin)

#Material Properties for DNV OS F101 Burst Pressure

β = -0.43 # assumed
αp= 3.64 #assumed
SMYS = 65           #Specified Minimum Yield Strength (material= X65)
SMTS = 77           #Specified Minimum Tensile Strength (material= X65)
E = 3.00E+04        #Youngs Modolus
V = 0.29            # Poissions Ratio


#For collapse pressure t1 wall thickness






