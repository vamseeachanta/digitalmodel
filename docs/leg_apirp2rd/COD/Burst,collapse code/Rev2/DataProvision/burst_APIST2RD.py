# Grade API H40 Pipe Data
import math

D = 14.9 #Diameter of pipe for outer riser (Units:Inch)
#Di = 11.9 #Diameter of pipe for inner riser (Units:Inch)
#t0 = 0.53  # Thickness of pipe for outer riser (units:inch) 
tfab =0.6
tca= 0.06

#pc= 4.8 #Collapse (Units:ksi)

# parameters

Dmax=2 #assumed greatest measured inside or outside diameter 
Dmin=1 #assumed smallest measured inside or outside diameter

##SeamlessPipe = 1.0 # αfab Fabrication Factor
##UOEPipe =0.85 # αfab Fabrication Factor
##UO =0.925 # αfab Fabrication Factor
##TRB=0.925 # αfab Fabrication Factor

αfab = 1.00    # Fabrication Factor at (Seamless Pipe)
#pi()= 3.14

#Material Properties for API ST2RD Burst Pressure 

E = 2.07E+05        #Youngs Modolus 
V = 0.3             # Poissions Ratio
k= 0.45             #is a parameter to account for variability in mechanical properties and wall thickness, and is equal
#Si= 95               #is the specified minimum yield strength of the pipe; (material= X65) for inner Riser
#Ui= 105              #is the specified minimum ultimate strength of the pipe.(material= X65) for inner Riser
S= 80              #is the specified minimum yield strength of the pipe; (material= X65) for outer Riser
U= 95             #is the specified minimum ultimate strength of the pipe.(material= X65) for outer Riser
δ0 = 0.01




