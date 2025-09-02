# Plate1 data is obtained from workbook: "614-CAL-2215-01 (SEWOL ANSYS Buckling Template) DRAFT2.xlsm";
# worksheet: "Standard Buckling Calculations" ; Column G
import munch

plateGData1 = {'PlateLength': 2.69, 'PlateLength_unit' : 'm',
                  'PlateBreadth' : 0.70, 'PlateBreadth_unit' : 'm',
                  'PlateThickness' : 0.014, 'PlateThickness_unit' : 'm',
                  'AverageWaterDepth' : 40, 'AverageWaterDepth_unit' : 'm',
                  'YieldStrength' : 34 , 'YieldStrength_unit' : 'ksi',
                  'PoissionsRatio' : 0.30,
                  'YoungsModulus' : 30450, 'YoungsModulus_unit' : 'ksi'}


plateGDataFT1 = {'PlateLength': 8.82, 'PlateLength_unit' : 'ft',
                  'PlateBreadth' : 2.30, 'PlateBreadth_unit' : 'ft',
                  'PlateThickness' : 0.046, 'PlateThickness_unit' : 'ft',
                  'AverageWaterDepth' : 131.23, 'AverageWaterDepth_unit' : 'ft',
                  'YieldStrength' : 34 , 'YieldStrength_unit' : 'ksi',
                  'PoissionsRatio' : 0.30,
                  'YoungsModulus' : 30450, 'YoungsModulus_unit' : 'ksi'}


plateGLoading1 = {'LongtudinalStress' : 0.5, 'LongtudinalStress_unit' : 'ksi',
                'TransverseStress' : 0.5, 'TransverseStress_unit' : 'ksi',
                'ShearStress' : 0.7, 'ShearStress_unit' : 'ksi'}

plateGData = munch.munchify(plateGData1)
plateGDataFT = munch.munchify(plateGDataFT1)
plateGLoading = munch.munchify(plateGLoading1)

l = plateGDataFT.PlateLength
s = plateGDataFT.PlateBreadth
t = plateGDataFT.PlateThickness
d = plateGDataFT.AverageWaterDepth
f = plateGDataFT.YieldStrength
p = plateGDataFT.PoissionsRatio
E = plateGDataFT.YoungsModulus

L = plateGData["PlateLength"]
S = plateGData["PlateBreadth"]
T = plateGData["PlateThickness"]
D = plateGData["AverageWaterDepth"]

σ_xx = plateGLoading["LongtudinalStress"]
σ_yy = plateGLoading["TransverseStress"]
τ    = plateGLoading["ShearStress"]

print("The value of inputs is",l,s,t,d,f,p,E,σ_xx,σ_yy,τ,L,S,T,D)
