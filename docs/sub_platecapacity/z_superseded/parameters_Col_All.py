# Plate1 data is obtained from workbook: "614-CAL-2215-01 (SEWOL ANSYS Buckling Template) DRAFT2.xlsm";
# worksheet: "Standard Buckling Calculations" ; Column G

import munch

plateHData = {'PlateLength': 2.64, 'PlateLength_unit' : 'm',
                  'PlateBreadth' : 0.70, 'PlateBreadth_unit' : 'm',
                  'PlateThickness' : 0.010, 'PlateThickness_unit' : 'm',
                  'AverageWaterDepth' : 40, 'AverageWaterDepth_unit' : 'm',
                  'YieldStrength' : 34 , 'YieldStrength_unit' : 'ksi',
                  'PoissionsRatio' : 0.30,
                  'YoungsModulus' : 30450, 'YoungsModulus_unit' : 'ksi'}


plateHDataFT = {'PlateLength': 8.67, 'PlateLength_unit' : 'ft',
                  'PlateBreadth' : 2.30, 'PlateBreadth_unit' : 'ft',
                  'PlateThickness' : 0.033, 'PlateThickness_unit' : 'ft',
                  'AverageWaterDepth' : 131.23, 'AverageWaterDepth_unit' : 'ft',
                  'YieldStrength' : 34 , 'YieldStrength_unit' : 'ksi',
                  'PoissionsRatio' : 0.30,
                  'YoungsModulus' : 30450, 'YoungsModulus_unit' : 'ksi'}


plateHLoading = {'LongtudinalStress' : 0.3, 'LongtudinalStress_unit' : 'ksi',
                'TransverseStress' : 1.9, 'TransverseStress_unit' : 'ksi',
                'ShearStress' : 1.0, 'ShearStress_unit' : 'ksi'}


constantvalue = {'BucklingFactor' : 0.15}

# How to access objects from above dictionaries (also same for JSON format files)
print(plateHLoading["LongtudinalStress"])


l = plateHDataFT["PlateLength"]
s = plateHDataFT["PlateBreadth"]
t = plateHDataFT["PlateThickness"]
d = plateHDataFT["AverageWaterDepth"]
f = plateHDataFT["YieldStrength"]
p = plateHDataFT["PoissionsRatio"]
E = plateHDataFT["YoungsModulus"]

L = plateHData["PlateLength"]
S = plateHData["PlateBreadth"]
T = plateHData["PlateThickness"]
D = plateHData["AverageWaterDepth"]



σH_xx = plateHLoading["LongtudinalStress"]
σH_yy = plateHLoading["TransverseStress"]
τ_H    = plateHLoading["ShearStress"]

print("The value of inputs is",l,s,t,d,f,p,E,L,S,T,D,σH_xx,σH_yy,τ_H)



k4_H = constantvalue["BucklingFactor"]


# algorithms with  columns-I, 



plateIData = {'PlateLength': 2.74, 'PlateLength_unit' : 'm',
                  'PlateBreadth' : 0.70, 'PlateBreadth_unit' : 'm',
                  'PlateThickness' : 0.010, 'PlateThickness_unit' : 'm',
                  'AverageWaterDepth' : 40, 'AverageWaterDepth_unit' : 'm',
                  'YieldStrength' : 34 , 'YieldStrength_unit' : 'ksi',
                  'PoissionsRatio' : 0.30,
                  'YoungsModulus' : 30450, 'YoungsModulus_unit' : 'ksi'}


plateIDataFT = {'PlateLength': 9, 'PlateLength_unit' : 'ft',
                  'PlateBreadth' : 2.30, 'PlateBreadth_unit' : 'ft',
                  'PlateThickness' : 0.033, 'PlateThickness_unit' : 'ft',
                  'AverageWaterDepth' : 131.23, 'AverageWaterDepth_unit' : 'ft',
                  'YieldStrength' : 34 , 'YieldStrength_unit' : 'ksi',
                  'PoissionsRatio' : 0.30,
                  'YoungsModulus' : 30450, 'YoungsModulus_unit' : 'ksi'}


plateILoading = {'LongtudinalStress' : 0.6, 'LongtudinalStress_unit' : 'ksi',
                'TransverseStress' : 3.0, 'TransverseStress_unit' : 'ksi',
                'ShearStress' : 2.2, 'ShearStress_unit' : 'ksi'}


constantvalue = {'BucklingFactor' : 0.15}

# How to access objects from above dictionaries (also same for JSON format files)
print(plateILoading["LongtudinalStress"])


l_I = plateIDataFT["PlateLength"]
s_I = plateIDataFT["PlateBreadth"]
t_I = plateIDataFT["PlateThickness"]
d_I = plateIDataFT["AverageWaterDepth"]
f_I = plateIDataFT["YieldStrength"]
p_I = plateIDataFT["PoissionsRatio"]
E_I = plateIDataFT["YoungsModulus"]

L_I = plateIData["PlateLength"]
S_I = plateIData["PlateBreadth"]
T_I = plateIData["PlateThickness"]
D_I = plateIData["AverageWaterDepth"]



σI_xx = plateILoading["LongtudinalStress"]
σI_yy = plateILoading["TransverseStress"]
τ_I    = plateILoading["ShearStress"]

print("The value of inputs is",l_I,s_I,t_I,d_I,f_I,p_I,E_I,L_I,S_I,T_I,D_I,σI_xx,σI_yy,τ_I)



k4_I = constantvalue["BucklingFactor"]

# algorithms with  columns-J

plateJData = {'PlateLength': 1.65, 'PlateLength_unit' : 'm',
                  'PlateBreadth' : 0.70, 'PlateBreadth_unit' : 'm',
                  'PlateThickness' : 0.013, 'PlateThickness_unit' : 'm',
                  'AverageWaterDepth' : 40, 'AverageWaterDepth_unit' : 'm',
                  'YieldStrength' : 34 , 'YieldStrength_unit' : 'ksi',
                  'PoissionsRatio' : 0.30,
                  'YoungsModulus' : 30450, 'YoungsModulus_unit' : 'ksi'}


plateJDataFT = {'PlateLength': 5.43, 'PlateLength_unit' : 'ft',
                  'PlateBreadth' : 2.30, 'PlateBreadth_unit' : 'ft',
                  'PlateThickness' : 0.043, 'PlateThickness_unit' : 'ft',
                  'AverageWaterDepth' : 131.23, 'AverageWaterDepth_unit' : 'ft',
                  'YieldStrength' : 34 , 'YieldStrength_unit' : 'ksi',
                  'PoissionsRatio' : 0.30,
                  'YoungsModulus' : 30450, 'YoungsModulus_unit' : 'ksi'}


plateJLoading = {'LongtudinalStress' : 0.8, 'LongtudinalStress_unit' : 'ksi',
                'TransverseStress' : 4.5, 'TransverseStress_unit' : 'ksi',
                'ShearStress' : 0.7, 'ShearStress_unit' : 'ksi'}


constantvalue = {'BucklingFactor' : 0.23}

# How to access objects from above dictionaries (also same for JSON format files)
print(plateJLoading["LongtudinalStress"])


l_J = plateJDataFT["PlateLength"]
s_J = plateJDataFT["PlateBreadth"]
t_J = plateJDataFT["PlateThickness"]
d_J = plateJDataFT["AverageWaterDepth"]
f_J = plateJDataFT["YieldStrength"]
p_J = plateJDataFT["PoissionsRatio"]
E_J = plateJDataFT["YoungsModulus"]

L_J = plateJData["PlateLength"]
S_J = plateJData["PlateBreadth"]
T_J = plateJData["PlateThickness"]
D_J = plateJData["AverageWaterDepth"]



σJ_xx = plateJLoading["LongtudinalStress"]
σJ_yy = plateJLoading["TransverseStress"]
τ_J    = plateJLoading["ShearStress"]

print("The value of inputs is",l_J,s_J,t_J,d_J,f_J,p_J,E_J,L_J,S_J,T_J,D_J,σJ_xx,σJ_yy,τ_J)



k4_J = constantvalue["BucklingFactor"]



# algorithms with  columns-K

plateKData = {'PlateLength': 1.84, 'PlateLength_unit' : 'm',
                  'PlateBreadth' : 0.70, 'PlateBreadth_unit' : 'm',
                  'PlateThickness' : 0.012, 'PlateThickness_unit' : 'm',
                  'AverageWaterDepth' : 40, 'AverageWaterDepth_unit' : 'm',
                  'YieldStrength' : 34 , 'YieldStrength_unit' : 'ksi',
                  'PoissionsRatio' : 0.30,
                  'YoungsModulus' : 30450, 'YoungsModulus_unit' : 'ksi'}


plateKDataFT = {'PlateLength': 6.03, 'PlateLength_unit' : 'ft',
                  'PlateBreadth' : 2.30, 'PlateBreadth_unit' : 'ft',
                  'PlateThickness' : 0.039, 'PlateThickness_unit' : 'ft',
                  'AverageWaterDepth' : 131.23, 'AverageWaterDepth_unit' : 'ft',
                  'YieldStrength' : 34 , 'YieldStrength_unit' : 'ksi',
                  'PoissionsRatio' : 0.30,
                  'YoungsModulus' : 30450, 'YoungsModulus_unit' : 'ksi'}


plateKLoading = {'LongtudinalStress' : 0.0, 'LongtudinalStress_unit' : 'ksi',
                'TransverseStress' : 5.8, 'TransverseStress_unit' : 'ksi',
                'ShearStress' : 0.8, 'ShearStress_unit' : 'ksi'}


constantvalue = {'BucklingFactor' : 0.18}

# How to access objects from above dictionaries (also same for JSON format files)
print(plateKLoading["LongtudinalStress"])


l_K = plateKDataFT["PlateLength"]
s_K = plateKDataFT["PlateBreadth"]
t_K = plateKDataFT["PlateThickness"]
d_K = plateKDataFT["AverageWaterDepth"]
f_K = plateKDataFT["YieldStrength"]
p_K = plateKDataFT["PoissionsRatio"]
E_K = plateKDataFT["YoungsModulus"]

L_K = plateKData["PlateLength"]
S_K = plateKData["PlateBreadth"]
T_K = plateKData["PlateThickness"]
D_K = plateKData["AverageWaterDepth"]



σK_xx = plateKLoading["LongtudinalStress"]
σK_yy = plateKLoading["TransverseStress"]
τ_K    = plateKLoading["ShearStress"]

print("The value of inputs is",l_K,s_K,t_K,d_K,f_K,p_K,E_K,L_K,S_K,T_K,D_K,σK_xx,σK_yy,τ_K)



k4_K = constantvalue["BucklingFactor"]





