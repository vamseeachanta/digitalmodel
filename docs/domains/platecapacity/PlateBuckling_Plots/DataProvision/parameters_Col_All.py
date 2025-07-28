# Plate1 data is obtained from workbook: "614-CAL-2215-01 (SEWOL ANSYS Buckling Template) DRAFT2.xlsm";
# worksheet: "Standard Buckling Calculations" ; Column (G-H-I-J-K)

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

# How to access objects from above dictionaries (also same for JSON format files)


constantGvalue1 = {'BucklingFactor' : 0.26,
                   'BCedges_simplysupported_long': 4,
                   'BC_sideclamped_long' : 7.00,
                   'Resulting material factor': 1.15,
                   'H4' : 101325,
                   'H5' : 1025 ,
                   'H6' : 9.81,
                   'H7' : 0.000145038,
                   'H8' : 0.001,
                   'BR_transversedirection' : 1,
                   'Integralfactor' : 0,
                   'BA_sheardirection' : 1}


constantGvalue = munch.munchify(constantGvalue1)
plateGData = munch.munchify(plateGData1)
plateGDataFT = munch.munchify(plateGDataFT1)
plateGLoading = munch.munchify(plateGLoading1)

l_G = plateGDataFT["PlateLength"]
s_G = plateGDataFT["PlateBreadth"]
t_G = plateGDataFT["PlateThickness"]
d_G = plateGDataFT["AverageWaterDepth"]
f_G = plateGDataFT["YieldStrength"]
p_G = plateGDataFT["PoissionsRatio"]
E_G = plateGDataFT["YoungsModulus"]

L_G = plateGData["PlateLength"]
S_G = plateGData["PlateBreadth"]
T_G = plateGData["PlateThickness"]
D_G = plateGData["AverageWaterDepth"]



σG_xx = plateGLoading["LongtudinalStress"]
σG_yy = plateGLoading["TransverseStress"]
τ_G    = plateGLoading["ShearStress"]





k4_G  = constantGvalue["BucklingFactor"]
c_xx  = constantGvalue["BCedges_simplysupported_long"]
cxx   = constantGvalue["BC_sideclamped_long"]
ϒ_M   = constantGvalue["Resulting material factor"]
x7    = constantGvalue["H4"]
x8    = constantGvalue["H5"]
x9    = constantGvalue["H6"]
x10   = constantGvalue["H7"]
x11   = constantGvalue["H8"]
C_τ   = constantGvalue["BR_transversedirection"]
ci_1  = constantGvalue["Integralfactor"]
C_τe2 = constantGvalue["BA_sheardirection"]



#COL H

plateHData1 = {'PlateLength': 2.64, 'PlateLength_unit' : 'm',
                  'PlateBreadth' : 0.70, 'PlateBreadth_unit' : 'm',
                  'PlateThickness' : 0.010, 'PlateThickness_unit' : 'm',
                  'AverageWaterDepth' : 40, 'AverageWaterDepth_unit' : 'm',
                  'YieldStrength' : 34 , 'YieldStrength_unit' : 'ksi',
                  'PoissionsRatio' : 0.30,
                  'YoungsModulus' : 30450, 'YoungsModulus_unit' : 'ksi'}


plateHDataFT1 = {'PlateLength': 8.67, 'PlateLength_unit' : 'ft',
                  'PlateBreadth' : 2.30, 'PlateBreadth_unit' : 'ft',
                  'PlateThickness' : 0.033, 'PlateThickness_unit' : 'ft',
                  'AverageWaterDepth' : 131.23, 'AverageWaterDepth_unit' : 'ft',
                  'YieldStrength' : 34 , 'YieldStrength_unit' : 'ksi',
                  'PoissionsRatio' : 0.30,
                  'YoungsModulus' : 30450, 'YoungsModulus_unit' : 'ksi'}


plateHLoading1 = {'LongtudinalStress' : 0.3, 'LongtudinalStress_unit' : 'ksi',
                'TransverseStress' : 1.9, 'TransverseStress_unit' : 'ksi',
                'ShearStress' : 1.0, 'ShearStress_unit' : 'ksi'}


constantHvalue1 = {'BucklingFactor' : 0.15,
                   'BCedges_simplysupported_long': 4,
                   'BC_sideclamped_long' : 7.00,
                   'Resulting material factor': 1.15,
                   'H4' : 101325,
                   'H5' : 1025 ,
                   'H6' : 9.81,
                   'H7' : 0.000145038,
                   'H8' : 0.001,
                   'BR_transversedirection' : 1,
                   'Integralfactor' : 0,
                   'BA_sheardirection' : 1}


constantHvalue = munch.munchify(constantHvalue1)
plateHData = munch.munchify(plateHData1)
plateHDataFT = munch.munchify(plateHDataFT1)
plateHLoading = munch.munchify(plateHLoading1)


# How to access objects from above dictionaries (also same for JSON format files)



l_H = plateHDataFT["PlateLength"]
s_H = plateHDataFT["PlateBreadth"]
t_H = plateHDataFT["PlateThickness"]
d_H = plateHDataFT["AverageWaterDepth"]
f_H = plateHDataFT["YieldStrength"]
p_H  = plateHDataFT["PoissionsRatio"]
E_H = plateHDataFT["YoungsModulus"]

L_H = plateHData["PlateLength"]
S_H = plateHData["PlateBreadth"]
T_H = plateHData["PlateThickness"]
D_H = plateHData["AverageWaterDepth"]



σH_xx = plateHLoading["LongtudinalStress"]
σH_yy = plateHLoading["TransverseStress"]
τ_H    = plateHLoading["ShearStress"]





k4_H  = constantHvalue["BucklingFactor"]
c_xx  = constantHvalue["BCedges_simplysupported_long"]
cxx   = constantHvalue["BC_sideclamped_long"]
ϒ_M   = constantHvalue["Resulting material factor"]
x7    = constantHvalue["H4"]
x8    = constantHvalue["H5"]
x9    = constantHvalue["H6"]
x10   = constantHvalue["H7"]
x11   = constantHvalue["H8"]
C_τ   = constantHvalue["BR_transversedirection"]
ci_1  = constantHvalue["Integralfactor"]
C_τe2 = constantHvalue["BA_sheardirection"]
 


# algorithms with  columns-I, 



plateIData1 = {'PlateLength': 2.74, 'PlateLength_unit' : 'm',
                  'PlateBreadth' : 0.70, 'PlateBreadth_unit' : 'm',
                  'PlateThickness' : 0.010, 'PlateThickness_unit' : 'm',
                  'AverageWaterDepth' : 40, 'AverageWaterDepth_unit' : 'm',
                  'YieldStrength' : 34 , 'YieldStrength_unit' : 'ksi',
                  'PoissionsRatio' : 0.30,
                  'YoungsModulus' : 30450, 'YoungsModulus_unit' : 'ksi'}


plateIDataFT1 = {'PlateLength': 9, 'PlateLength_unit' : 'ft',
                  'PlateBreadth' : 2.30, 'PlateBreadth_unit' : 'ft',
                  'PlateThickness' : 0.033, 'PlateThickness_unit' : 'ft',
                  'AverageWaterDepth' : 131.23, 'AverageWaterDepth_unit' : 'ft',
                  'YieldStrength' : 34 , 'YieldStrength_unit' : 'ksi',
                  'PoissionsRatio' : 0.30,
                  'YoungsModulus' : 30450, 'YoungsModulus_unit' : 'ksi'}


plateILoading1 = {'LongtudinalStress' : 0.6, 'LongtudinalStress_unit' : 'ksi',
                'TransverseStress' : 3.0, 'TransverseStress_unit' : 'ksi',
                'ShearStress' : 2.2, 'ShearStress_unit' : 'ksi'}



constantIvalue1 = {'BucklingFactor' : 0.15,
                   'BCedges_simplysupported_long': 4,
                   'BC_sideclamped_long' : 7.00,
                   'Resulting material factor': 1.15,
                   'H4' : 101325,
                   'H5' : 1025 ,
                   'H6' : 9.81,
                   'H7' : 0.000145038,
                   'H8' : 0.001,
                   'BR_transversedirection' : 1,
                   'Integralfactor' : 0,
                   'BA_sheardirection' : 1}


constantIvalue = munch.munchify(constantIvalue1)
plateIData = munch.munchify(plateIData1)
plateIDataFT = munch.munchify(plateIDataFT1)
plateILoading = munch.munchify(plateILoading1)


# How to access objects from above dictionaries (also same for JSON format files)



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




k4_I  = constantIvalue["BucklingFactor"]
c_xx  = constantIvalue["BCedges_simplysupported_long"]
cxx   = constantIvalue["BC_sideclamped_long"]
ϒ_M   = constantIvalue["Resulting material factor"]
x7    = constantIvalue["H4"]
x8    = constantIvalue["H5"]
x9    = constantIvalue["H6"]
x10   = constantIvalue["H7"]
x11   = constantIvalue["H8"]
C_τ   = constantIvalue["BR_transversedirection"]
ci_1  = constantIvalue["Integralfactor"]
C_τe2 = constantIvalue["BA_sheardirection"]





# algorithms with  columns-J

plateJData1 = {'PlateLength': 1.65, 'PlateLength_unit' : 'm',
                  'PlateBreadth' : 0.70, 'PlateBreadth_unit' : 'm',
                  'PlateThickness' : 0.013, 'PlateThickness_unit' : 'm',
                  'AverageWaterDepth' : 40, 'AverageWaterDepth_unit' : 'm',
                  'YieldStrength' : 34 , 'YieldStrength_unit' : 'ksi',
                  'PoissionsRatio' : 0.30,
                  'YoungsModulus' : 30450, 'YoungsModulus_unit' : 'ksi'}


plateJDataFT1 = {'PlateLength': 5.43, 'PlateLength_unit' : 'ft',
                  'PlateBreadth' : 2.30, 'PlateBreadth_unit' : 'ft',
                  'PlateThickness' : 0.043, 'PlateThickness_unit' : 'ft',
                  'AverageWaterDepth' : 131.23, 'AverageWaterDepth_unit' : 'ft',
                  'YieldStrength' : 34 , 'YieldStrength_unit' : 'ksi',
                  'PoissionsRatio' : 0.30,
                  'YoungsModulus' : 30450, 'YoungsModulus_unit' : 'ksi'}


plateJLoading1 = {'LongtudinalStress' : 0.8, 'LongtudinalStress_unit' : 'ksi',
                'TransverseStress' : 4.5, 'TransverseStress_unit' : 'ksi',
                'ShearStress' : 0.7, 'ShearStress_unit' : 'ksi'}


constantJvalue1 = {'BucklingFactor' : 0.23,
                   'BCedges_simplysupported_long': 4,
                   'BC_sideclamped_long' : 7.00,
                   'Resulting material factor': 1.15,
                   'H4' : 101325,
                   'H5' : 1025 ,
                   'H6' : 9.81,
                   'H7' : 0.000145038,
                   'H8' : 0.001,
                   'BR_transversedirection' : 1,
                   'Integralfactor' : 0,
                   'BA_sheardirection' : 1}


constantJvalue = munch.munchify(constantJvalue1)
plateJData = munch.munchify(plateJData1)
plateJDataFT = munch.munchify(plateJDataFT1)
plateJLoading = munch.munchify(plateJLoading1)

# How to access objects from above dictionaries (also same for JSON format files)



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





k4_J  = constantJvalue["BucklingFactor"]
c_xx  = constantJvalue["BCedges_simplysupported_long"]
cxx   = constantJvalue["BC_sideclamped_long"]
ϒ_M   = constantJvalue["Resulting material factor"]
x7    = constantJvalue["H4"]
x8    = constantJvalue["H5"]
x9    = constantJvalue["H6"]
x10   = constantJvalue["H7"]
x11   = constantJvalue["H8"]
C_τ   = constantJvalue["BR_transversedirection"]
ci_1  = constantJvalue["Integralfactor"]
C_τe2 = constantJvalue["BA_sheardirection"]



# algorithms with  columns-K

plateKData1 = {'PlateLength': 1.84, 'PlateLength_unit' : 'm',
                  'PlateBreadth' : 0.70, 'PlateBreadth_unit' : 'm',
                  'PlateThickness' : 0.012, 'PlateThickness_unit' : 'm',
                  'AverageWaterDepth' : 40, 'AverageWaterDepth_unit' : 'm',
                  'YieldStrength' : 34 , 'YieldStrength_unit' : 'ksi',
                  'PoissionsRatio' : 0.30,
                  'YoungsModulus' : 30450, 'YoungsModulus_unit' : 'ksi'}


plateKDataFT1 = {'PlateLength': 6.03, 'PlateLength_unit' : 'ft',
                  'PlateBreadth' : 2.30, 'PlateBreadth_unit' : 'ft',
                  'PlateThickness' : 0.039, 'PlateThickness_unit' : 'ft',
                  'AverageWaterDepth' : 131.23, 'AverageWaterDepth_unit' : 'ft',
                  'YieldStrength' : 34 , 'YieldStrength_unit' : 'ksi',
                  'PoissionsRatio' : 0.30,
                  'YoungsModulus' : 30450, 'YoungsModulus_unit' : 'ksi'}


plateKLoading1 = {'LongtudinalStress' : 0.0, 'LongtudinalStress_unit' : 'ksi',
                'TransverseStress' : 5.8, 'TransverseStress_unit' : 'ksi',
                'ShearStress' : 0.8, 'ShearStress_unit' : 'ksi'}



constantKvalue1 = {'BucklingFactor' : 0.18,
                   'BCedges_simplysupported_long': 4,
                   'BC_sideclamped_long' : 7.00,
                   'Resulting material factor': 1.15,
                   'H4' : 101325,
                   'H5' : 1025 ,
                   'H6' : 9.81,
                   'H7' : 0.000145038,
                   'H8' : 0.001,
                   'BR_transversedirection' : 1,
                   'Integralfactor' : 0,
                   'BA_sheardirection' : 1}


constantKvalue = munch.munchify(constantKvalue1)
plateKData = munch.munchify(plateKData1)
plateKDataFT = munch.munchify(plateKDataFT1)
plateKLoading = munch.munchify(plateKLoading1)

# How to access objects from above dictionaries (also same for JSON format files)



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





k4_K  = constantKvalue["BucklingFactor"]
c_xx  = constantKvalue["BCedges_simplysupported_long"]
cxx   = constantKvalue["BC_sideclamped_long"]
ϒ_M   = constantKvalue["Resulting material factor"]
x7    = constantKvalue["H4"]
x8    = constantKvalue["H5"]
x9    = constantKvalue["H6"]
x10   = constantKvalue["H7"]
x11   = constantKvalue["H8"]
C_τ   = constantKvalue["BR_transversedirection"]
ci_1  = constantKvalue["Integralfactor"]
C_τe2 = constantKvalue["BA_sheardirection"]





