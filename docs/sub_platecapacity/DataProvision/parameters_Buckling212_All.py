# Plate1 data is obtained from workbook: "614-CAL-2215-01 (SEWOL ANSYS Buckling Template) DRAFT2.xlsm";
# 

import munch

plateGData1 = {'PlateLength': 104.4, 'PlateLength_unit' : 'inches',
                  'PlateBreadth' : 27.6, 'PlateBreadth_unit' : 'inches',
                  'PlateThickness' : 0.552, 'PlateThickness_unit' : 'inches',
                  'PoissionsRatio' : 0.30,                 
                  'YoungsModulus' : 30000000, 'YoungsModulus_unit' : 'psi',
                  'TangentModulus' :25000000, 'TangentModulus_unit' : 'psi',
                  'YieldPoint' : 33000,       'YieldPoint_unit' : 'psi',
                  'CompressiveLoad1' : 33000, 'CompressiveLoad1_Unit' : 'psi',
                  'CompressiveLoad2' : 12280, 'CompressiveLoad2_unit' : 'psi',
                  'constantvalueTable1' : 0.425,
                  'constantvalueTable3' : 4,
                  'constantvalueTable5' : 0.425,
                  'constanttangentmodulus':1,
                  'constantplatelength1':1,
                  'constantplatelength2':0.8,
                  'RatioValue':40.0}
               
                   

plateGData = munch.munchify(plateGData1)


l1_p=plateGData["PlateLength"]
b_p=plateGData["PlateBreadth"]
t_p=plateGData["PlateThickness"]
v=plateGData["PoissionsRatio"]
E=plateGData["YoungsModulus"]
E_t=plateGData["TangentModulus"]
sig_y=plateGData["YieldPoint"]
sig_1=plateGData["CompressiveLoad1"]
sig_2=plateGData["CompressiveLoad2"]
k_1=plateGData["constantvalueTable1"]
k_3=plateGData["constantvalueTable3"]
k_5=plateGData["constantvalueTable5"]
lam_2=plateGData["constanttangentmodulus"]
l2_p=plateGData["constantplatelength1"]
l3_p=plateGData["constantplatelength2"]
x2=plateGData["RatioValue"]



