# Plate1 data is obtained from workbook: "";
# 

import munch

plateGData1 = {   'YoungsModulus' : 30450, 'YoungsModulus_unit' : 'ksi',
                  'YieldStrength' : 34, 'YieldStrength_unit' : 'ksi',
                  'PoissionsRatio' : 0.30,
                  'ShearModulus' : 11712, 'ShearModulus_unit' : 'ksi',
                  'Ksi' : 6.9, 'Ksi_unit' : 'psi',
                  'MaterialResistanceFactor' : 1.15, 'MaterialResistanceFactor_unit' : 'Mpa',

                  'LengthofPanel': 6330, 'LengthofPanel_unit' : 'mm',
                  
                  'WidthofAdjoiningHullPlate': 700, 'WidthofAdjoiningHullPlate_Unit' : 'mm',
                  
                  'ThicknessofAdjoiningHullPlate': 12.0, 'ThicknessofAdjoiningHullPlate_Unit' : 'mm',
                  


                   #'SectionType': T, 

                  'WebHeight': 600.0, 'WebHeight_unit' : 'mm',

                  'ThicknessofWeb': 10.0, 'ThicknessofWeb_unit' : 'mm',

                  'ThicknessofFlange': 20.0, 'ThicknessofFlange_unit' : 'mm',

                  'WidthofFlange': 200.0, 'WidthofFlange_unit' : 'mm',

                  'SpanLength': 2230.0, 'SpanLength_unit' : 'mm',

                  'DistanceBetweenSideWaysofStiffner': 3500.0, 'WidthofFlange_unit' : 'mm',
                  
                  


                   #'SectionType': T, 

                  'WebHeightofGrider': 600.0, 'WebHeightofGrider_unit' : 'mm',

                  'ThicknessofWebGrider': 9.0, 'ThicknessofWebGrider_unit' : 'mm',

                  'ThicknessofFlangeGrider': 12.0, 'ThicknessofFlangeGrider_unit' : 'mm',

                  'WidthofFlangeGrider': 100.0, 'WidthofFlangeGrider_unit' : 'mm',

                  'LengthofGrider': 3500.0, 'LengthofGrider_unit' : 'mm',

                  'EffectiveLengthofGrider': 3500.0, 'EffectiveLengthofGrider_unit' : 'mm',

                   #'POCompressionFlag': Yes,       

                  
                  }
               
                   

plateGData = munch.munchify(plateGData1)



E_G=plateGData["YoungsModulus"]

fy_G=plateGData["YieldStrength"]

U_G=plateGData["PoissionsRatio"]

G_G=plateGData["ShearModulus"]

Ksi_G=plateGData["Ksi"]

ym_G=plateGData["MaterialResistanceFactor"]

LP_G=plateGData["LengthofPanel"]

s_G=plateGData["WidthofAdjoiningHullPlate"]

t_G=plateGData["ThicknessofAdjoiningHullPlate"]


#st_G=plateGData["SectionType"]

HW_G=plateGData["WebHeight"]

TW_G=plateGData["ThicknessofWeb"]

TF_G=plateGData["ThicknessofFlange"]

B_G=plateGData["WidthofFlange"]

L_G=plateGData["SpanLength"]

Lt_G=plateGData["DistanceBetweenSideWaysofStiffner"]



#st_G=plateGData["SectionType"]

hwG_G=plateGData["WebHeightofGrider"]

tw_G=plateGData["ThicknessofWebGrider"]

tf_G=plateGData["ThicknessofFlangeGrider"]

b_G=plateGData["WidthofFlangeGrider"]


LG_G=plateGData["LengthofGrider"]

LGe_G=plateGData["EffectiveLengthofGrider"]




#PO_G=plateGData["POCompressionFlag"]


σxsd=15.719916 #39

σysd=48.5 #41

τsd=0.9  #43

Qsd_G=0 #49

Z1_G=0  #47













