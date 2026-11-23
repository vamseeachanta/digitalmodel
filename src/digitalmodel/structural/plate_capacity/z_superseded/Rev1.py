"""Superseded plate buckling calculation revision 1 (archived)."""

import pandas as pd

from digitalmodel.infrastructure.utils.private_paths import private_data_path

# The inputs workbook sits in the private data folder (DIGITALMODEL_PRIVATE_DATA).
_INPUTS = private_data_path("ParametricInputs.xlsx")
sheet1=pd.read_excel(_INPUTS,sheetname='Locations')
sheet2=pd.read_excel(_INPUTS,sheetname='Stress')
# Gives the min value of LENGTH
x1=sheet1[["Fr11_Plate1_Path1.txt"]].min()
# Gives the Maximum value of LENGTH
x2=sheet1[["Fr11_Plate1_Path1.txt"]].max()
# Gives the min value of STRESS
y1=sheet2[["Fr11_Plate1_Path1.txt"]].min()
# Gives the min value of STRESS
y2=sheet2[["Fr11_Plate1_Path1.txt"]].max()
x=x2-x1                                        # Gives difference between minimum and maximum LENGTHS (700)
y=sheet2[["Fr11_Plate1_Path1.txt"]].mean()*0.145  # Gives MEAN STRESS (-0.400049)

