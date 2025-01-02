import xlrd
import pandas as pd
sheet1=pd.read_excel("C:/Users/AceEngineer-002/Desktop/PB/pandas/ParametricInputs.xlsx",sheetname='Locations')
sheet2=pd.read_excel("C:/Users/AceEngineer-002/Desktop/PB/pandas/ParametricInputs.xlsx",sheetname='Stress')
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

