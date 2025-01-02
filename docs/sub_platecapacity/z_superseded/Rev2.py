import xlrd
from xlrd import open_workbook,XL_CELL_TEXT
import pandas as pd
sheet1=pd.read_excel("C:/Users/AceEngineer-002/Desktop/PB/pandas/ParametricInputs.xlsx",sheetname='Locations')
sheet2=pd.read_excel("C:/Users/AceEngineer-002/Desktop/PB/pandas/ParametricInputs.xlsx",sheetname='Stress')
sheet3=pd.read_excel("C:/Users/AceEngineer-002/Desktop/PB/pandas/ParametricInputs.xlsx",sheetname='PlateNo')
x1=sheet1.max() #minimum length
x2=sheet1.min() #maximum length
y1=sheet2.max()
y2=sheet2.min()
length=x1-x2
y=sheet2.mean()*0.145
print("The value of LENGTH & MEAN STRESS is",length,y)
#Longitudinal
t1=sheet3[["PlateNo"]]
LengthPath1=(t1-1)*16+1
LengthPath2=LengthPath1+4
MinCompressivePath1=(t1-1)*24+3
MaxCompressivePath1=MinCompressivePath1
MeanCompressivePath1=MaxCompressivePath1
MinShearPath1=(t1-1)*24+5
MaxShearPath1=MinShearPath1
MeanShearPath1=MaxShearPath1
MinCompressivePath2=MinCompressivePath1+6
MaxCompressivePath2=MaxCompressivePath1+6
MeanCompressivePath2=MeanCompressivePath1+6
MinShearPath2=MinShearPath1+6
MaxShearPath2=MaxShearPath1+6
MeanShearPath2=MeanShearPath1+6
print("The longtidunal values",LengthPath1,LengthPath2,MinCompressivePath1,MaxCompressivePath1,MeanCompressivePath1,MinShearPath1,MaxShearPath1,MeanShearPath1,MinCompressivePath2,MaxCompressivePath2,MeanCompressivePath2,MinShearPath2,MaxShearPath2,MeanShearPath2)
#Transverse
LengthPath3=LengthPath1+10
LengthPath4=LengthPath2+10
MinCompressivePath3=(t1-1)*24+13
MaxCompressivePath3=MinCompressivePath3
MeanCompressivePath3=MaxCompressivePath3
MinShearPath3=(t1-1)*24+17
MaxShearPath3=MinShearPath3
MeanShearPath3=MaxShearPath3
MinCompressivePath4=MinCompressivePath3+6
MaxCompressivePath4=MaxCompressivePath3+6
MeanCompressivePath4=MeanCompressivePath3+6
MinShearPath4=MinShearPath3+6
MaxShearPath4=MaxShearPath3+6
MeanShearPath4=MeanShearPath3+6
print("The Transverse values",LengthPath3,LengthPath4,MinCompressivePath3,MaxCompressivePath3,MeanCompressivePath3,MinShearPath3,MaxShearPath3,MeanShearPath3,MinCompressivePath4,MaxCompressivePath4,MeanCompressivePath4,MinShearPath4,MaxShearPath4,MeanShearPath4)


