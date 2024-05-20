#120 ksi Non Linear Stress Strain Curve
import xlrd
import matplotlib.pyplot as plt

workbook = xlrd.open_workbook('0159-ANL-0001-01 (120 ksi Non Linear Stress Strain Curve) (Released).xlsx')  # Open Excel file 
sheet = workbook.sheet_by_name(u'RO Analysis')   # Read Sheet

E = 2.12e+08  #Young's Modolous (E)

SigmaYS = 8.27e+05  #Yield Strength of material (SigmaYS)

K = 0.002

n = 18.85

SigmaStressList = sheet.col_values(5,21,76)    #Stress in KPa
##print(SigmaStressList) #List of float values which can't be callable

int_SigmaStressList=[]
for intt in SigmaStressList:
   int_SigmaStressList.append(int(intt))  #converting float value to int
##print(int_SigmaStressList) # List of Stress values converted from float

StrainRatioValues = []

for SigmaS in int_SigmaStressList:
    e = (SigmaS/E)+(K*(SigmaS/SigmaYS)**n)  #Ramberg-Osgood Equation
    print("Strain Ratio for Stress is: ", e )
    StrainRatioValues.append(e)
##print(StrainRatioValues) # Strain Ratio values

#Plotting the Strain(%) and Stress(KPa) on graph
plt.plot(StrainRatioValues,SigmaStressList)
plt.title('Non-Linear Stress Strain Cruve')
plt.xlabel('Strain(%)', fontsize=15)
plt.ylabel('Stress(KPa)', fontsize=15)
plt.tight_layout()
plt.savefig('Non-Linear Stress Strain Cruve.png')
plt.show()
