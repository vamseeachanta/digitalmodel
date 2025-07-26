import math
import matplotlib.pyplot as plt
from dataManager.loadConfiguration import loadConfiguration

configParams = loadConfiguration('dataManager//APISTD2RD.ini')

def APISTD2RDCalculation(APISTD2RD):

    #Burst Pressure Check
   
    BurstPressure_1=APISTD2RD.k*(APISTD2RD.SMYS+APISTD2RD.SMUS)*math.log(APISTD2RD.NominalOD_1/(APISTD2RD.NominalOD_1-2*APISTD2RD.NominalWT_1))
    print("BurstPressure_1 in Mpa : ","{0:.2f}".format(BurstPressure_1))
    print("BurstPressure_1 in Psi : ","{0:.2f}".format(BurstPressure_1*145.038))

    BurstPressure_2=APISTD2RD.k*(APISTD2RD.SMYS+APISTD2RD.SMUS)*math.log(APISTD2RD.NominalOD_1/(APISTD2RD.NominalOD_1-2*APISTD2RD.NominalWT_2))
    print("BurstPressure_2 in Mpa : ","{0:.2f}".format(BurstPressure_2))
    print("BurstPressure_2 in Psi : ","{0:.2f}".format(BurstPressure_2*145.038))
                           
    BurstPressure_3=APISTD2RD.k*(APISTD2RD.SMYS+APISTD2RD.SMUS)*math.log(APISTD2RD.NominalOD_1/(APISTD2RD.NominalOD_1-2*APISTD2RD.NominalWT_3))
    print("BurstPressure_3 in Mpa: ","{0:.2f}".format(BurstPressure_3))
    print("BurstPressure_3 in Psi: ","{0:.2f}".format(BurstPressure_3*145.038))

    BurstPressure_4=APISTD2RD.k*(APISTD2RD.SMYS+APISTD2RD.SMUS)*math.log(APISTD2RD.NominalOD_1/(APISTD2RD.NominalOD_1-2*APISTD2RD.NominalWT_4))
    print("BurstPressure_4 in Mpa : ","{0:.2f}".format(BurstPressure_4))
    print("BurstPressure_4 in Psi : ","{0:.2f}".format(BurstPressure_4*145.038))
	
    BurstPressure_5=APISTD2RD.k*(APISTD2RD.SMYS+APISTD2RD.SMUS)*math.log(APISTD2RD.NominalOD_1/(APISTD2RD.NominalOD_1-2*APISTD2RD.NominalWT_5))
    print("BurstPressure_5 in Mpa : ","{0:.2f}".format(BurstPressure_5))
    print("BurstPressure_5 in Psi: ","{0:.2f}".format(BurstPressure_5*145.038))
	
    BurstPressure_6=APISTD2RD.k*(APISTD2RD.SMYS+APISTD2RD.SMUS)*math.log(APISTD2RD.NominalOD_1/(APISTD2RD.NominalOD_1-2*APISTD2RD.NominalWT_6))
    print("BurstPressure_6 in Mpa: ","{0:.2f}".format(BurstPressure_6))
    print("BurstPressure_6 in Psi: ","{0:.2f}".format(BurstPressure_6*145.038))
	
    BurstPressure_7=APISTD2RD.k*(APISTD2RD.SMYS+APISTD2RD.SMUS)*math.log(APISTD2RD.NominalOD_1/(APISTD2RD.NominalOD_1-2*APISTD2RD.NominalWT_7))
    print("BurstPressure_7 in Mpa: ","{0:.2f}".format(BurstPressure_7))
    print("BurstPressure_7 in Psi: ","{0:.2f}".format(BurstPressure_7*145.038))
  

    x_axis = [0.281,0.312,0.344,0.375,0.406,0.438,0.469]

    y_axis1 = [BurstPressure_1*145.038,BurstPressure_2*145.038,BurstPressure_3*145.038,BurstPressure_4*145.038,BurstPressure_5*145.038,BurstPressure_6*145.038,BurstPressure_7*145.038]
    
    y_axis2 = [1878.78,2090.85,2310.78,2524.86,2739.94,2963.02,3180.16]
    
    #it creates plot tittle, color and size
    plt.suptitle('BurstPressure With Thickness', fontsize=14, color='black')
    #it creates plot Sub-tittle, color and size
    plt.title('Outer Diameter=14 inch, Design_FactorFD=0.60, API STD 2RD', fontsize=10, color='blue')
    #it creates plot X-axis name, fontsize and colour
    plt.xlabel('Thickness (Inchs)', fontsize=12,  color='black') 
    #it creates plot Y-axis name, fontsize and colour             
    plt.ylabel('Burst_Pressure(Psi)', fontsize=12, color='black')
    #plotting x and y values
    plt.plot(x_axis, y_axis1, '-b', label='BurstPressure')
    plt.plot(x_axis, y_axis2, '-r', label='DesignPressure')
    plt.legend(loc='upper left')
    plt.savefig("BurstPressure With Thickness_US_OD",dpi=800)

APISTD2RDCalculation(configParams.APISTD2RD)    
