import math
import matplotlib.pyplot as plt
from dataManager.loadConfiguration import loadConfiguration

configParams = loadConfiguration('dataManager//APISTD2RD.ini')

def APISTD2RDCalculation(APISTD2RD):

    #Burst Pressure Check
   
    BurstPressure_1=APISTD2RD.k*(APISTD2RD.SMYS+APISTD2RD.SMUS)*math.log(APISTD2RD.NominalID_1/(APISTD2RD.NominalID_1-2*APISTD2RD.NominalWT_1))
    #print("BurstPressure_1 in Mpa : ","{0:.2f}".format(BurstPressure_1))
    print("BurstPressure_1 in Psi : ","{0:.2f}".format(BurstPressure_1*145.038))

    BurstPressure_2=APISTD2RD.k*(APISTD2RD.SMYS+APISTD2RD.SMUS)*math.log(APISTD2RD.NominalID_1/(APISTD2RD.NominalID_1-2*APISTD2RD.NominalWT_2))
    #print("BurstPressure_2 in Mpa : ","{0:.2f}".format(BurstPressure_2))
    print("BurstPressure_2 in Psi : ","{0:.2f}".format(BurstPressure_2*145.038))
                           
    BurstPressure_3=APISTD2RD.k*(APISTD2RD.SMYS+APISTD2RD.SMUS)*math.log(APISTD2RD.NominalID_1/(APISTD2RD.NominalID_1-2*APISTD2RD.NominalWT_3))
    #print("BurstPressure_3 in Mpa: ","{0:.2f}".format(BurstPressure_3))
    print("BurstPressure_3 in Psi: ","{0:.2f}".format(BurstPressure_3*145.038))

    BurstPressure_4=APISTD2RD.k*(APISTD2RD.SMYS+APISTD2RD.SMUS)*math.log(APISTD2RD.NominalID_1/(APISTD2RD.NominalID_1-2*APISTD2RD.NominalWT_4))
    #print("BurstPressure_4 in Mpa : ","{0:.2f}".format(BurstPressure_4))
    print("BurstPressure_4 in Psi : ","{0:.2f}".format(BurstPressure_4*145.038))
	
    BurstPressure_5=APISTD2RD.k*(APISTD2RD.SMYS+APISTD2RD.SMUS)*math.log(APISTD2RD.NominalID_1/(APISTD2RD.NominalID_1-2*APISTD2RD.NominalWT_5))
    #print("BurstPressure_5 in Mpa : ","{0:.2f}".format(BurstPressure_5))
    print("BurstPressure_5 in Psi: ","{0:.2f}".format(BurstPressure_5*145.038))
	
    BurstPressure_6=APISTD2RD.k*(APISTD2RD.SMYS+APISTD2RD.SMUS)*math.log(APISTD2RD.NominalID_1/(APISTD2RD.NominalID_1-2*APISTD2RD.NominalWT_6))
    #print("BurstPressure_6 in Mpa: ","{0:.2f}".format(BurstPressure_6))
    print("BurstPressure_6 in Psi: ","{0:.2f}".format(BurstPressure_6*145.038))


    BurstLimit_1=BurstPressure_1*APISTD2RD.DesignFactor
    #print("BurstLimit_1 in Mpa: ","{0:.2f}".format(BurstLimit_1))
    print("BurstLimit_1 in Psi: ","{0:.2f}".format(BurstLimit_1*145.038))
    
    BurstLimit_2=BurstPressure_2*APISTD2RD.DesignFactor
    #print("BurstLimit_2 in Mpa: ","{0:.2f}".format(BurstLimit_2))
    print("BurstLimit_2 in Psi: ","{0:.2f}".format(BurstLimit_2*145.038))

    BurstLimit_3=BurstPressure_3*APISTD2RD.DesignFactor
    #print("BurstLimit_3 in Mpa: ","{0:.2f}".format(BurstLimit_3))
    print("BurstLimit_3 in Psi: ","{0:.2f}".format(BurstLimit_3*145.038))

    BurstLimit_4=BurstPressure_4*APISTD2RD.DesignFactor
    #print("BurstLimit_4 in Mpa: ","{0:.2f}".format(BurstLimit_4))
    print("BurstLimit_4 in Psi: ","{0:.2f}".format(BurstLimit_4*145.038))

    BurstLimit_5=BurstPressure_5*APISTD2RD.DesignFactor
    #print("BurstLimit_5 in Mpa: ","{0:.2f}".format(BurstLimit_5))
    print("BurstLimit_5 in Psi: ","{0:.2f}".format(BurstLimit_5*145.038))

    BurstLimit_6=BurstPressure_6*APISTD2RD.DesignFactor
    #print("BurstLimit_6 in Mpa: ","{0:.2f}".format(BurstLimit_6))
    print("BurstLimit_6 in Psi: ","{0:.2f}".format(BurstLimit_6*145.038))

##    BurstLimitNoFOS=0
##
##    if BurstLimitNoFOS == 1:
##        BurstLimitNoFOS = True
##    else:
##        BurstLimitNoFOS = False
##
##    print("BurstLimitNoFOS is: {}" .format(BurstLimitNoFOS))
    
    
    x_axis = [1.0,1.2,1.4,1.6,1.8,2.0]

    #y_axis1 = [BurstPressure_1*145.038,BurstPressure_2*145.038,BurstPressure_3*145.038,BurstPressure_4*145.038,BurstPressure_5*145.038,BurstPressure_6*145.038]
    
    y_axis2 = [BurstLimit_1*145.038,BurstLimit_2*145.038,BurstLimit_3*145.038,BurstLimit_4*145.038,BurstLimit_5*145.038,BurstLimit_6*145.038]
    
    #it creates plot tittle, color and size
    plt.suptitle('BurstPressure With Thickness', fontsize=14, color='black')
    #it creates plot Sub-tittle, color and size
    plt.title('Inner Diameter=12 inch, Design_FactorFD=0.60, API STD 2RD', fontsize=10, color='blue')
    #it creates plot X-axis name, fontsize and colour
    plt.xlabel('Thickness (Inchs)', fontsize=12,  color='black') 
    #it creates plot Y-axis name, fontsize and colour             
    plt.ylabel('Burst_Pressure(Psi)', fontsize=12, color='black')
    #plotting x and y values
    #plt.plot(x_axis, y_axis1, '-b', label='BurstPressure')
    plt.plot(x_axis, y_axis2, '-r', label='BurstLimit')
    plt.legend(loc='upper left')
    plt.savefig("BurstPressure With Thickness_US_ID",dpi=800)

APISTD2RDCalculation(configParams.APISTD2RD)    
