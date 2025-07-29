import math
import matplotlib.pyplot as plt
from dataManager.loadConfiguration import loadConfiguration

configParams = loadConfiguration('dataManager//APISTD2RD.ini')

def APISTD2RDCalculation(APISTD2RD):

    #Collapse Pressure Check

    #Yield collapse pressure Py

    YieldCollapsePressure_1= 2*APISTD2RD.SMYS*(APISTD2RD.NominalWT_1/APISTD2RD.NominalOD_1)
    YieldCollapsePressure_2= 2*APISTD2RD.SMYS*(APISTD2RD.NominalWT_2/APISTD2RD.NominalOD_1)
    YieldCollapsePressure_3= 2*APISTD2RD.SMYS*(APISTD2RD.NominalWT_3/APISTD2RD.NominalOD_1)
    YieldCollapsePressure_4= 2*APISTD2RD.SMYS*(APISTD2RD.NominalWT_4/APISTD2RD.NominalOD_1)
    YieldCollapsePressure_5= 2*APISTD2RD.SMYS*(APISTD2RD.NominalWT_5/APISTD2RD.NominalOD_1)
    YieldCollapsePressure_6= 2*APISTD2RD.SMYS*(APISTD2RD.NominalWT_6/APISTD2RD.NominalOD_1)
    YieldCollapsePressure_7= 2*APISTD2RD.SMYS*(APISTD2RD.NominalWT_7/APISTD2RD.NominalOD_1)
   
    #print("YieldCollapsePressure : ","{0:.2f}".format(YieldCollapsePressure))

    #elastic collapse pressure Pel
    ElasticCollapsePressure_1= 2*APISTD2RD.E*(APISTD2RD.NominalWT_1/APISTD2RD.NominalOD_1)**3/(1-APISTD2RD.Poissionsratio**2)
    ElasticCollapsePressure_2= 2*APISTD2RD.E*(APISTD2RD.NominalWT_2/APISTD2RD.NominalOD_1)**3/(1-APISTD2RD.Poissionsratio**2)
    ElasticCollapsePressure_3= 2*APISTD2RD.E*(APISTD2RD.NominalWT_3/APISTD2RD.NominalOD_1)**3/(1-APISTD2RD.Poissionsratio**2)
    ElasticCollapsePressure_4= 2*APISTD2RD.E*(APISTD2RD.NominalWT_4/APISTD2RD.NominalOD_1)**3/(1-APISTD2RD.Poissionsratio**2)
    ElasticCollapsePressure_5= 2*APISTD2RD.E*(APISTD2RD.NominalWT_5/APISTD2RD.NominalOD_1)**3/(1-APISTD2RD.Poissionsratio**2)
    ElasticCollapsePressure_6= 2*APISTD2RD.E*(APISTD2RD.NominalWT_6/APISTD2RD.NominalOD_1)**3/(1-APISTD2RD.Poissionsratio**2)
    ElasticCollapsePressure_7= 2*APISTD2RD.E*(APISTD2RD.NominalWT_7/APISTD2RD.NominalOD_1)**3/(1-APISTD2RD.Poissionsratio**2)
    
    #print("ElasticCollapsePressure : ","{0:.2f}".format(ElasticCollapsePressure))
   
    #plastic collapse pressure Pp

    PlasticCollapsePressure_1=2*APISTD2RD.NominalWT_1/APISTD2RD.NominalOD_1*APISTD2RD.SMYS*APISTD2RD.alphafab
    PlasticCollapsePressure_2=2*APISTD2RD.NominalWT_2/APISTD2RD.NominalOD_1*APISTD2RD.SMYS*APISTD2RD.alphafab
    PlasticCollapsePressure_3=2*APISTD2RD.NominalWT_3/APISTD2RD.NominalOD_1*APISTD2RD.SMYS*APISTD2RD.alphafab
    PlasticCollapsePressure_4=2*APISTD2RD.NominalWT_4/APISTD2RD.NominalOD_1*APISTD2RD.SMYS*APISTD2RD.alphafab
    PlasticCollapsePressure_5=2*APISTD2RD.NominalWT_5/APISTD2RD.NominalOD_1*APISTD2RD.SMYS*APISTD2RD.alphafab
    PlasticCollapsePressure_6=2*APISTD2RD.NominalWT_6/APISTD2RD.NominalOD_1*APISTD2RD.SMYS*APISTD2RD.alphafab
    PlasticCollapsePressure_7=2*APISTD2RD.NominalWT_7/APISTD2RD.NominalOD_1*APISTD2RD.SMYS*APISTD2RD.alphafab
            
    #print("PlasticCollapsePressure : ","{0:.2f}".format(PlasticCollapsePressure))
    
    #collapse pressure Pc

    CollapsePressure_1=YieldCollapsePressure_1*ElasticCollapsePressure_1/math.sqrt(YieldCollapsePressure_1**2+ElasticCollapsePressure_1**2)
    print("CollapsePressure_1 : ","{0:.2f}".format(CollapsePressure_1))
    CollapsePressure_2=YieldCollapsePressure_2*ElasticCollapsePressure_2/math.sqrt(YieldCollapsePressure_2**2+ElasticCollapsePressure_2**2)
    print("CollapsePressure_2 : ","{0:.2f}".format(CollapsePressure_2))
    CollapsePressure_3=YieldCollapsePressure_3*ElasticCollapsePressure_3/math.sqrt(YieldCollapsePressure_3**2+ElasticCollapsePressure_3**2)
    print("CollapsePressure_3 : ","{0:.2f}".format(CollapsePressure_3))
    CollapsePressure_4=YieldCollapsePressure_4*ElasticCollapsePressure_4/math.sqrt(YieldCollapsePressure_4**2+ElasticCollapsePressure_4**2)
    print("CollapsePressure_4 : ","{0:.2f}".format(CollapsePressure_4))
    CollapsePressure_5=YieldCollapsePressure_5*ElasticCollapsePressure_5/math.sqrt(YieldCollapsePressure_5**2+ElasticCollapsePressure_5**2)
    print("CollapsePressure_5 : ","{0:.2f}".format(CollapsePressure_5))
    CollapsePressure_6=YieldCollapsePressure_6*ElasticCollapsePressure_6/math.sqrt(YieldCollapsePressure_6**2+ElasticCollapsePressure_6**2)
    print("CollapsePressure_6 : ","{0:.2f}".format(CollapsePressure_6))
    CollapsePressure_7=YieldCollapsePressure_7*ElasticCollapsePressure_7/math.sqrt(YieldCollapsePressure_7**2+ElasticCollapsePressure_7**2)
    print("CollapsePressure_7 : ","{0:.2f}".format(CollapsePressure_7))

    #collapse pressure may alternatively be calculated as a function of the elastic capacity, plastic capacity and the ovality of the pipe as

    LHS=(CollapsePressure_1-ElasticCollapsePressure_1)*(CollapsePressure_1**2-PlasticCollapsePressure_1**2)
    RHS=CollapsePressure_1*ElasticCollapsePressure_1*PlasticCollapsePressure_1*2*APISTD2RD.ovality*(APISTD2RD.NominalOD_1/APISTD2RD.NominalWT_1)

    if LHS == RHS:
        CollapsePressureCheckPass = True
    else:
        CollapsePressureCheckPass = False

    print("CollapsePressureCheckPass is: {}" .format(CollapsePressureCheckPass))


    x_axis = [0.065,0.109,0.154,0.218,0.276,0.344,0.436]
    
    y_axis1 = [CollapsePressure_1*145.038,CollapsePressure_2*145.038,CollapsePressure_3*145.038,CollapsePressure_4*145.038,CollapsePressure_5*145.038,CollapsePressure_6*145.038,CollapsePressure_7*145.038]
    y_axis2 = [904.60,3369.36,6286.10,9869.82,12797.68,16098.32,20486.39]
    #[4979.88,4979.88,4979.88,4979.88,4979.88,4979.88,4979.88]

    
    #it creates plot tittle, color and size
    plt.suptitle('CollapsePressureWithThickness', fontsize=14,color='black')
    plt.title('Dia=2.375 Inch, Design_FactorFD=0.70, APISTD2RD', fontsize=12, color='blue')
    #it creates plot X-axis name, fontsize and colour
    plt.xlabel('Thickness (Inchs)', fontsize=12, color='black') 
    #it creates plot Y-axis name, fontsize and colour             
    plt.ylabel('CollapsePressure(Psi)', fontsize=12, color='black')
    #plotting x and y values
    plt.plot(x_axis, y_axis1, '-b', label='CollapsePressure')
    plt.plot(x_axis, y_axis2, '-r', label='DesignPressure')
    plt.legend(loc='upper left')
    plt.savefig("CollapsePressureWithThickness_US.png",dpi=800)
  
    
APISTD2RDCalculation(configParams.APISTD2RD)    
