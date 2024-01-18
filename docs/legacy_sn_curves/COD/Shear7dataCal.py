import FatigueBasiccurve
from FatigueBasiccurve import*
fatigueBasicurve = pd.read_excel("C:\\Users\\AceEngineer-04\\Dropbox\\0119 Programming\\017 Fatigue Curves\\Cal\\FatiguebasicurveCal.xlsx")
shear7Data = fatigueBasicurve.loc[:, 'Curve Type':'S0 (Mpa)']
pd.DataFrame(shear7Data)

##high stress range calculation for shear7data
shear7Data["High Stress Range (Mpa)"] = 1000
cal = shear7Data["High Stress Range (Mpa)"]**(slope)
highStressRange = fatigueConstant1 /cal
shear7Data["N - High Stress Range"] = highStressRange


##low stress range calculation for shear7 data
shear7Data["Low Stress Range(Mpa)"] = 1
shear7Data["Low Stress Range(Mpa)"].iloc[30:45] = np.arange(2,17)
lowStressRange = shear7Data["Low Stress Range(Mpa)"]

def dat(refernceConstantCycleandStressdata,y,v,z,a,c,d,f,e,h):
    for Numofslopes in refernceConstantCycleandStressdata:
        if Numofslopes == 1:
            return(z/v**(y))
        elif Numofslopes == 2:
            return(c/v**(a))
        elif Numofslopes == 3:
            return(f/v**(d))
        elif Numofslopes == 4:
            return(h/v**(e))

fatigueData = dat(refernceConstantCycleandStressdata,slope,lowStressRange,fatigueConstant1,slope2,fatigue,slope3,fatigue1,slope4,fatigue2)
shear7Data["N - Low Stress Range"] = fatigueData

shear7Data.to_excel("C:\\Users\\AceEngineer-04\\Dropbox\\0119 Programming\\017 Fatigue Curves\\Cal\\shear7DataCal.xlsx")
##                   
##

    

