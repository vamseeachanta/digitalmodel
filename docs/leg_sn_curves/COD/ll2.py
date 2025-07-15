from FatigueBasiccurve import*
linearslopeData = fatigueRawdata.loc[:, 'Curve Type':'m1']
pd.DataFrame(linearslopeData)
highStressRangevalues = [900,500,350,300,250,200]
highStresscal = linearslopeData["Log a1"]
slope = linearslopeData["m1"]
y = []

for data,value in enumerate(highStressRangevalues):
    y.append(highStresscal/value**(slope))

##linearslopeData["High Stress Range 2 (Mpa)"] = [data for value in y]

##z = (list(map(lambda line: list(map(lambda x: float(x),line.str.extract().astype(np.float))), y)))       
   
        
        
                                            
##z = map(linearslopeData,y)
##linearslopeData["High Stress Range 2 (Mpa)"] = x for values in y
    
