from FatigueBasiccurve import*
linearslopeData = fatigueRawdata.loc[:, 'Curve Type':'m1']
pd.DataFrame(linearslopeData)

# high stress range calulation at 900
linearslopeData["High Stress Range 2 (Mpa)"] =  900
cal = (linearslopeData["High Stress Range 2 (Mpa)"])**linearslopeData["m1"]
highStressRange = linearslopeData["Log a1"]/cal
linearslopeData["N - High Stress Range 2"] = highStressRange

# high stress range calulation at 500
linearslopeData["High Stress Range 2 (Mpa)1"] =  500
cal1 = (linearslopeData["High Stress Range 2 (Mpa)1"])**linearslopeData["m1"]
highStressRange1 = linearslopeData["Log a1"]/cal1
linearslopeData["N - High Stress Range 21"] = highStressRange1

# high stress range calulation at 350
linearslopeData["High Stress Range 2 (Mpa)2"] =  350
cal2 = (linearslopeData["High Stress Range 2 (Mpa)2"])**linearslopeData["m1"]
highStressRange2 = linearslopeData["Log a1"]/cal2
linearslopeData["N - High Stress Range 22"] = highStressRange2

# high stress range calulation at 300
linearslopeData["High Stress Range 2 (Mpa)3"] =  300
cal3 = (linearslopeData["High Stress Range 2 (Mpa)3"])**linearslopeData["m1"]
highStressRange3 = linearslopeData["Log a1"]/cal3
linearslopeData["N - High Stress Range 23"] = highStressRange3

# high stress range calulation at 250
linearslopeData["High Stress Range 2 (Mpa)4"] =  250
cal4 = (linearslopeData["High Stress Range 2 (Mpa)4"])**linearslopeData["m1"]
highStressRange4 = linearslopeData["Log a1"]/cal4
linearslopeData["N - High Stress Range 24"] = highStressRange4

# high stress range calulation at 200
linearslopeData["High Stress Range 2 (Mpa)5"] =  200
cal5 = (linearslopeData["High Stress Range 2 (Mpa)5"])**linearslopeData["m1"]
highStressRange5 = linearslopeData["Log a1"]/cal5
linearslopeData["N - High Stress Range 25"] = highStressRange5
linearslopeData.to_excel("C:\\Users\\AceEngineer-04\\Dropbox\\0119 Programming\\017 Fatigue Curves\\Cal\\FatigueLinearslopeCal.xlsx")

