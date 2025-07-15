import pandas as pd
fatigueData = pd.read_excel("C:\\Users\\AceEngineer-04\\Dropbox\\0119 Programming\\017 Fatigue Curves\\Ref\\SN Curve Definitions.xlsx",sheetname= 'Raw Data')
fatigueRawdata = fatigueData.rename(columns=fatigueData.iloc[23]).drop(fatigueData.index[:23]).reset_index(drop = True).drop(fatigueData.index[:1]).drop(['Check Flag Status', 'Lookup Index','Log s1'],axis=1)
fatigueRawdata.to_excel("C:\\Users\\AceEngineer-04\\Dropbox\\0119 Programming\\017 Fatigue Curves\\Data\\FatigueRawdata.xlsx")
