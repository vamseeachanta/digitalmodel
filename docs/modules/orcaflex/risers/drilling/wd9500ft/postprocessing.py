import OrcFxAPI as orca
import matplotlib.pyplot as pyplot
import pandas as pd
import numpy as np
whArray = np.zeros(shape=(143,1)).ravel()
wtArray = np.zeros(shape=(143,1)).ravel()
offsetArray = np.zeros(shape=(143,1)).ravel()
bmArray = np.zeros(shape=(143,1)).ravel()
vmArray = np.zeros(shape=(143,1)).ravel()
ufjArray = np.zeros(shape=(143,1)).ravel()
lfjArray = np.zeros(shape=(143,1)).ravel()
bendStiffness = 2
waterDepth = 2895.6
resultsList = pd.DataFrame(
    {'Wave Height': whArray,
    'Wave Period': wtArray,
    'Vesel Offset%': offsetArray,
    'Bending Moment': bmArray,
    'von Mises Stress': vmArray,
    'UFJ Angle': ufjArray,
    'LFJ Angle': lfjArray
    })
writer = pd.ExcelWriter('Results.xlsx', engine='xlsxwriter')
resultsList.to_excel(writer, sheet_name='Sheet1')
workbook  = writer.book
worksheet = writer.sheets['Sheet1']
workbook.close()
