import xlsxwriter
import glob

workbook = xlsxwriter.Workbook('Results.xlsx')
worksheet = workbook.add_worksheet('Code check results')

bold = workbook.add_format({'bold': True})
worksheet.write('A1', 'DNV OS F101 Load Controlled Maximum', bold)
worksheet.write('A2', 'Load Case:', bold)
worksheet.write('B2', 'Condition a', bold)
worksheet.write('C2', 'Condition b', bold)
worksheet.set_column(0, 2, 40)
row = 3

for fileName in glob.glob('*.txt'):
    col = 0
    with open(fileName, 'r') as f:
        for line in f:
            worksheet.write(row, col, line)
            col += 1
    row += 1
    
workbook.close()