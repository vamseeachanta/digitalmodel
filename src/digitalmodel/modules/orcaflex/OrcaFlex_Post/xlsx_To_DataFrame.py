def xlsx_To_DataFrame(data):
    """
    https://stackoverflow.com/questions/38056233/python-search-excel-sheet-for-specific-strings-in-a-column-and-extract-those-ro/38056526
    https://www.tutorialspoint.com/How-to-check-if-multiple-strings-exist-in-another-string-in-Python
    """
    import pandas as pd
    import xlrd
    ReadData = []
    wb = xlrd.open_workbook(data['FileName'])

    sh = wb.sheet_by_name(data['SheetName'])
    KeyWordRowNumber = WorkSheetRowNumberWithText(sh, data["KeyWords"])

    if KeyWordRowNumber == None:
        raise Exception("Error in keyword provided for search criteria")

    StartRowNumber = KeyWordRowNumber + data['RowsToSkip']
    EndRowNumber = KeyWordRowNumber + data['RowsToSkip'] + data['RowsToRead']
    if EndRowNumber > sh.nrows:
        EndRowNumber = sh.nrows
    for rownum in range(StartRowNumber, EndRowNumber):
        ReadData.append((sh.row_values(rownum)))

    df = pd.DataFrame(ReadData)

    # Assign columns
    if data['Columns'] != None:
        if len(data['Columns']) <= len(df.columns):
            AdditionalColumns = list(range(len(data['Columns']), len(df.columns)))
            df.columns = data['Columns'] + AdditionalColumns
        else:
            df.columns = data['Columns'][0:len(df.columns)]

    return df

"""
Objective: To obtain the row number of the worksheet with specified keyword(s)
"""
def WorkSheetRowNumberWithText(sh, KeyWordArray):
    rownum = None
    for rownum in range(0, sh.nrows):
        if any(keyword in sh.row_values(rownum) for keyword in KeyWordArray):
            return rownum
            break

if __name__ == '__main__':
    FileName = 'K:\\0173 KM Extreme\\SLWR\\Fatigue\\Test.xlsx'
    Columns = ['Arc Length', 'S-N Curve', 'Theta', 'Overall Damage', 'Life (years)']
    CustomData = {"FileName": FileName,
                    "SheetName": "Sheet1",
                    "KeyWords": ['Arc Length'],
                    "RowsToSkip": 2,
                    "RowsToRead": 1000,
                    "Columns": Columns}
    df = xlsx_To_DataFrame(CustomData)
    print(df)