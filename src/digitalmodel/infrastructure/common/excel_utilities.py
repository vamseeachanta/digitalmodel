class ReadFromExcel():

    def from_xlsx(self, cfg, file_index=0):
        cfg_template_single_xlsx = {'io': 'data_manager/data/sample_data.xlsx'}
        import pandas as pd

        if cfg.__contains__('files'):
            cfg_temp = cfg['files']['from_xlsx'][file_index]
        else:
            cfg_temp = cfg

        if cfg_temp.__contains__('sheet_name'):
            result = pd.read_excel(['io'],
                                   sheet_name=cfg_temp['sheet_name'],
                                   skiprows=cfg_temp['skiprows'],
                                   skipfooter=cfg_temp['skipfooter'],
                                   index_col=cfg_temp['index_col'])
        else:
            xls = pd.ExcelFile(cfg_temp['io'])
            result = {}
            for sheet_name in xls.sheet_names:
                result[sheet_name] = xls.parse(sheet_name)

        return result

    def from_xlsx_get_line_number_containing_keyword(self, cfg):
        """
        https://stackoverflow.com/questions/38056233/python-search-excel-sheet-for-specific-strings-in-a-column-and-extract-those-ro/38056526
        https://www.tutorialspoint.com/How-to-check-if-multiple-strings-exist-in-another-string-in-Python
        """
        import xlrd
        wb = xlrd.open_workbook(cfg['io'])

        sh = wb.sheet_by_name(cfg['sheet_name'])
        keyword_row_number = self.from_xlsx_get_WorkSheetRowNumberWithText(sh, cfg["key_words"])
        return keyword_row_number

    def xlsx_to_df_by_keyword_search(self, cfg):
        import pandas as pd
        cfg.update(cfg['start_row'])
        row_number_with_keyword = self.from_xlsx_get_line_number_containing_keyword(cfg)
        start_row = cfg['start_row']['transform']['scale'] * row_number_with_keyword + \
                    cfg['start_row']['transform']['shift']
        cfg.update(cfg['end_row'])
        row_number_with_keyword = self.from_xlsx_get_line_number_containing_keyword(cfg)
        end_row = cfg['end_row']['transform']['scale'] * row_number_with_keyword + \
                  cfg['end_row']['transform']['shift']
        number_of_rows = end_row - start_row + 1
        df = pd.read_excel(io=cfg['io'], sheet_name=cfg['sheet_name'], skiprows=start_row - 1, nrows=number_of_rows)
        if cfg.__contains__('column'):
            if cfg['column'].__contains__('drop_unwanted_columns'):
                dropped_column_array = [column for column in range(len(cfg['column']['names']), len(df.columns))]
                df = self.from_df_delete_unwanted_columns(df, dropped_column_array)
            if cfg['column'].__contains__('names'):
                df.columns = cfg['column']['names']

        return df

    def superseded_xlsx_to_df_by_keyword_search(self, data):
        """
        https://stackoverflow.com/questions/38056233/python-search-excel-sheet-for-specific-strings-in-a-column-and-extract-those-ro/38056526
        https://www.tutorialspoint.com/How-to-check-if-multiple-strings-exist-in-another-string-in-Python
        """
        import pandas as pd
        import xlrd
        ReadData = []
        wb = xlrd.open_workbook(data['FileName'])

        sh = wb.sheet_by_name(data['SheetName'])
        KeyWordRowNumber = self.from_xlsx_get_WorkSheetRowNumberWithText(sh, data["KeyWords"])

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
            if data['Columns'] == '1st Row':
                df.columns = df.iloc[0]
                df.drop([0], inplace=True)
                df.reset_index(inplace=True)
            elif len(data['Columns']) <= len(df.columns):
                AdditionalColumns = list(range(len(data['Columns']), len(df.columns)))
                df.columns = data['Columns'] + AdditionalColumns
            else:
                df.columns = data['Columns'][0:len(df.columns)]

        return df

    def from_xlsx_get_WorkSheetRowNumberWithText(self, sh, KeyWordArray):
        """
        Objective: To obtain the row number of the worksheet with specified keyword(s)
        """
        rownum = None
        for rownum in range(0, sh.nrows):
            if any(keyword in sh.row_values(rownum) for keyword in KeyWordArray):
                return rownum
                break
