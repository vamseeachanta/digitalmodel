import os
import math
import logging
import pandas as pd
from pathlib import Path


from assetutilities.common.file_management import FileManagement
from assetutilities.common.data import ReadData
from assetutilities.common.data import SaveData

from digitalmodel.custom.aqwa.aqwa_utilities import AqwaUtilities

fm = FileManagement()
au = AqwaUtilities()
rd = ReadData()
save_data = SaveData()

class AqwaLISFiles:

    def __init__(self):
        pass

    def router(self, cfg):
        cfg = fm.router(cfg)
        for result_item in cfg['result']:
            df = self.get_data(cfg, result_item)
            self.injectSummaryToExcel(df, result_item, cfg)

    def get_data(self, cfg, cfg_lis):
        input_files = cfg['file_management']['input_files']['LIS']
        
        df_master = pd.DataFrame()
        for input_file in input_files:
            cfg_file = {'io': input_file, 'basename': os.path.basename(input_file)}
            cfg_lis_search_cfg = cfg_lis['search_cfg']['start']
            cfg_keyword_lines = {
                'io': cfg_file['io'],
                'line': cfg_lis['search_cfg']['start'].copy()
                }

            start_keyword_lines = rd.from_ascii_file_get_line_number_containing_keywords(cfg_keyword_lines)
            start_keyword_line = start_keyword_lines[cfg_lis['search_cfg']['start']['occurrence'] -1]

            cfg_keyword_lines = {
                'io': cfg_file['io'],
                'line': cfg_lis['search_cfg']['end'].copy()
                }

            end_keyword_lines = rd.from_ascii_file_get_line_number_containing_keywords(cfg_keyword_lines)
            end_keyword_line = end_keyword_lines[cfg_lis['search_cfg']['end']['occurrence'] -1]

            cfg_data_format = {
                'io': input_file,
                'start_line': start_keyword_line,
                'end_line': end_keyword_line
            }

            data = rd.from_ascii_file_get_lines_as_string_arrays(cfg_data_format)
            cfg_refine = cfg_lis['search_cfg']['data_extraction']
            df = self.get_refine_data_using_cfg(data, cfg_refine, cfg_lis_search_cfg)
            df['input_file'] = Path(input_file).stem
            df_master = pd.concat([df_master, df], axis=0)

        return df_master

    def get_refine_data_using_cfg(self, data, cfg_refine, cfg_lis_search_cfg):
        refine_data_line_numbers = rd.get_array_rows_containing_keywords(data, cfg_refine['key_words'], cfg_refine)
        df_data = pd.DataFrame()
        for refine_data_line in refine_data_line_numbers:
            df_refine = self.get_data_for_refine_data_line_number(data, refine_data_line, cfg_refine, cfg_lis_search_cfg)
            df_data = pd.concat([df_data, df_refine], axis=0)

        return df_data
    
    def get_data_for_refine_data_line_number(self, data, refine_data_line, cfg_refine, cfg_lis_search_cfg):
        columns = self.get_header_columns(data, refine_data_line, cfg_refine)

        cfg_data_trans = cfg_refine['data']['transform']
        data_start_line = cfg_data_trans['scale']*refine_data_line + cfg_data_trans['shift'] - 1
        data_array = data[data_start_line:]
        df = pd.DataFrame(columns=columns)
        for data_line in data_array:
            data_row = data_line.split()
            if len(data_row) == len(columns):
                df.loc[len(df)] = data_row
            elif len(data_row) == len(columns) - 1:
                if 'ACC R.A.O.S-VARIATION WITH WAVE PERIOD/FREQUENCY' in cfg_refine['key_words']:
                    data_row = data_row[0:2] + [df.iloc[-1, 2]] + data_row[2:]
                    df.loc[len(df)] = data_row
                elif 'P O S I T I O N   R . A . O . S   A T   U S E R - R E Q U E S T E D' in cfg_lis_search_cfg['key_words']:
                    data_row = data_row[0:2] + [df.iloc[-1, 2]] + data_row[2:]
                    df.loc[len(df)] = data_row
            else:
                break

        return df

    def get_header_columns(self, data, refine_data_line, cfg_refine):
        if 'columns' in cfg_refine['header']:
            columns = cfg_refine['header']['columns']
        else:
            cfg_header_trans = cfg_refine['header']['transform']
            header = data[cfg_header_trans['scale']*refine_data_line + cfg_header_trans['shift'] - 1]
            columns = header.split()
        return columns

    def injectSummaryToExcel(self, df, result_item, cfg):
        if 'inject_into' in result_item and result_item[
                'inject_into']['flag']:
            inject_into_file = result_item['inject_into']['filename']
            file_name = os.path.join(cfg['Analysis']['analysis_root_folder'],
                        inject_into_file)
            if not os.path.isfile(file_name):
                raise Exception(f"Inject Into File {file_name} not found for writing summary data")

            sheetname = result_item['inject_into']['sheetname']

            cfg_save_to_existing_workbook = {'template_file_name': file_name, 'sheetname': sheetname, 'saved_file_name': file_name, 'if_sheet_exists': 'replace', 'df': df}
            save_data.df_to_sheet_in_existing_workbook(cfg_save_to_existing_workbook)
