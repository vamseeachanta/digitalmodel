import os
import math
import logging
import pandas as pd
from pathlib import Path

# Third party imports

from assetutilities.common.file_management import FileManagement
from assetutilities.common.data import ReadData
from assetutilities.common.data import SaveData

# Reader imports
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
        csv_filename_array = []
        for result_item in cfg['result']:
            df, input_file = self.get_data(cfg, result_item)

            df = self.get_sorted_dataframe(df, result_item)
            df = self.get_output_transformed_data(df, cfg)

            csv_filename = self.save_to_csv(df, result_item, cfg, input_file)
            csv_filename_array.append(csv_filename)
            self.injectSummaryToExcel(df, result_item, cfg)

        cfg[cfg['basename']] = {'csv_filename': csv_filename_array}

        return cfg

    def get_data(self, cfg, cfg_lis):
        input_files = cfg['file_management']['input_files']['LIS']
        filename_pattern = ''
        df_master = pd.DataFrame()
        for input_file in input_files:
            filename_pattern = Path(input_file).stem

            try:
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
            except Exception as e:
                logging.error(f"Error in processing {input_file}: {e}")


        return df_master, filename_pattern

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
                df.loc[len(df)] = [float(item) for item in data_row]
            elif len(data_row) == len(columns) - 1:
                if 'ACC R.A.O.S-VARIATION WITH WAVE PERIOD/FREQUENCY' in cfg_refine['key_words']:
                    data_row = data_row[0:2] + [df.iloc[-1, 2]] + data_row[2:]
                    df.loc[len(df)] = [float(item) for item in data_row]
            elif len(data_row) == len(columns) - 2:
                if 'P O S I T I O N   R . A . O . S   A T   U S E R - R E Q U E S T E D' in cfg_lis_search_cfg['key_words']:
                    try:
                        float(data_row[0])
                        data_row = [df.iloc[-1, 0]] + [df.iloc[-1, 1]] + data_row
                        df.loc[len(df)] = [float(item) for item in data_row]
                    except ValueError:
                        pass
            elif len(data_row) == 0:
                if 'P O S I T I O N   R . A . O . S   A T   U S E R - R E Q U E S T E D' in cfg_lis_search_cfg['key_words']:
                    pass
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

    def get_sorted_dataframe(self, df, result_item):
        sort_cfg = result_item['search_cfg']['data_extraction'].get('sort', None)
        if sort_cfg is not None and sort_cfg['flag']:
            df.sort_values(by=sort_cfg['columns'], inplace=True)

        return df

    def get_output_transformed_data(self, df, cfg):
        ot_cfg = cfg.get('output_transformation', None)
        if ot_cfg is not None and ot_cfg['flag']:
            frequency_column = ot_cfg['input']['columns']['frequency']
            transform_data_key = 'rao_velocity'
            df = self.transform_by_data_key(df, ot_cfg, frequency_column, transform_data_key)
            transform_data_key = 'rao_acceleration'
            df = self.transform_by_data_key(df, ot_cfg, frequency_column, transform_data_key)

        return df

    def transform_by_data_key(self, df, ot_cfg, frequency_column, transform_data_key):
        if ot_cfg['output'][transform_data_key]['flag']:
            input_columns = ot_cfg['input']['columns']['transform']
            for column_idx in range(0, len(input_columns)):
                input_column = input_columns[column_idx]
                output_column = ot_cfg['output'][transform_data_key]['columns'][column_idx]
                if transform_data_key == 'rao_velocity':
                    df[output_column] = df.apply(lambda row: round((float(row[input_column])*float(row[frequency_column])), 4), axis= 1)
                elif transform_data_key == 'rao_acceleration':
                    df[output_column] = df.apply(lambda row: round((float(row[input_column])*float(row[frequency_column])**2), 4), axis= 1)

            return df

    def save_to_csv(self, df, result_item, cfg, input_file):
        save_csv = result_item.get('save_csv', True)
        if save_csv:
            sheetname = result_item['inject_into']['sheetname']
            csv_filename = os.path.join(cfg['Analysis']['result_folder'], input_file +'_' + sheetname + '.csv')
            df.to_csv(csv_filename, index=False, header=True)

        return csv_filename

    def injectSummaryToExcel(self, df, result_item, cfg):
        if 'inject_into' in result_item and result_item[
                'inject_into']['flag']:
            inject_into_file = result_item['inject_into']['filename']
            file_name = os.path.join(cfg['Analysis']['analysis_root_folder'],
                        inject_into_file)
            if not os.path.isfile(file_name):
                raise Exception(f"Inject Into File {file_name} not found for writing summary data")

            sheetname = result_item['inject_into']['sheetname']

            cfg_save_to_existing_workbook = {'template_file_name': file_name, 'sheetname': sheetname, 'saved_file_name': file_name, 'if_sheet_exists': 'replace', 'df': df, 'index': False}
            save_data.df_to_sheet_in_existing_workbook(cfg_save_to_existing_workbook)
