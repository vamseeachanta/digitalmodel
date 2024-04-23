import os
import math
import logging
import pandas as pd

from assetutilities.common.utilities import is_file_valid_func
from assetutilities.common.update_deep import update_deep_dictionary
from assetutilities.common.data import ReadData

from digitalmodel.custom.aqwa_utilities import AqwaUtilities

au = AqwaUtilities()
rd = ReadData()

class AqwaLISFiles:

    def __init__(self):
        pass

    def router(self, cfg):
        au.file_management(cfg)
        if cfg['result']['moorings']['flag']:
            cfg_lis = cfg['result']['moorings']
            df = self.get_data(cfg, cfg_lis)

    def get_data(self, cfg, cfg_lis):
        input_files = cfg['file_management']['input_files']['LIS']
        
        for input_file in input_files:
            cfg_file = {'io': input_file, 'basename': os.path.basename(input_file)}
            cfg_keyword_lines = {
                'io': cfg_file['io'],
                'line': {
                    'key_words': [cfg_lis['search_cfg']['start']['keyword']],
                    'transform': {
                        'scale': 1,
                        'shift': 0
                    }
                }
            }

            start_keyword_lines = rd.from_ascii_file_get_line_number_containing_keywords(cfg_keyword_lines)
            # start_keyword_line = start_keyword_lines[cfg_lis['search_cfg']['start']['occurrence'] -1]

            cfg_data_format = {
                'io': input_file,
                'start_line': start_keyword_lines,
                # 'end_line': None
            }

            data = rd.from_ascii_file_get_lines_as_string_arrays(cfg_data_format)
            df_master = pd.DataFrame()
            df_file = self.get_data_as_dataframe(data, cfg_lis, cfg_file)
            df_master = pd.concat([df_master, df_file], axis=1)
            

        return df_master


    def get_data_as_dataframe(self, raw_data, cfg_lis, cfg_file):
        keyword = cfg_lis['search_cfg']['data_extraction']['keyword']
        raw_data = [x for x in raw_data if keyword in x]
        
        cfg_file_keys = list(cfg_file.keys())
        data_columns = cfg_file_keys + [x.split()[0] for x in raw_data]
        cfg_file_values = [cfg_file[cfg_file_keys[0]], cfg_file[cfg_file_keys[1]]]

        data = cfg_file_values + [float(x.split()[4]) for x in raw_data]
        df = pd.DataFrame(columns=data_columns)
        df.loc[len(df)] = data
        return df
