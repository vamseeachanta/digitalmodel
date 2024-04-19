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
            self.read_lis_files(cfg, cfg_lis)

    def read_lis_files(self, cfg, cfg_lis):
        input_files = cfg['file_management']['input_files']['lis']
        cfg_keyword_lines = {
            'io': input_files[0],
            'line': {
                'key_words': [cfg_lis['search_term']],
                'transform': {
                    'scale': 1,
                    'shift': 0
                }
            }
        }

        keyword_lines = rd.from_ascii_file_get_line_number_containing_keywords(cfg_keyword_lines)

        lis_files = cfg['results']['lis_files']
        for lis_file in lis_files:
            if not is_file_valid_func(lis_file):
                raise ValueError(f"File {lis_file} is not valid")

            df = pd.read_csv(lis_file, sep='\t', skiprows=1)
            print(df.head())
