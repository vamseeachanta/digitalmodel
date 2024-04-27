import os
import math
import pandas as pd

import logging
from assetutilities.common.utilities import is_file_valid_func
from assetutilities.common.update_deep import update_deep_dictionary

from digitalmodel.custom.aqwa_dat_files import AqwaDATFiles

dat_files = AqwaDATFiles()

class AqwaPreProcess:

    def __init__(self):
        pass

    def post_process_router(self, cfg):

# Check for license
        cfg = self.get_cfg_with_master_data(cfg)
# Code for vissualizations

        if cfg['type']['preprocess']:
            raise NotImplementedError("Preprocess not implemented")

        if cfg['type']['analysis']:
            raise NotImplementedError("Preprocess not implemented")

        if cfg['type']['results']:
            if cfg['result_method'] == 'lis':
                lis_files.router(cfg)
            elif cfg['result_method'] == 'aqwareader':
                ar.router(cfg)

        else:
            logging.info("No option to run specified ... End Run.")

        return cfg

    def get_cfg_with_master_data(self, cfg):
        if 'summary_settings_master' in cfg:
            summary_settings_master = cfg['summary_settings_master'].copy()
            summary_settings = cfg['summary_settings']

            for group_index in range(0, len(summary_settings['groups'])):
                group = summary_settings['groups'][group_index].copy()
                group = update_deep_dictionary(summary_settings_master['groups'][0], group)
                summary_settings['groups'][group_index] = group.copy()

        return cfg
