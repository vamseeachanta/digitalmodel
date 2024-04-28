import os
import math
import logging
import pandas as pd

from assetutilities.common.file_management import FileManagement
from assetutilities.common.data import ReadData
from assetutilities.common.data import SaveData

from digitalmodel.custom.aqwa.aqwa_utilities import AqwaUtilities

fm = FileManagement()
au = AqwaUtilities()
rd = ReadData()
save_data = SaveData()

class AqwaDATFiles:

    def __init__(self):
        pass

    def router(self, cfg):
        self.process_all_data_categories(cfg)

    def process_all_data_categories(self, cfg):
        for dc_cfg in cfg['inputs']:
            self.prepare_data_category(dc_cfg, cfg)

    def prepare_data_category(self, dc_cfg, cfg):
        data_category = cfg['data_category']
        return data_category

    def get_header(self, data_category,cfg_header):
        data_category = self.prepare_data_category(cfg)
        header = cfg['header'][data_category]
        return header

    def get_body(self, data_category,cfg_body):
        data_category = self.prepare_data_category(cfg)
        header = cfg['header'][data_category]
        return header

    def get_footer(self, data_category,cfg_footer):
        data_category = self.prepare_data_category(cfg)
        header = cfg['header'][data_category]
        return header

