import os
import math
import logging
import pandas as pd

from assetutilities.common.file_management import FileManagement
from assetutilities.common.data import ReadData
from assetutilities.common.data import SaveData

from digitalmodel.custom.aqwa_utilities import AqwaUtilities

fm = FileManagement()
au = AqwaUtilities()
rd = ReadData()
save_data = SaveData()

class AqwaDATFiles:

    def __init__(self):
        pass

    def router(self, cfg):
        self.prepare_data_category(cfg)

    def prepare_data_category(self, cfg):
        data_category = cfg['data_category']
        return data_category
    
    def get_header(self, data_category):
        data_category = self.prepare_data_category(cfg)
        header = cfg['header'][data_category]
        return header
    
    