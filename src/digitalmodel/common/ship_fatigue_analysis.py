import os
import statistics
import logging
import pandas as pd
import numpy as np

from assetutilities.common.utilities import add_cwd_to_filename
from assetutilities.common.data import ReadData
from assetutilities.common.data import SaveData
from assetutilities.common.visualizations import Visualization


class ShipFatigueAnalysis():

    def __init__(self):
        pass

    def router(self, cfg):
        if cfg['inputs']['files']['lcf']['file_type'] == 'seasam_xtract':
            self.get_fatigue_states(cfg)

    def get_fatigue_states(self, cfg):
        fatigue_states = cfg['inputs']['files']['lcf']['fatigue_states']

        for fatigue_state in fatigue_states:
            if not os.path.isdir(fatigue_state):
                cwd = os.getcwd()
                fatigue_states[0] = add_cwd_to_filename(fatigue_state, cwd)
                if not os.path.isdir(fatigue_state):
                    raise ValueError(f'Invalid directory: {fatigue_states[0]}')

        seasam_xtract_file = cfg['inputs']['files']['lcf']['file_name']
        seasam_xtract_file = os.path.join(os.path.dirname(__file__), seasam_xtract_file)
        seasam_xtract_file = ReadData(seasam_xtract_file)
        seasam_xtract_file = seasam_xtract_file.read_seasam_xtract()

    def sesam_xtract_from_file(self, seasam_xtract_file):
        seasam_xtract_file = ReadData(seasam_xtract_file)
        seasam_xtract_file = seasam_xtract_file.read_seasam_xtract()