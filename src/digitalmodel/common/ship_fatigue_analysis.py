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

        state_0_files = self.get_files_in_directory(fatigue_states[0])
        state_1_files = self.get_files_in_directory(fatigue_states[1])

        fatigue_state_pairs = self.get_fatigue_state_pairs(state_0_files, state_1_files)
                    
        seasam_xtract_file = cfg['inputs']['files']['lcf']['file_name']
        seasam_xtract_file = os.path.join(os.path.dirname(__file__), seasam_xtract_file)
        seasam_xtract_file = ReadData(seasam_xtract_file)
        seasam_xtract_file = seasam_xtract_file.read_seasam_xtract()


    def get_fatigue_state_pairs(self, state_0_files, state_1_files):
        fatigue_state_pairs = []
        for basename_0 in state_0_files['basenames']:
            basename_index_1 in state_1_files['basenames']:
                if file_0 == file_1:
                    fatigue_state_pairs.append([file_0, file_1])
        return fatigue_state_pairs

    def get_files_in_directory(self, directory):
        from assetutilities.engine import engine
        cfg_file_management = {'basename': 'file_management', 'files': {'files_in_current_directory': {'flag': False, 'directory': directory}, 'extension': 'txt'}}
        cfg_result = engine(inputfile=None, cfg=cfg_file_management)
        files = cfg_result['file_management']

        return files