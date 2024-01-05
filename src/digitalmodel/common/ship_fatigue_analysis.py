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

        
        fatigue_state_pairs = self.get_stress_ranges(cfg, fatigue_state_pairs)

    def get_stress_ranges(self, cfg, fatigue_state_pairs):
        for fatigue_state_pair in fatigue_state_pairs:
            fatigue_state_pair = self.read_files_and_get_stress_range(fatigue_state_pair)


        return fatigue_state_pairs
    
    def read_files_and_get_stress_range(self, fatigue_state_pair):
        stress_range = []
        for state in fatigue_state_pair:
            seasam_xtract_file = ReadData(fatigue_state_pair[state])
            seasam_xtract_file = seasam_xtract_file.read_seasam_xtract()
            stress_range.append(seasam_xtract_file['stress_range'])
        stress_range = self.get_stress_range(stress_range)
        fatigue_state_pair['stress_range'] = stress_range

    def get_fatigue_state_pairs(self, state_0_files, state_1_files):
        fatigue_state_pairs = []
        for state_0_index in range(0, len(state_0_files['basenames'])):
            basename_0 = state_0_files['basenames'][state_0_index]
            basename_index_1= state_1_files['basenames'].index(basename_0)
            fatigue_state_pair = {'state_0': state_0_files['files'][state_0_index], 'state_1': state_1_files['files'][basename_index_1]}
            fatigue_state_pairs.append(fatigue_state_pair)
            
        return fatigue_state_pairs

    def get_files_in_directory(self, directory):
        from assetutilities.engine import engine
        cfg_file_management = {'basename': 'file_management', 'files': {'files_in_current_directory': {'flag': False, 'directory': directory}, 'extension': 'txt'}}
        cfg_result = engine(inputfile=None, cfg=cfg_file_management)
        files = cfg_result['file_management']

        return files