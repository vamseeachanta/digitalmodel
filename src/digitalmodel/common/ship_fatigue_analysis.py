import os
import logging
import statistics
import logging
import pandas as pd
import numpy as np

from assetutilities.common.utilities import add_cwd_to_filename
from assetutilities.common.data import ReadData
from assetutilities.common.data import SaveData
from assetutilities.common.visualizations import Visualization
read_data = ReadData()

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
            fatigue_state_pair = self.get_stress_data(cfg, fatigue_state_pair)

        return fatigue_state_pairs


    def get_stress_data(self, cfg, fatigue_state_pair):
        for df_label in ['state_0_df', 'state_1_df']:
            
            for column in ['Mises', 'S11', 'S22', 'S33']:
                fatigue_state_pair[df_label][column] = self.get_stress(fatigue_state_pair[df_label][column])
             
        
    def read_files_and_get_stress_range(self, fatigue_state_pair):
        stress_range = []

        for state in ['state_0', 'state_1']:
            state_df = self.read_seasam_xtract(fatigue_state_pair[state])
            transformed_df = self.transform_df_for_stress_analysis(state_df)
            fatigue_state_pair.update({(state + '_df'): transformed_df})
        
        return fatigue_state_pair

    def transform_df_for_stress_analysis(self, df):
        # Convert columns to float
        df.replace('N/A', np.nan, inplace=True)
        for column in df.columns:
            try:
                df[column] = df[column].astype(float)
            except:
                logging.info(f'Could not convert column {column} to float')
        
        add_columns = ['Element', 'x_min', 'x_max', 'y_min', 'y_max', 'z_min', 'S']
        for column in add_columns:
            df[column] = np.nan

        for df_row in range(0, len(df)):
            df.loc[df_row, 'x_min'] = min(df.iloc[df_row]['X-coord(1)'], df.iloc[df_row]['X-coord(2)'], df.iloc[df_row]['X-coord(3)'], df.iloc[df_row]['X-coord(4)'], df.iloc[df_row]['X-coord(5)'], df.iloc[df_row]['X-coord(6)'], df.iloc[df_row]['X-coord(7)'], df.iloc[df_row]['X-coord(8)'])
            df.loc[df_row, 'x_max'] = max(df.iloc[df_row]['X-coord(1)'], df.iloc[df_row]['X-coord(2)'], df.iloc[df_row]['X-coord(3)'], df.iloc[df_row]['X-coord(4)'], df.iloc[df_row]['X-coord(5)'], df.iloc[df_row]['X-coord(6)'], df.iloc[df_row]['X-coord(7)'], df.iloc[df_row]['X-coord(8)'])
            df.loc[df_row, 'y_min'] = min(df.iloc[df_row]['Y-coord(1)'], df.iloc[df_row]['Y-coord(2)'], df.iloc[df_row]['Y-coord(3)'], df.iloc[df_row]['Y-coord(4)'], df.iloc[df_row]['Y-coord(5)'], df.iloc[df_row]['Y-coord(6)'], df.iloc[df_row]['Y-coord(7)'], df.iloc[df_row]['Y-coord(8)'])
            df.loc[df_row, 'y_max'] = max(df.iloc[df_row]['Y-coord(1)'], df.iloc[df_row]['Y-coord(2)'], df.iloc[df_row]['Y-coord(3)'], df.iloc[df_row]['Y-coord(4)'], df.iloc[df_row]['Y-coord(5)'], df.iloc[df_row]['Y-coord(6)'], df.iloc[df_row]['Y-coord(7)'], df.iloc[df_row]['Y-coord(8)'])
            df.loc[df_row, 'z_min'] = min(df.iloc[df_row]['Z-coord(1)'], df.iloc[df_row]['Z-coord(2)'], df.iloc[df_row]['Z-coord(3)'], df.iloc[df_row]['Z-coord(4)'], df.iloc[df_row]['Z-coord(5)'], df.iloc[df_row]['Z-coord(6)'], df.iloc[df_row]['Z-coord(7)'], df.iloc[df_row]['Z-coord(8)'])   
            df.loc[df_row, 'z_max'] = max(df.iloc[df_row]['Z-coord(1)'], df.iloc[df_row]['Z-coord(2)'], df.iloc[df_row]['Z-coord(3)'], df.iloc[df_row]['Z-coord(4)'], df.iloc[df_row]['Z-coord(5)'], df.iloc[df_row]['Z-coord(6)'], df.iloc[df_row]['Z-coord(7)'], df.iloc[df_row]['Z-coord(8)'])   
            if 'VONMISES(1)' in df.columns:
                df.loc[df_row, 'S'] = statistics.mean([df.iloc[df_row]['VONMISES(1)'], df.iloc[df_row]['VONMISES(2)'], df.iloc[df_row]['VONMISES(3)'], df.iloc[df_row]['VONMISES(4)'], df.iloc[df_row]['VONMISES(5)'], df.iloc[df_row]['VONMISES(6)'], df.iloc[df_row]['VONMISES(7)'], df.iloc[df_row]['VONMISES(8)']])
            elif 'SIGXX(3)' in df.columns:
                df.loc[df_row, 'S'] = statistics.mean([df.iloc[df_row]['SIGXX(1)'], df.iloc[df_row]['SIGXX(2)'], df.iloc[df_row]['SIGXX(3)'], df.iloc[df_row]['SIGXX(4)'], df.iloc[df_row]['SIGXX(5)'], df.iloc[df_row]['SIGXX(6)'], df.iloc[df_row]['SIGXX(7)'], df.iloc[df_row]['SIGXX(8)']])
            elif 'SIGYY(1)' in df.columns:
                df.loc[df_row, 'S'] = statistics.mean([df.iloc[df_row]['SIGYY(1)'], df.iloc[df_row]['SIGYY(2)'], df.iloc[df_row]['SIGYY(3)'], df.iloc[df_row]['SIGYY(4)'], df.iloc[df_row]['SIGYY(5)'], df.iloc[df_row]['SIGYY(6)'], df.iloc[df_row]['SIGYY(7)'], df.iloc[df_row]['SIGYY(8)']])
            elif 'TAUXY(1)' in df.columns:
                df.loc[df_row, 'S'] = statistics.mean([df.iloc[df_row]['TAUXY(1)'], df.iloc[df_row]['TAUXY(2)'], df.iloc[df_row]['TAUXY(3)'], df.iloc[df_row]['TAUXY(4)'], df.iloc[df_row]['TAUXY(5)'], df.iloc[df_row]['TAUXY(6)'], df.iloc[df_row]['TAUXY(7)'], df.iloc[df_row]['TAUXY(8)']])
            else:
                raise ValueError('Could not find stress column')

        return df

    def read_seasam_xtract(self, seasam_xtract_file):
        cfg = {
            'io': seasam_xtract_file,
            'start_line': 6,
            'end_line': 6,
            'DataFrame': False
        }

        column_data = read_data.from_ascii_file_get_structured_data_delimited_white_space(cfg)
        columns = [data[0] for data in column_data]

        cfg = {
            'io': seasam_xtract_file,
            'start_line': 7,
            'columns': columns,
            'DataFrame': True
        }

        df = read_data.from_ascii_file_get_structured_data_delimited_white_space(cfg)

        return df

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