import os
import pathlib
import pandas as pd
import OrcFxAPI
import logging
from digitalmodel.modules.orcaflex.orcaflex_preprocess import OrcaflexPreProcess
from digitalmodel.modules.orcaflex.orcaflex_objects import OrcaFlexObjects
from assetutilities.common.utilities import is_file_valid_func
from assetutilities.common.file_management import FileManagement


orcaflex_preprocess = OrcaflexPreProcess()
orcaflex_objects = OrcaFlexObjects()
fm = FileManagement()

class Mooring():
    def __init__(self):
        pass

    def router(self, cfg):
        groups = cfg['orcaflex_analysis']['mooring']['groups']
        for group in groups:
            if group['calculation'] == 'pretension':
                self.pretension_analysis(cfg, group)
            else:
                raise ValueError('Invalid calculation type for mooring analysis')

    def pretension_analysis(self, cfg, group):
        # utilize model to add moorings
        # utilize winch commands to add pretensions

        yml_files = cfg['file_management']['input_files']['yml']

        tension_df = self.get_tension(cfg, group)
        length_df = self.get_length(cfg, group)

        target_pretension = group['target_pretension']

        for yml_file_idx in range(0, len(yml_files)):
            yml_file = yml_files[yml_file_idx]
            yml_file_stem = pathlib.Path(yml_file).stem

            tension_df_file = tension_df[tension_df['fe_filename_stem']==yml_file_stem]
            length_df_file = length_df[length_df['fe_filename_stem']==yml_file_stem]

            if len(tension_df_file) == 1:
                pre_tension_analysis = self.pre_tension_analysis(cfg, target_pretension, tension_df_file, length_df_file, yml_file)

            else:
                logging.debug(f"No output to perform analysis for yml file {yml_file_stem}.")
                continue

            model.SaveData(yml_file)

    def pre_tension_analysis(self, cfg, target_pretension, tension_df_file, length_df_file, yml_file):
        
        model = OrcFxAPI.Model()
        model.LoadData(yml_file)

        pretension_analysis = []
        for item in target_pretension:
            pretension = item['pretension']
            object_name = item['name']
            ofx_object_cfg = {'ObjectName': object_name}
            ofx_object = orcaflex_objects.get_OrcFXAPIObject(model, ofx_object_cfg)
            if ofx_object is None:
                raise ValueError('Invalid object name. Code not implemented yet')

            current_tension = tension_df_file[object_name].values[0]
            current_length = length_df_file[object_name].values[0]

            pretension_delta = pretension - current_tension
            pretension_delta_percent = pretension_delta / current_tension * 100
            pretension_delta_percent_abs = abs(pretension_delta_percent)

            pre_tension_analysis_item = {
                'object_name': object_name,
                'current_tension': current_tension,
                'current_length': current_length,
                'pretension': pretension,
                'pretension_delta': pretension_delta,
                'pretension_delta_percent': pretension_delta_percent,
                'pretension_delta_percent_abs': pretension_delta_percent_abs
            }

            pretension_analysis.append(pre_tension_analysis_item)

        pretension_analysis_df = pd.DataFrame(pretension_analysis)
        pretension_analysis_df = pretension_analysis_df.round(4)
        filename_dir = fm.get_file_management_input_directory(cfg)
        filename_stem = pathlib.Path(yml_file).stem
        filename = os.path.join(filename_dir, filename_stem + '_pretension_analysis.csv')
        pretension_analysis_df.to_csv(filename, index=False)

        return pretension_analysis

    def get_tension(self, cfg, group):
        tension_cfg = group['tension']
        tension_filename = tension_cfg['filename']

        analysis_root_folder = cfg['Analysis']['analysis_root_folder']
        is_file_valid, tension_filename = is_file_valid_func(tension_filename, analysis_root_folder)

                
        tension_df = pd.read_csv(tension_filename)
        return tension_df

    def get_length(self, cfg, group):
        length_cfg = group['length']
        length_filename = length_cfg['filename']

        analysis_root_folder = cfg['Analysis']['analysis_root_folder']
        is_file_valid, length_filename = is_file_valid_func(length_filename, analysis_root_folder)

                
        length_df = pd.read_csv(length_filename)
        return length_df