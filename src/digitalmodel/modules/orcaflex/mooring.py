import os
import pathlib
import pandas as pd
import OrcFxAPI
import logging
from digitalmodel.modules.orcaflex.orcaflex_preprocess import OrcaflexPreProcess
from digitalmodel.modules.orcaflex.orcaflex_objects import OrcaFlexObjects
from assetutilities.common.utilities import is_file_valid_func
from assetutilities.common.file_management import FileManagement
from assetutilities.common.data import SaveData

orcaflex_preprocess = OrcaflexPreProcess()
orcaflex_objects = OrcaFlexObjects()
fm = FileManagement()
save_data = SaveData()

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
                pretension_analysis_dict = self.pre_tension_analysis(cfg, group, target_pretension, tension_df_file, length_df_file, yml_file)
                self.prepare_includefile_for_analysis(cfg, group, yml_file, pretension_analysis_dict)
                self.prepare_includefile_for_all_lines(cfg, group, yml_file, pretension_analysis_dict)

            else:
                logging.debug(f"No output to perform analysis for yml file {yml_file_stem}.")
                continue

        pass

    def pre_tension_analysis(self, cfg, group, target_pretension, tension_df_file, length_df_file, yml_file):

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
            pretension_delta_percent = pretension_delta / pretension * 100
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

        pretension_analysis_df_sorted = pretension_analysis_df.sort_values(by=['pretension_delta_percent_abs'], ascending=True)
        stabilizing_lines_sorted = pretension_analysis_df_sorted['object_name'].to_list()
        preferred_stabilizing_lines = group['iteration_cfg']['stabilizing_lines']['object_name']
        no_of_preferred_stabilizing_lines = group['iteration_cfg']['stabilizing_lines']['number_of_lines']
        stabilizing_lines = []
        for item in stabilizing_lines_sorted:
            if item in preferred_stabilizing_lines:
                stabilizing_lines.append(item)

        stabilizing_lines = stabilizing_lines[:no_of_preferred_stabilizing_lines]
        max_pretension = pretension_analysis_df_sorted['pretension_delta_percent_abs'].max()

        logging.info(f"For Filename: {filename}:")
        logging.info(f"    ... Stabilizing lines: {stabilizing_lines}")
        logging.info(f"    ... Max pretension difference %: {round(max_pretension,0)}")
        pretension_analysis_dict = {
            'pretension_analysis_df': pretension_analysis_df,
            'stabilizing_lines': stabilizing_lines,
            'filename': filename,
            'max_pretension': max_pretension,
        }

        return pretension_analysis_dict

    def prepare_includefile_for_analysis(self, cfg, group, yml_file, pretension_analysis_dict):
        pretension_analysis_df = pretension_analysis_dict['pretension_analysis_df']
        stabilizing_lines = pretension_analysis_dict['stabilizing_lines']

        includefile_dict = {}
        for row_idx in range(0, len(pretension_analysis_df)):
            row = pretension_analysis_df.iloc[row_idx]
            object_name = row['object_name']
            current_length = float(row['current_length'])
            pretension = float(row['pretension'])

            stage_array_item = []
            if object_name in stabilizing_lines:
                stage_array_item.append(['Specified length', current_length])
            else:
                stage_array_item.append(['Specified tension', pretension])

            stage_array_item.append(['Specified tension', pretension])
            stage_array_item.append(['Specified payout', 0])

            item = {object_name: {'StageMode, StageValue': stage_array_item}}
            includefile_dict.update(item)

        filename_dir = fm.get_file_management_input_directory(cfg)
        filename_stem = pathlib.Path(yml_file).stem
        filename = 'includefile_' + filename_stem
        filename_path = os.path.join(filename_dir, filename)
        save_data.saveDataYaml({'Winches': includefile_dict}, filename_path, default_flow_style=False)

    def prepare_includefile_for_all_lines(self, cfg, group, yml_file, pretension_analysis_dict):
        pretension_analysis_df = pretension_analysis_dict['pretension_analysis_df']

        includefile_dict = {}
        for row_idx in range(0, len(pretension_analysis_df)):
            row = pretension_analysis_df.iloc[row_idx]
            object_name = row['object_name']
            current_length = float(row['current_length'])

            stage_array_item = []
            stage_array_item.append(['Specified length', current_length])
            stage_array_item.append(['Specified payout', 0])
            stage_array_item.append(['Specified payout', 0])

            item = {object_name: {'StageMode, StageValue': stage_array_item}}
            includefile_dict.update(item)

        filename_dir = fm.get_file_management_input_directory(cfg)
        filename_stem = pathlib.Path(yml_file).stem
        filename = 'includefile_all_lines_' + filename_stem
        filename_path = os.path.join(filename_dir, filename)
        save_data.saveDataYaml({'Winches': includefile_dict}, filename_path, default_flow_style=False)

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