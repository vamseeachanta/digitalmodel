import os
import pathlib
import pandas as pd
import OrcFxAPI
import logging
from digitalmodel.modules.orcaflex.orcaflex_preprocess import OrcaflexPreProcess
from digitalmodel.modules.orcaflex.orcaflex_objects import OrcaFlexObjects
from digitalmodel.modules.orcaflex.orcaflex_utilities import OrcaflexUtilities
from digitalmodel.modules.orcaflex.all_vars import AllVars

from assetutilities.common.utilities import is_file_valid_func
from assetutilities.common.file_management import FileManagement
from assetutilities.common.data import SaveData

orcaflex_preprocess = OrcaflexPreProcess()
orcaflex_objects = OrcaFlexObjects()
ou = OrcaflexUtilities()
fm = FileManagement()
save_data = SaveData()
all_vars = AllVars()  # noqa


class Mooring():
    def __init__(self):
        pass

    def router(self, cfg):
        groups = cfg['orcaflex_analysis']['mooring']['groups']
        for group in groups:
            if cfg['orcaflex']['analysis']['mooring']['calculation'] == 'pretension':
                self.pretension_analysis(cfg, group)
            else:
                raise ValueError('Invalid calculation type for mooring analysis')

    def pretension_analysis(self, cfg, group):
        # utilize model to add moorings
        # utilize winch commands to add pretensions

        sim_files = cfg['file_management']['input_files']['sim']
        yml_files = cfg['file_management']['input_files']['yml']

        for fileIndex in range(0, len(sim_files)):
            file_name = sim_files[fileIndex]
            filename_wo_ext = file_name.with_suffix('')
            yml_file = filename_wo_ext.with_suffix('.yml')
            file_meta_data = {'sim': file_name, 'yml': yml_file}

            current_iteration = 0
            iteration_flag = True
            if iteration_flag:
                sim_file = file_name
                var_data_dict = all_vars.get_var_data_by_model(cfg, sim_file)
                force_balance_analysis_dict = self.force_balance_analysis(cfg, group, file_meta_data, var_data_dict)
                current_iteration = current_iteration + 1
                iteration_flag = self.get_iteration_flag(cfg, group, force_balance_analysis_dict, current_iteration)

        return cfg

    def force_balance_analysis(self, cfg, group, file_meta_data, var_data_dict):
        output_dict = {}
        target_pre_tension_df = self.get_target_pretension(cfg, group)
        current_var_df = var_data_dict['StaticResult']['Line'][['ObjectName', 'Effective tension', 'Arc length']]

        new_arc_length = []
        target_pre_tension_df['current_tension'] = None
        target_pre_tension_df['new_line_length'] = None
        for index, row in target_pre_tension_df.iterrows():
            object_name = row['ObjectName']
            current_var_df_filtered = current_var_df[current_var_df['ObjectName'] == object_name]
            effective_tension = current_var_df_filtered['Effective tension'].values[0]
            arc_length = current_var_df_filtered['Arc length'].values[0]

            line_length = row['line_length']
            new_line_length = line_length.copy()
            line_EA = row['line_EA']
            target_tension = row['target_tension']
            for list_i in range(len(line_length)):
                line_length_section = line_length[list_i]
                if line_length_section is None:
                    other_line_length = sum(filter(None, line_length))
                    line_length[list_i] = arc_length - other_line_length

            delta_length = []
            for list_i in range(len(line_length)):
                delta_length_section = line_length[list_i]/line_EA[list_i]*(effective_tension - target_tension)
                delta_length.append(delta_length_section)

            new_arc_length = arc_length + sum(delta_length)
            for list_i in range(len(new_line_length)):
                line_length_section = new_line_length[list_i]
                if line_length_section is None:
                    other_line_length = sum(filter(None, new_line_length))
                    line_length_section = new_arc_length - other_line_length
                    new_line_length[list_i] = round(float(line_length_section), 4)
            target_pre_tension_df.at[index, 'new_line_length'] = new_line_length
            target_pre_tension_df.at[index, 'current_tension'] = effective_tension

        tension_criteria_pass_flag = self.get_tension_criteria(cfg, group, target_pre_tension_df)

        self.prepare_includefile_for_all_lines(cfg, file_meta_data, target_pre_tension_df)
        status_flag = ou.file_run_and_save(cfg, file_meta_data)
        output_dict = {'status_flag': status_flag, 'tension_criteria_pass_flag': tension_criteria_pass_flag}

        return output_dict

    def get_tension_criteria(self, cfg, group, target_pre_tension_df):
        tolerance = group['target_pretension']['tolerance']
        tension_criteria_pass_flag = True
        for index, row in target_pre_tension_df.iterrows():
            current_tension = row['current_tension']
            target_tension = row['target_tension']
            tension_difference = abs(current_tension - target_tension)/target_tension*100
            if tension_difference > tolerance:
                tension_criteria_pass_flag = False

        return tension_criteria_pass_flag

    def get_iteration_flag(self, cfg, group, force_balance_analysis_dict, current_iteration):
        iteration_flag = False
        max_iterations = group['target_pretension']['iterations']
        if current_iteration <= max_iterations:
            iteration_flag = True
        if force_balance_analysis_dict['tension_criteria_pass_flag'] == False:
            iteration_flag = True

        return iteration_flag

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
        save_data.saveDataYaml({'Lines': includefile_dict}, filename_path, default_flow_style=False)

    def prepare_includefile_for_all_lines(self, cfg, file_meta_data, target_pre_tension_df):
        yml_file = file_meta_data['yml']
        includefile_dict = {}
        for row_idx in range(0, len(target_pre_tension_df)):
            row = target_pre_tension_df.iloc[row_idx]
            object_name = row['ObjectName']

            array_item = []

            new_line_definition = []
            new_line_length = row['new_line_length']
            for i, length in enumerate(new_line_length):
                line_section_item = {f'Length[{i+1}]': length}
                new_line_definition.append(line_section_item)

            array_item = {object_name: new_line_definition}
            includefile_dict.update(array_item)

        filename_dir = fm.get_file_management_input_directory(cfg)
        filename_stem = pathlib.Path(yml_file).stem
        filename = 'includefile_line_defintion_' + filename_stem
        filename_path = os.path.join(filename_dir, filename)
        save_data.saveDataYaml({'Lines': includefile_dict}, filename_path, default_flow_style=False)

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
    
    def get_target_pretension(self, cfg, group):
        tension_cfg = group['target_pretension']
        tension_filename = tension_cfg['filename']

        analysis_root_folder = cfg['Analysis']['analysis_root_folder']
        is_file_valid, tension_filename = is_file_valid_func(tension_filename, analysis_root_folder)

        pre_tension_df = pd.read_csv(tension_filename)
        pre_tension_df['line_length'] =[eval(item) for item in list(pre_tension_df['line_length'])]
        pre_tension_df['line_EA'] =[eval(item) for item in list(pre_tension_df['line_EA'])]
        
        return pre_tension_df
