import os
import pathlib
import pandas as pd
import numpy as np
try:
    import OrcFxAPI
except Exception:
    raise RuntimeError("OrcaFlex license not available. Run on different computer")
from loguru import logger
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

            logger.info(f"Mooring analysis started for {file_name}")
            current_iteration = 0
            iteration_flag = True
            # while iteration_flag:
            sim_file = file_name
            var_data_dict = all_vars.get_var_data_by_model(cfg, sim_file)
            force_balance_analysis_dict = self.mooring_pre_tension_analysis(cfg, group, file_meta_data, var_data_dict)
            fender_force_analysis_dict = self.fender_force_analysis(cfg, group, file_meta_data, var_data_dict)
            # Run the analysis is currently manual process
            # status_flag = ou.file_run_and_save(cfg, file_meta_data)
            current_iteration = current_iteration + 1
            iteration_flag = self.get_iteration_flag(cfg, group, force_balance_analysis_dict, current_iteration)

        return cfg

    def mooring_pre_tension_analysis(self, cfg, group, file_meta_data, var_data_dict):
        output_dict = {}
        target_pre_tension_df = self.get_target_pretension(cfg, group)
        columns = ['ObjectName', 'Effective tension', 'Arc length', 'End GY force']
        current_var_df = var_data_dict['StaticResult']['Line'][columns]

        new_arc_length = []
        target_pre_tension_df['current_tension'] = None
        target_pre_tension_df['new_line_length'] = None
        target_pre_tension_df['end_Gy_force'] = None
        for index, row in target_pre_tension_df.iterrows():
            object_name = row['ObjectName']
            mask = current_var_df['ObjectName'] == object_name
            current_var_df_filtered = current_var_df[mask]
            effective_tension = (
                current_var_df_filtered['Effective tension'].values[0]
            )
            arc_length = current_var_df_filtered['Arc length'].values[0]

            line_length = row['line_length']
            new_line_length = line_length.copy()
            line_ea = row['line_EA']
            target_tension = row['target_tension']
            target_pre_tension_df.at[index, 'end_Gy_force'] = (
                current_var_df_filtered['End GY force'].values[0]
            )

            self.evaluate_line_length_current(arc_length, line_length)

            self.evaluate_line_length_next_iteration(effective_tension, arc_length, line_length, new_line_length, line_ea, target_tension)

            target_pre_tension_df.at[index, 'new_line_length'] = (
                new_line_length
            )
            target_pre_tension_df.at[index, 'current_tension'] = (
                effective_tension
            )

        tension_criteria_pass_flag = self.get_tension_criteria(
            cfg, group, target_pre_tension_df
        )

        self.prepare_includefile_for_all_lines(
            cfg, file_meta_data, target_pre_tension_df
        )
        
        target_pre_tension_df = target_pre_tension_df.apply(pd.to_numeric, errors='ignore').round(4)
        output_dict = {
            'tension_criteria_pass_flag': tension_criteria_pass_flag,
            'target_pre_tension_df': target_pre_tension_df
        }

        filename_dir = cfg['Analysis']['result_folder']
        yml_file = file_meta_data['yml']
        filename_stem = pathlib.Path(yml_file).stem
        filename = filename_stem + '_pretension_analysis.csv'
        filename_path = os.path.join(filename_dir, filename)
        target_pre_tension_df.to_csv(filename_path, index=False)

        return output_dict

    def evaluate_line_length_next_iteration(self, effective_tension, arc_length, line_length, new_line_length, line_ea, target_tension):
        delta_length = []
        for i, length in enumerate(line_length):
            delta = length / line_ea[i]
            tension_diff = effective_tension - target_tension
            delta_length_section = delta * tension_diff
            delta_length.append(delta_length_section)

        new_arc_length = arc_length + sum(delta_length)
        for i, length in enumerate(new_line_length):
            if length is None:
                other_length = sum(filter(None, new_line_length))
                length_section = new_arc_length - other_length
                new_line_length[i] = round(float(length_section), 4)

    def evaluate_line_length_current(self, arc_length, line_length):
        for i, length in enumerate(line_length):
            if length is None:
                other_line_length = sum(filter(None, line_length))
                line_length[i] = arc_length - other_line_length

    def fender_force_analysis(self, cfg, group, file_meta_data, var_data_dict):
        output_dict = {}

        fender_force_df = self.evaluate_compression_current(cfg, group, var_data_dict)

        delta_compression = self.evaluate_compression_next_iteration(group, fender_force_df)
        
        vessel_df = var_data_dict['StaticResult']['Vessel'][['ObjectName', 'X', 'Y', 'Z']]
        fender_contact_vessel = group['target_fender_force']['contact_vessel']
        vessel_df_filetered = vessel_df[vessel_df['ObjectName']==fender_contact_vessel].copy()
        vessel_df_filetered['Y_next_iteration'] = vessel_df_filetered['Y'] - delta_compression

        fender_force_df = fender_force_df.apply(pd.to_numeric, errors='ignore').round(4)
        output_dict = {
            'fender_force_df': fender_force_df, 'delta_compression': delta_compression, 'vessel_df_filetered': vessel_df_filetered,
        }

        fender_criteria_pass_flag = self.get_fender_criteria(
            cfg, group, output_dict
        )
        output_dict.update({'fender_criteria_pass_flag': fender_criteria_pass_flag})
        self.prepare_includefile_for_fender_compression(
            cfg, file_meta_data, output_dict)

        filename_dir = cfg['Analysis']['result_folder']
        yml_file = file_meta_data['yml']
        filename_stem = pathlib.Path(yml_file).stem
        filename = filename_stem + '_fender_force_analysis.csv'
        filename_path = os.path.join(filename_dir, filename)
        fender_force_df.to_csv(filename_path, index=False)

        return output_dict

    def evaluate_compression_current(self, cfg, group, var_data_dict):
        fender_force_df = self.get_target_fender_force(cfg, group)
        columns = ['ObjectName', 'Displacement', 'x', 'In-frame connection GY force', 'In-frame connection Ly force']
        current_var_df = var_data_dict['StaticResult']['Constraint'][columns]

        fender_force_df['current_fender_force'] = None
            logger.error(f"Error in mooring analysis: {e}")
        for index, row in fender_force_df.iterrows():
            object_name = row['ObjectName']
            mask = current_var_df['ObjectName'] == object_name
            current_var_df_filtered = current_var_df[mask]
            displacement = current_var_df_filtered['Displacement'].values[0]
            compression_x = current_var_df_filtered['x'].values[0]
            fender_force = current_var_df_filtered['In-frame connection GY force'].values[0]
            fender_properties = row['fender_properties']

            fender_force_df.at[index, 'current_fender_force'] = fender_force
            fender_force_df.at[index, 'compression'] = compression_x
            fender_force_df.at[index, 'displacement'] = displacement
            fender_force_df.at[index, 'fender_properties'] = fender_properties
        return fender_force_df

    def evaluate_compression_next_iteration(self, group, fender_force_df):
        total_force = -fender_force_df['current_fender_force'].sum()
        total_target_force = group['target_fender_force']['resultant_force']
        fender_force_df['new_fender_force'] = total_target_force/total_force * fender_force_df['current_fender_force']
        for index, row in fender_force_df.iterrows():
            fender_properties = row['fender_properties']
            fender_properties_df = pd.DataFrame(fender_properties, columns=['compression', 'force'])
            new_fender_force = row['new_fender_force']
            new_compression = np.interp(-new_fender_force, fender_properties_df['force'], fender_properties_df['compression'])
            fender_force_df.at[index, 'new_compression'] = new_compression
            fender_force_df.at[index, 'new_fender_force'] = new_fender_force
            fender_force_df.at[index, 'delta_compression'] = new_compression - row['compression']
        delta_compression = fender_force_df['compression'].iloc[3] - fender_force_df['new_compression'].iloc[3]
        return delta_compression

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

    def get_fender_criteria(self, cfg, group, output_dict):
        tolerance = group['target_fender_force']['tolerance']
        fender_force_df = output_dict['fender_force_df']
        fender_criteria_pass_flag = True
        total_fender_force_current = fender_force_df['current_fender_force'].sum()
        total_fender_force_target = group['target_fender_force']['resultant_force']
        fender_force_difference = abs(abs(total_fender_force_current) - total_fender_force_target)/total_fender_force_target*100
        if fender_force_difference > tolerance:
            fender_criteria_pass_flag = False

        return fender_criteria_pass_flag

    def get_iteration_flag(self, cfg, group, force_balance_analysis_dict, current_iteration):
        iteration_flag = False
        max_iterations = group['target_pretension']['iterations']
        if current_iteration <= max_iterations:
            iteration_flag = True
        if force_balance_analysis_dict['tension_criteria_pass_flag'] == False:
            iteration_flag = True

        return iteration_flag

    def prepare_includefile_for_all_lines(self, cfg, file_meta_data, target_pre_tension_df):
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
        yml_file = file_meta_data['yml']
        filename_stem = pathlib.Path(yml_file).stem
        filename = 'includefile_' + filename_stem + '_mooring_line_length'
        filename_path = os.path.join(filename_dir, filename)
        save_data.saveDataYaml({'Lines': includefile_dict}, filename_path, default_flow_style=False)

    def prepare_includefile_for_fender_compression(self, cfg, file_meta_data, output_dict):
        vessel_df_filtered = output_dict['vessel_df_filetered']
        includefile_dict = {}
        for row_idx in range(0, len(vessel_df_filtered)):
            row = vessel_df_filtered.iloc[row_idx]
            object_name = row['ObjectName']

            array_item = []

            new_vessel_y = row['Y_next_iteration']
            new_vessel_y = round(float(new_vessel_y), 4)
            initial_y_next_iteration = {'InitialY': new_vessel_y}

            array_item = {object_name: initial_y_next_iteration}
            includefile_dict.update(array_item)

        filename_dir = fm.get_file_management_input_directory(cfg)
        yml_file = file_meta_data['yml']
        filename_stem = pathlib.Path(yml_file).stem
        filename = 'includefile_' + filename_stem + '_fender_compression'
        filename_path = os.path.join(filename_dir, filename)
        save_data.saveDataYaml({'Vessels': includefile_dict}, filename_path, default_flow_style=False)

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

    def get_target_fender_force(self, cfg, group):
        fender_force_cfg = group['target_fender_force']
        fender_force_filename = fender_force_cfg['filename']

        analysis_root_folder = cfg['Analysis']['analysis_root_folder']
        is_file_valid, fender_force_filename = is_file_valid_func(fender_force_filename, analysis_root_folder)

        fender_force_df = pd.read_csv(fender_force_filename)
        
        fender_force_df['fender_properties'] = [
            eval(item) for item in list(fender_force_df['fender_properties'])
        ]

        return fender_force_df

