# Standard library imports
import io
import itertools
import logging
import os
import subprocess
from pathlib import Path

# Third party imports
import numpy as np
import pandas as pd
from assetutilities.common.data import ReadData, SaveData
from assetutilities.common.data_exploration import DataExploration
from assetutilities.common.file_management import FileManagement
from assetutilities.common.update_deep import update_deep_dictionary

# Reader imports
from digitalmodel.custom.aqwa.aqwa_utilities import AqwaUtilities

fm = FileManagement()
rd = ReadData()
save_data = SaveData()
au = AqwaUtilities()
de = DataExploration()

class AqwaReader:

    def __init__(self) -> None:
        pass

    def router(self, cfg):
        self.aqwareader_exe = au.get_aqwareader_exe(cfg)
        self.workbench_bat = au.get_workbench_bat(cfg)
        cfg = fm.router(cfg)
        self.postprocess(cfg)

    def postprocess(self, cfg):

        for result_item in cfg['result']:
            result_item, cfg = self.update_iteration_item_with_master_settings(result_item, cfg)
            input_files = cfg['file_management']['input_files']['PLT']

            master_df = pd.DataFrame()
            result_category = result_item['category'] 

            for input_file in input_files:
                input_file_result = []
                result_item_dict = self.get_result_groups(input_file, cfg, result_item)
                input_file_result.append(result_item_dict)

                try:
                    df_item = pd.DataFrame(result_item_dict)
                except Exception as e:
                    if 'you must pass an index' in str(e):
                        df_item = pd.DataFrame(result_item_dict, index = [0])
                    else:
                        raise e

                df_item['input_file'] = Path(input_file).stem
                save_csv = result_item.get('save_csv', False)
                if save_csv:
                    sheetname = result_item['inject_into'].get('sheetname', None)
                    if sheetname is None:
                        sheetname = Path(input_file).stem + '_' + result_item['label']
                    self.save_to_csv(df_item, result_item, cfg, sheetname)
                    # self.save_statistics(cfg, df_item, sheetname)
                    self.inject_to_excel(df_item, result_item, cfg)

                # if result_category == 'equilibrium':
                #     df = pd.DataFrame.from_dict(input_file_result)
                #     master_df = pd.concat([master_df, df], axis=0)

            sheetname = result_item['label']
            self.save_to_csv(master_df, result_item, cfg, sheetname)
            self.inject_to_excel(master_df, result_item, cfg)

    def get_result_groups(self, input_file, cfg, result_item):
        result_item_dict = {}
        for aqwa_fundamental_cfg in result_item['groups']:
            result_group = self.get_result_group(input_file, cfg, aqwa_fundamental_cfg, result_item)
            result_item_dict= update_deep_dictionary(result_item_dict, result_group)

        return result_item_dict

    def get_result_group(self, input_file, cfg, aqwa_fundamental_cfg, result_item):

        logging.debug(f"Running AqwaReader for {input_file } ... START\n")

        plt_2d_array = self.get_property_args_array(aqwa_fundamental_cfg, result_item)

        result_group = {}
        for plt_1d_array in plt_2d_array:
            args = self.get_args_for_data(input_file, cfg, plt_1d_array)
            stdout_process = subprocess.Popen(args,
                            stdout=subprocess.PIPE, 
                            stderr=subprocess.PIPE)
            stdout_output, err = stdout_process.communicate()
            result_category = result_item['category']
            if result_category == 'equilibrium':
                result_plt_1d_item = self.process_equilibrium_result(input_file, stdout_output)
            elif result_category == 'frequency':
                result_plt_1d_item = self.process_frequency_result(input_file, stdout_output)
            elif result_category == 'timeresponse':
                result_plt_1d_item = self.process_timeresponse_result(input_file, stdout_output)

            result_group = update_deep_dictionary(result_group , result_plt_1d_item)
            change_result_plt_1d_item= self.get_change_of_data_result_group(result_plt_1d_item, aqwa_fundamental_cfg)
            if change_result_plt_1d_item is not None:
                result_group = update_deep_dictionary(result_group , change_result_plt_1d_item)

            save_aqwareader_csv = result_item.get('save_aqwareader_csv', False)
            if save_aqwareader_csv:
                args = self.get_args_for_data(input_file, cfg, plt_1d_array, result_format='csv')
                csv_process = subprocess.Popen(args,
                            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
                csv_out, err = csv_process.communicate()

            # code = subprocess.call(args, stdout = ts, stderr = ts)
            logging.debug(f"Returned {err} \n")
            logging.debug(f"Running AqwaReader for {input_file } ... COMPLETE\n")

        return result_group

    def get_change_of_data_result_group(self, result_plt_1d_item, aqwa_fundamental_cfg):
        change_result_plt_1d_item = None
        change_of_data_flag = False
        change_of_data = aqwa_fundamental_cfg.get('change_of_data', None)
        if change_of_data  is not None and change_of_data ['flag']:
            change_of_data_flag  = True

        if change_of_data_flag:
            data_key = list(result_plt_1d_item.keys())[1]
            values = result_plt_1d_item[data_key]

            if change_of_data['reference'] == 'start':
                reference = values[0]
                values = [value - reference for value in values]
            elif change_of_data['reference'] == 'end':
                reference = values[-1]
                values = [value - reference for value in values]
            elif change_of_data['reference'] == 'mean':
                reference = values.mean()
                values = [value - reference for value in values]
            elif change_of_data['reference'] == 'transform':
                result_keys = list(result_plt_1d_item.keys())
                if 'COG_in X direction' in result_keys[1]:
                    idx = 0
                elif 'COG_in Y direction' in result_keys[1]:
                    idx = 1
                elif 'COG_in Z direction' in result_keys[1]:
                    idx = 2
                elif 'COG_about X axis' in result_keys[1]:
                    idx = 3
                elif 'COG_about Y axis' in result_keys[1]:
                    idx = 4
                elif 'COG_about Z axis' in result_keys[1]:
                    idx = 5
                else:
                    raise Exception(f"Invalid result key {result_keys[1]}")

                cfg_transform = change_of_data['transform']
                values = [value * cfg_transform['scale'][idx] + cfg_transform['shift'][idx] for value in values]

            else:
                print(f"Invalid reference {change_of_data['reference']}")
                return change_result_plt_1d_item

            change_data_key = data_key.replace(change_of_data['label_substitution']['before'], change_of_data['label_substitution']['after'])
            column_1_key = list(result_plt_1d_item.keys())[0]
            change_result_plt_1d_item = {column_1_key: result_plt_1d_item[column_1_key] , change_data_key: values}

        return change_result_plt_1d_item 

    def get_args_for_data(self, input_file, cfg, plt_1d_array, result_format='stdout'):
        basename = Path(input_file).stem
        csv_output = basename + '_' + ''.join([str(x) for x in plt_1d_array])
        workdir = cfg['Analysis']['result_folder']

        argStrings = []
        argVals = []

        args = [self.workbench_bat]

        argStrings.append("-cmd")
        argVals.append(self.aqwareader_exe)

        argStrings.append("--Type")
        argVals.append("Graphical")

        argStrings.append("--InFile")
        argVals.append("{:s}".format(str(input_file)))

        argStrings.append("--OutFile")
        if result_format == 'csv':
            argVals.append("{:s}".format(os.path.join(workdir, csv_output)))
        elif result_format == 'stdout':
            argVals.append(result_format)

        if result_format in ['csv', 'stdout']:
            argStrings.append("--Format")
            argVals.append('csv')
        else:
            raise Exception(f"Invalid result_format {result_format}")

        argStrings.append("--PLT1")
        argVals.append("{:d}".format(plt_1d_array[0]))

        argStrings.append("--PLT2")
        argVals.append("{:d}".format(plt_1d_array[1]))

        argStrings.append("--PLT3")
        argVals.append("{:d}".format(plt_1d_array[2]))

        argStrings.append("--PLT4")
        argVals.append("{:d}".format(plt_1d_array[3]))
        
        for (string, val) in zip(argStrings, argVals):
            args.extend([string, val])
        return args

    def get_property_args_array(self, aqwa_fundamental_cfg, result_item):
        plt_2d_array = []
        first_level = aqwa_fundamental_cfg.get('first_level', None)
        second_level = aqwa_fundamental_cfg.get('second_level', None)
        third_level_label = aqwa_fundamental_cfg.get('third_level_label', None)
        third_level = aqwa_fundamental_cfg.get('third_level', None)
        fourth_level = aqwa_fundamental_cfg.get('fourth_level', None)

        if fourth_level is None:
            fourth_level = [idx+1 for idx in range(0, 6)]

        plt_2d_for_itertools = [first_level, second_level, third_level, fourth_level]
        plt_2d_array = [list(item) for item in list(itertools.product(*plt_2d_for_itertools))]

        return plt_2d_array

    def process_equilibrium_result(self, input_file, stdout_output):
        df = pd.read_csv(io.BytesIO(stdout_output), encoding='utf8', sep=",|:", dtype={"switch": np.int8}, names=['column1', 'column2'], engine='python')
        structure_number = df.iloc[1]['column2'].replace('\t', '').replace('"', '').replace('Structure Number ', '')
        third_level = df.iloc[2]['column2'].replace('\t', '').replace('"', '')
        fourth_level = df.iloc[3]['column2'].replace('\t', '').replace('"', '')

        iteration = [float(item) for item in list(df[5:-1]['column1'])]
        value = [float(item) for item in list(df[5:-1]['column2'])]
        value_label = third_level + '_'+ fourth_level
        result = {'iteration': iteration, value_label: value}

        return result

    def process_frequency_result(self, input_file, stdout_output):
        df = pd.read_csv(io.BytesIO(stdout_output), encoding='utf8', sep=",|:", dtype={"switch": np.int8}, names=['column1', 'column2'], engine='python')
        structure_number = df.iloc[0]['column2'].replace('\t', '').replace('PARAMETERS VERSUS FREQUENCY', '').replace('STRUCTURE', '').replace('"', '').replace(' ', '')
        third_level = df.iloc[2]['column2'].replace('\t', '').replace('"', '').replace('-', '')
        fourth_level = df.iloc[3]['column2'].replace('\t', '').replace('"', '')

        frequency = [float(item) for item in list(df[5:-1]['column1'])]
        value = [float(item) for item in list(df[5:-1]['column2'])]
        value_label = third_level + '_'+ fourth_level
        result = {'frequency': frequency, value_label: value}
        return result

    def process_timeresponse_result(self, input_file, stdout_output):
        df = pd.read_csv(io.BytesIO(stdout_output), encoding='utf8', sep=",|:", dtype={"switch": np.int8}, names=['column1', 'column2'], engine='python')

        if df is None or df.empty:
            raise Exception(f"Error in reading std output data from {input_file}")

        structure_number = df.iloc[1]['column2'].replace('\t', '').replace('"', '').replace('Structure Number ', '')
        third_level = df.iloc[2]['column2'].replace('\t', '').replace('"', '').replace('-', '')
        fourth_level = df.iloc[3]['column2'].replace('\t', '').replace('"', '')

        time_step = [float(item) for item in list(df[5:-1]['column1'])]
        value = [float(item) for item in list(df[5:-1]['column2'])]
        value_label = third_level + '_'+ fourth_level
        result = {'time_step': time_step, value_label: value}
        return result

    def inject_to_excel(self, df, result_item, cfg):
        if 'inject_into' in result_item and result_item[
                'inject_into']['flag']:
            inject_into_file = result_item['inject_into']['filename']
            file_name = os.path.join(cfg['Analysis']['analysis_root_folder'],
                        inject_into_file)
            if not os.path.isfile(file_name):
                raise Exception(f"Inject Into File {file_name} not found for writing summary data")

            sheetname = result_item['inject_into']['sheetname']
            if sheetname is None:
                sheetname = result_item['label']

            cfg_save_to_existing_workbook = {'template_file_name': file_name, 'sheetname': sheetname, 'saved_file_name': file_name, 'if_sheet_exists': 'replace', 'df': df}
            save_data.df_to_sheet_in_existing_workbook(cfg_save_to_existing_workbook)

    def save_to_csv(self, df, result_item, cfg, sheetname):

        csv_filename = os.path.join(cfg['Analysis']['result_folder'], sheetname + '.csv')
        df.to_csv(csv_filename, index=False, header=True)

    def save_statistics(self, cfg, df, sheetname):
        statistics_filename = os.path.join(cfg['Analysis']['result_folder'], sheetname + '_statistics.csv')
        df_statistics = de.get_df_statistics(df)
        df_statistics.to_csv(statistics_filename, index=True, header=True)

    def update_iteration_item_with_master_settings(self, iteration_item, cfg):
        master_settings = cfg.get('master_settings', {})
        iteration_item = update_deep_dictionary(iteration_item, master_settings)

        if 'filename_pattern' in iteration_item:
            filename_pattern = iteration_item['filename_pattern']
            if filename_pattern is not None:
                cfg['file_management']['files']['files_in_current_directory']['filename_pattern'] = filename_pattern

        if 'directory' in iteration_item:
            directory = iteration_item['directory']
            if directory is not None:
                cfg['file_management']['files']['files_in_current_directory']['directory'] = directory

        cfg = fm.router(cfg)

        return iteration_item, cfg