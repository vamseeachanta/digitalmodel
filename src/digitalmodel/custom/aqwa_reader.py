import os
import math
import logging
import subprocess

import pandas as pd
from pathlib import Path

from assetutilities.common.data import ReadData
from assetutilities.common.data import SaveData

from digitalmodel.custom.aqwa_utilities import AqwaUtilities

au = AqwaUtilities()
rd = ReadData()
save_data = SaveData()

class AqwaReader:

    def __init__(self):
        pass

    def router(self, cfg):
        self.get_aqwareader_exe(cfg)
        self.get_workbench_bat(cfg)
        au.file_management(cfg)
        for result_item in cfg['result']:
            df = self.get_data(cfg, result_item)
            self.inject_to_excel(df, result_item, cfg)

    def get_aqwareader_exe(self, cfg):
        ANSYSInstallDir = cfg['software']['ANSYSInstallDir']
        aqwareader_exe = os.path.join(ANSYSInstallDir, "aisol", "bin", "winx64", "AqwaReader.exe")
        if not os.path.isfile(aqwareader_exe):
            raise Exception(f"AqwaReader.exe not found in {aqwareader_exe}")

        self.aqwareader_exe = aqwareader_exe

    def get_workbench_bat(self, cfg):
        ANSYSInstallDir = cfg['software']['ANSYSInstallDir']
        workbench_bat = os.path.join(ANSYSInstallDir, "aisol", "workbench.bat")
        if not os.path.isfile(workbench_bat):
            raise Exception(f"workbench.bat not found in {workbench_bat}")

        self.workbench_bat = workbench_bat
        
    def get_data(self, cfg, result_item):
        input_files = cfg['file_management']['input_files']['PLT']

        df_all_files = pd.DataFrame()
        for input_file in input_files:
            self.get_result_groups(input_file, cfg, result_item)

        return df_all_files

    def get_result_groups(self, input_file, cfg, result_item):
        for aqwa_fundamental_cfg in result_item['groups']:
            self.get_aqwa_fundamental_result(input_file, cfg, aqwa_fundamental_cfg, result_item)

    def get_aqwa_fundamental_result(self, input_file, cfg, aqwa_fundamental_cfg, result_item):

        logging.debug(f"Running AqwaReader for {input_file } ... START\n")        
        args = self.get_args_for_data(input_file, cfg, aqwa_fundamental_cfg)
        stdout_process = subprocess.Popen(args,
                           stdout=subprocess.PIPE, 
                           stderr=subprocess.PIPE)
        out, err = stdout_process.communicate()
        fundamental_result = self.process_aqwa_fundamental_result(out)

        save_csv = result_item.get('save_csv', False)
        if save_csv:
            args = self.get_args_for_data(input_file, cfg, aqwa_fundamental_cfg, result_format='csv')
            csv_process = subprocess.Popen(args,
                           stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            csv_out, err = stdout_process.communicate()

        # code = subprocess.call(args, stdout = ts, stderr = ts)
        logging.debug(f"Returned {err} \n")
        logging.debug(f"Running AqwaReader for {input_file } ... COMPLETE\n")

    def get_args_for_data(self, input_file, cfg, aqwa_fundamental_cfg, result_format='stdout'):
        basename = Path(input_file).stem
        csv_output = basename + aqwa_fundamental_cfg['file_suffix']
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
        argVals.append("{:d}".format(aqwa_fundamental_cfg['plt'][0]))

        argStrings.append("--PLT2")
        argVals.append("{:d}".format(aqwa_fundamental_cfg['plt'][1]))

        argStrings.append("--PLT3")
        argVals.append("{:d}".format(aqwa_fundamental_cfg['plt'][2]))

        argStrings.append("--PLT4")
        argVals.append("{:d}".format(aqwa_fundamental_cfg['plt'][3]))
        
        for (string, val) in zip(argStrings, argVals):
            args.extend([string, val])
        return args


    def inject_to_excel(self, df, result_item, cfg):
        if 'inject_into' in result_item and result_item[
                'inject_into']['flag']:
            inject_into_file = result_item['inject_into']['filename']
            file_name = os.path.join(cfg['Analysis']['analysis_root_folder'],
                        inject_into_file)
            if not os.path.isfile(file_name):
                raise Exception(f"Inject Into File {file_name} not found for writing summary data")

            sheetname = result_item['inject_into']['sheetname']

            cfg_save_to_existing_workbook = {'template_file_name': file_name, 'sheetname': sheetname, 'saved_file_name': file_name, 'if_sheet_exists': 'replace', 'df': df}
            save_data.df_to_sheet_in_existing_workbook(cfg_save_to_existing_workbook)
