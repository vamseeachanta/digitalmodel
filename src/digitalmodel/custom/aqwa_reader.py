import os
import math
import logging
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
        au.file_management(cfg)
        for result_item in cfg['result']:
            df = self.get_data(cfg, result_item)
            self.injectSummaryToExcel(df, result_item, cfg)

    def get_aqwareader_exe(self, cfg):
        ANSYSInstallDir = cfg['software']['ANSYSInstallDir']
        aqwareader_exe = os.path.join(ANSYSInstallDir, "aisol", "bin", "winx64", "AqwaReader.exe")
        # if not os.path.isfile(aqwareader_exe):
        #     raise Exception(f"AqwaReader.exe not found in {aqwareader_exe}")

        self.aqwareader_exe = aqwareader_exe

    def get_data(self, cfg, result_item):
        input_files = cfg['file_management']['input_files']['PLT']

        df_all_files = pd.DataFrame()
        for input_file in input_files:
            self.get_result_groups(input_file, cfg, result_item)

        return df_all_files

    def get_result_groups(self, input_file, cfg, result_item):
        for result_group in result_item['groups']:
            self.get_result_group(input_file, cfg, result_group)

    def get_result_group(self, input_file, cfg, result_group):
        basename = Path(input_file).stem
        csv_output = basename + result_group['file_suffix']
                
        argStrings = []
        argVals = []
        args = [self.aqwareader_exe]

        argStrings.append("--Type")
        argStrings.append("--InFile")
        argStrings.append("--OutFile")
        argStrings.append("--Format")
        argStrings.append("--PLT1")
        argStrings.append("--PLT2")
        argStrings.append("--PLT3")
        argStrings.append("--PLT4")
        
        argVals.append("Graphical")
        argVals.append("{:s}".format(inputFile))
        argVals.append("{:s}".format(os.path.join(workdir, csv_output)))
        argVals.append("csv")
        argVals.append("{:d}".format(resultSet[1]))
        argVals.append("{:d}".format(resultSet[2]))
        argVals.append("{:d}".format(resultSet[3]))
        argVals.append("{:d}".format(resultSet[4]))
        
        for (string, val) in zip(argStrings, argVals):
            args.extend([string, val])
        
        code = subprocess.call(args, stdout = ts, stderr = ts)
        ts.write("Returned " + str(code) + "\n")


    def injectSummaryToExcel(self, df, result_item, cfg):
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
