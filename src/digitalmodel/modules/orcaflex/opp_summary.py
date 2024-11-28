# Standard library imports
import logging
import math
import os

# Third party imports
import numpy as np
import pandas as pd
from assetutilities.common.data import SaveData, TransformData

# Reader imports
from digitalmodel.modules.orcaflex.opp_range_graph import OPPRangeGraph
from digitalmodel.modules.orcaflex.opp_time_series import OPPTimeSeries
from digitalmodel.modules.orcaflex.orcaflex_objects import OrcaFlexObjects
from digitalmodel.modules.orcaflex.orcaflex_utilities import OrcaflexUtilities

save_data = SaveData()
ou = OrcaflexUtilities()
of_objects = OrcaFlexObjects()
opp_ts = OPPTimeSeries()
opp_rg = OPPRangeGraph()

class OPPSummary():

    def __init__(self) -> None:
        pass


    def get_summary_for_file(self, cfg: dict, model: object, file_name: str) -> None:
        summary_groups = cfg['summary_settings']['groups']
        summary_groups_for_file = {}
        for summary_group in summary_groups:
            summary_group_label = summary_group['Label']
            df_columns = ['fe_filename', 'Label']
            df = pd.DataFrame()
            for summary_cfg in summary_group['Columns']:
                summary_label = summary_cfg['Label']
                result_array = [file_name, summary_label]
                summary = self.get_summary_from_orcaflex_run(model, summary_cfg)
                df_columns = df_columns + summary['variables']
                result_array = result_array + summary['values']
                result_df = pd.DataFrame([result_array], columns=df_columns)
                df = pd.concat([df, result_df], ignore_index=True)
  
            summary_groups_for_file.update({summary_group_label: df})

    def get_summary_from_orcaflex_run(self, model, summary_cfg):
        variables = []
        values = []
        value = self.process_summary_by_model_and_cfg_item(model, summary_cfg)
        
        variables.append(summary_cfg['Label'])
        values.append(value)
        summary = {'variables': variables, 'values': values}

        return summary
        
        


    def process_summary(self, cfg):
        if 'summary_settings' in cfg: 
            SummaryDFAllFiles = self.process_summary_groups(cfg)
            self.SummaryDFAllFiles = SummaryDFAllFiles
            self.save_summary(cfg, self.SummaryDFAllFiles, self.load_matrix)



    def process_summary_groups(self, cfg):

        summary_cfg = cfg['summary_settings'].copy()
        SummaryDFAllFiles = [pd.DataFrame()] * len(
            summary_cfg['groups'])

        for SummaryIndex in range(0, len(cfg['summary_settings']['groups'])):
            if 'filename_pattern' in cfg['summary_settings']['groups'][SummaryIndex]:
                filename_pattern = cfg['summary_settings']['groups'][SummaryIndex]['filename_pattern']
                if filename_pattern is not None:
                    cfg['file_management']['files']['files_in_current_directory']['filename_pattern'] = filename_pattern

            if 'directory' in cfg['summary_settings']['groups'][SummaryIndex]:
                directory = cfg['summary_settings']['groups'][SummaryIndex]['directory']
                if directory is not None:
                    cfg['file_management']['files']['files_in_current_directory']['directory'] = directory

            ou.file_management(cfg)
            self.load_matrix = ou.get_load_matrix_with_filenames(cfg)

            sim_files = cfg.file_management['input_files']['sim']

            for fileIndex in range(0, len(sim_files)):
                file_name = sim_files[fileIndex]
                self.fileIndex = fileIndex
                model = ou.get_model_from_filename(file_name=file_name, load_matrix=self.load_matrix)
                FileDescription = 'Description'
                FileObjectName = 'Dummy_Object'
                print("Post-processing file: {}".format(file_name))

                SimulationFileName = ou.get_SimulationFileName(file_name)

                try:
                    SummaryDFAllFiles[SummaryIndex] = self.postProcessSummary(
                            model, SummaryDFAllFiles[SummaryIndex], SummaryIndex,
                            FileDescription, FileObjectName, SimulationFileName,
                            fileIndex, cfg)
                except Exception as e:
                    logging.info(str(e))
                    raise Exception("Error in post processing")

        return SummaryDFAllFiles


    def postProcessSummary(self, model, SummaryDF, SummaryIndex,
                           FileDescription, FileObjectName, FileName, fileIndex,
                           cfg):
        summary_group_cfg = cfg['summary_settings']['groups'][SummaryIndex]
        if SummaryDF.empty:
            columns = self.get_summary_df_columns(summary_group_cfg)
            pd.options.mode.chained_assignment = None
            SummaryDF = pd.DataFrame(columns=columns)
            pd.reset_option('mode.chained_assignment')

        summary_from_sim_file = []
        summary_from_sim_file.append(FileName)
        summary_from_sim_file.append(FileDescription)

        loading_condition_array = self.get_loading_condition_array(fileIndex)

        if model is not None:
            for SummaryColumnIndex in range(0,
                                            len(summary_group_cfg['Columns'])):
                summary_group_item_cfg = summary_group_cfg['Columns'][
                    SummaryColumnIndex]
                try:
                    output_value = self.process_summary_by_model_and_cfg_item(
                        model, summary_group_item_cfg)
                    summary_from_sim_file.append(output_value)
                except Exception as e:
                    summary_from_sim_file.append(None)
                    print(str(e))
                    print(f"Summary Group {summary_group_item_cfg}  in post processing")

            if np.nan in summary_from_sim_file:
                print("Summary Incomplete for : '{}' in simulation file: '{}'".
                      format(summary_group_cfg['SummaryFileName'],
                             FileDescription))
                if (ArcLengthArray is None):
                    print(
                        "          Arc length is {}. Define appropriate arc length value or range"
                        .format(ArcLengthArray))
                elif (len(ArcLengthArray) == 0):
                    print(
                        "          Arc length is {}. Define appropriate arc length value or range"
                        .format(ArcLengthArray))
                elif len(ArcLengthArray) == 1:
                    print(
                        "          Node may not be in arc length exact position for Arc length {}. Provide a range of approx. element length"
                        .format(ArcLengthArray))
                elif len(ArcLengthArray) == 2:
                    print(
                        "          Node may not be in arc length range position for Arc length {}. Check range and fe element length"
                        .format(ArcLengthArray))
                print(
                    "          (or) Simulation failed for : {} before postprocess Time {}"
                    .format(FileDescription, SimulationPeriod))
        else:
            summary_from_sim_file = summary_from_sim_file + [None] * len(
                summary_group_cfg['Columns'])

        pd.options.mode.chained_assignment = None
        SummaryDF.loc[len(
            SummaryDF)] = loading_condition_array + summary_from_sim_file
        pd.reset_option('mode.chained_assignment')

        return SummaryDF

    def get_loading_condition_array(self, file_index):
        if self.load_matrix is None:
            loading_condition_array = []
        else:
            loading_condition_array = self.load_matrix.iloc[file_index].to_list(
            )
        return loading_condition_array


    def process_summary_by_model_and_cfg_item(self, model, cfg_item):
        RangeDF = pd.DataFrame()

        OrcFXAPIObject,TimePeriod,arclengthRange,objectExtra, VariableName, Statistic_Type = of_objects.get_orcaflex_objects(model, cfg_item)

        if cfg_item['Command'] == 'Range Graph':
            output = opp_rg.get_RangeGraph(OrcFXAPIObject, TimePeriod, VariableName,
                       arclengthRange, objectExtra)

            AdditionalDataName = 'X'
            RangeDF[AdditionalDataName] = output.X

            output_value = self.get_additional_data(cfg_item, RangeDF, VariableName,
                                                    output, Statistic_Type)
        elif cfg_item['Command'] == 'TimeHistory':
            output = opp_ts.get_TimeHistory(OrcFXAPIObject, TimePeriod, objectExtra, VariableName)

            output_value = self.get_additional_data(cfg_item, RangeDF, VariableName,
                                                    output, Statistic_Type)
        elif cfg_item['Command'] in ['Static Result', 'StaticResult']:
            output_value = self.get_StaticResult(OrcFXAPIObject, VariableName,
                                                 objectExtra)
        elif cfg_item['Command'] in ['GetData', 'Get Data']:
            output_value = self.get_input_data(OrcFXAPIObject, VariableName,
                                               model)

        return output_value


    def get_summary_df_columns(self, summary_group_cfg):
        if self.load_matrix is None:
            load_matrix_columns = []
        else:
            load_matrix_columns = self.load_matrix.columns.to_list()

        columns = ['FileName', 'Description']
        summary_columns = self.get_summary_value_columns(summary_group_cfg)
        df_columns = load_matrix_columns + columns + summary_columns

        return df_columns

    def get_summary_value_columns(self, summary_group_cfg):
        summary_columns = []
        for item in summary_group_cfg['Columns']:
            if type(item['Label']) == list:
                summary_columns = summary_columns + item['Label']
            else:
                summary_columns.append(item['Label'])
        return summary_columns


    def save_summary(self, cfg, SummaryDFAllFiles, load_matrix):
        if not cfg.orcaflex['postprocess']['summary']['flag']:
            print("No analysis summary per user request")
            return None

        summary_groups = cfg['summary_settings']['groups']
        SummaryFileNameArray = []
        for SummaryIndex in range(0, len(summary_groups)):
            summary_group_cfg = summary_groups[SummaryIndex]
            SummaryFileName = summary_group_cfg['SummaryFileName']
            summary_value_columns = self.get_summary_value_columns(
                summary_group_cfg)
            summary_column_count = len(summary_value_columns)
            SummaryFileNameArray.append(SummaryFileName)
            SummaryDF = SummaryDFAllFiles[SummaryIndex]
            summaryDF_temp = SummaryDF.iloc[:, (
                len(SummaryDF.columns) -
                summary_column_count):len(SummaryDF.columns)]

            loadng_condition_array = []
            if load_matrix is not None:
                loadng_condition_array = [None] * len(load_matrix.columns)

            if len(summaryDF_temp) > 0:
                if 'AddMeanToSummary' in cfg['summary_settings'].keys():
                    summaryDF_temp_numeric = summaryDF_temp.apply(pd.to_numeric, errors='coerce')
                    result_array = ['Mean', 'Mean'
                                   ] + summaryDF_temp_numeric.mean(axis=0).tolist()
                    SummaryDF.loc[len(
                        SummaryDF)] = loadng_condition_array + result_array
                if 'AddMinimumToSummary' in cfg['summary_settings'].keys():
                    result_array = ['Minimum', 'Minimum'
                                   ] + summaryDF_temp_numeric.min(axis=0).tolist()
                    SummaryDF.loc[len(
                        SummaryDF)] = loadng_condition_array + result_array
                if 'AddMaximumToSummary' in cfg['summary_settings'].keys():
                    result_array = ['Maximum', 'Maximum'
                                   ] + summaryDF_temp_numeric.max(axis=0).tolist()
                    SummaryDF.loc[len(
                        SummaryDF)] = loadng_condition_array + result_array

            try:
                decimalArray = pd.Series([0, 0, 0, 2, 0],
                                         index=SummaryDF.columns.values)
                SummaryDF = SummaryDF.round(decimalArray)
            except Exception:
                SummaryDF = SummaryDF.round(2)

        self.save_summary_to_csv(SummaryFileNameArray, cfg)
        self.saveSummaryToNewExcel(SummaryFileNameArray, cfg)
        self.injectSummaryToNewExcel(SummaryFileNameArray, cfg)

        cfg[cfg['basename']].update({'summary_groups': len(summary_groups)})
        print(f"Processed summary files: {len(summary_groups)}")


    def save_summary_to_csv(self, SummaryFileNameArray, cfg):
        cfg[cfg['basename']].update({'summary': {}})

        for group_idx in range(0, len(SummaryFileNameArray)):
            df = self.SummaryDFAllFiles[group_idx]
            file_name = os.path.join(cfg['Analysis']['result_folder'],
                    cfg['Analysis']['file_name'] + '_' + SummaryFileNameArray[group_idx] + '.csv')

            df.to_csv(file_name, index=False)

            result_dict = {SummaryFileNameArray[group_idx]: df.to_dict()}
            cfg[cfg['basename']]['summary'].update(result_dict)


    def injectSummaryToNewExcel(self, SummaryFileNameArray, cfg):
        for group_idx in range(0, len(SummaryFileNameArray)):
            summary_group_cfg = cfg['summary_settings']['groups'][group_idx]
            if 'inject_into' in summary_group_cfg and summary_group_cfg[
                    'inject_into']['flag']:
                inject_into_file = summary_group_cfg['inject_into']['filename']
                file_name = os.path.join(cfg['Analysis']['analysis_root_folder'],
                            inject_into_file)
                if not os.path.isfile(file_name):
                    raise Exception(f"Inject Into File {file_name} not found for writing summary data")

                sheetname = summary_group_cfg['inject_into']['sheetname']
                if sheetname is None:
                    sheetname = SummaryFileNameArray[group_idx]

                df = self.SummaryDFAllFiles[group_idx]
                cfg_save_to_existing_workbook = {'template_file_name': file_name, 'sheetname': sheetname, 'saved_file_name': file_name, 'if_sheet_exists': 'replace', 'df': df}
                save_data.df_to_sheet_in_existing_workbook(cfg_save_to_existing_workbook)



    def saveSummaryToNewExcel(self, SummaryFileNameArray, cfg):
        if len(self.SummaryDFAllFiles) > 0:
            customdata = {
                "FileName":
                    os.path.join(cfg['Analysis']['result_folder'],
                    cfg['Analysis']['file_name'] + '.xlsx'),
                "SheetNames":
                    SummaryFileNameArray,
                "thin_border":
                    True
            }
            save_data.DataFrameArray_To_xlsx_openpyxl(self.SummaryDFAllFiles,
                                                      customdata)

    def get_additional_data(self, cfg, RangeDF, VariableName, output,
                            Statistic_Type):
        if cfg['Command'] == 'Range Graph':
            RangeDF[VariableName] = getattr(output, Statistic_Type)
            if VariableName == "API STD 2RD Method 1":
                RangeDF[VariableName] = [
                    math.sqrt(x) for x in RangeDF[VariableName]
                ]
            if Statistic_Type == "Max":
                output_value = RangeDF[VariableName].max()
            elif Statistic_Type == "Min":
                output_value = RangeDF[VariableName].min()
            elif Statistic_Type == "Mean":
                output_value = RangeDF[VariableName].mean()
        else:
            if Statistic_Type == "Max":
                output_value = output.max()
            elif Statistic_Type == "Min":
                output_value = output.min()
            elif Statistic_Type == "Mean":
                output_value = output.mean()

        if cfg.__contains__('transform'):
            trans_cfg = cfg['transform']
            trans_cfg['data'] = output_value
            transformed_cfg = self.transform_output(trans_cfg)
            output_value = transformed_cfg['data']

        return output_value


    def transform_output(self, cfg):
        trans_data = TransformData()
        trans_data.get_transformed_data(cfg)

        return cfg



    def get_StaticResult(self, OrcFXAPIObject, VariableName, objectExtra=None):
        output = OrcFXAPIObject.StaticResult(VariableName, objectExtra)

        return output


    def get_input_data(self, OrcFXAPIObject, VariableName, model):
        if 'Current' in VariableName[0]:
            ActiveCurrent = OrcFXAPIObject.GetData('ActiveCurrent', -1)
            model.environment.SelectedCurrent = ActiveCurrent
            output_value = model.environment.GetData(VariableName[0], VariableName[1])
        else:
            output_value = OrcFXAPIObject.GetData(VariableName[0], VariableName[1])

        return output_value

