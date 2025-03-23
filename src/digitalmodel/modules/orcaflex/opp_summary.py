# Standard library imports
import copy
import math
import os
from pathlib import Path

# Third party imports
import pandas as pd
from assetutilities.common.data import SaveData

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

    def get_summary_for_file(self, cfg: dict, model_dict: dict, file_name: str) -> None:
        summary_groups = cfg['summary_settings']['groups']
        summary_groups_for_file = {}
        for summary_group in summary_groups:
            summary_group_label = summary_group['Label']

            df_columns = ['fe_filename', 'run_status', 'stop_time', 'description', 'statistic']
            run_status = model_dict['run_status']
            stop_time = model_dict['stop_time']
            file_name_for_output = str(Path(file_name).resolve()).replace('\\', '/')
            result_array = [file_name_for_output, run_status, stop_time, None, None]
            df = pd.DataFrame([result_array], columns=df_columns)

            for summary_cfg in summary_group['Columns']:
                summary = self.get_summary_from_orcaflex_run(model_dict, summary_cfg)
                df[summary['variables']] = summary['values']

            summary_groups_for_file.update({summary_group_label: df})

        return summary_groups_for_file

    def get_summary_from_orcaflex_run(self, model_dict, summary_cfg):
        variables = []
        values = []
        value = self.process_summary_by_model_and_cfg_item(model_dict, summary_cfg)

        variables.append(summary_cfg['Label'])
        values.append(value)
        summary = {'variables': variables, 'values': values}

        return summary

    def add_file_result_to_all_results(self, summary: dict, summary_groups_for_file: dict) -> None:
        if not summary:
            summary = copy.deepcopy(summary_groups_for_file)
        else:
            for key in summary_groups_for_file.keys():
                summary[key] = pd.concat([summary[key], summary_groups_for_file[key]], ignore_index=True)

        return summary

    def save_summary(self, summary, cfg):
        if not summary:
            return

        csv_decimal = 6
        if 'csv_decimal' in cfg.orcaflex['postprocess']['summary']:
            csv_decimal = cfg.orcaflex['postprocess']['summary']['csv_decimal']

        summary_array = []
        for key in summary.keys():
            df = summary[key]
            file_name = os.path.join(cfg['Analysis']['result_folder'],
                    cfg['Analysis']['file_name'] + '_' + key + '.csv')

            statistics_cfg = cfg['orcaflex']['postprocess']['summary']['statistics']
            statistics_flag = None
            if statistics_cfg['Minimum'] or statistics_cfg['Maximum'] or statistics_cfg['Mean'] or statistics_cfg['StdDev']:
                statistics_flag = True
            if statistics_flag:
                df = ou.add_basic_statistics_to_df(df)

            df.round(csv_decimal).to_csv(file_name, index=False)

            summary_array.append({'data': file_name, 'label': key})

        cfg[cfg['basename']] = {'summary': {'groups': summary_array}}

    def process_summary_by_model_and_cfg_item(self, model_dict, cfg_item):
        RangeDF = pd.DataFrame()

        OrcFXAPIObject,TimePeriod,arclengthRange,objectExtra, VariableName, Statistic_Type = of_objects.get_orcaflex_objects(model_dict, cfg_item)

        if cfg_item['Command'] == 'Range Graph':
            output = opp_rg.get_RangeGraph(OrcFXAPIObject, TimePeriod, VariableName,
                       arclengthRange, objectExtra)

            AdditionalDataName = 'X'
            RangeDF[AdditionalDataName] = output.X

            output_value = self.get_additional_data(cfg_item, RangeDF, VariableName,
                                                    output, Statistic_Type)
        elif cfg_item['Command'] == 'TimeHistory':
            output = opp_ts.get_TimeHistory(OrcFXAPIObject, TimePeriod, objectExtra, VariableName)

            if OrcFXAPIObject is not None and output is not None:
                output_value = self.get_additional_data(cfg_item, RangeDF, VariableName,
                                                        output, Statistic_Type)
            else:
                output_value = None
        elif cfg_item['Command'] in ['Static Result', 'StaticResult']:
            output_value = self.get_StaticResult(OrcFXAPIObject, VariableName,
                                                 objectExtra)
        elif cfg_item['Command'] in ['GetData', 'Get Data']:
            OrcFXAPIObject, VariableName = of_objects.get_input_data_variable_name(cfg_item, OrcFXAPIObject, VariableName)
            output_value = self.get_input_data(OrcFXAPIObject, VariableName)

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

    def get_StaticResult(self, OrcFXAPIObject, VariableName, objectExtra=None):
        output = OrcFXAPIObject.StaticResult(VariableName, objectExtra)

        return output

    def get_input_data(self, OrcFXAPIObject, VariableName):
        if 'RefCurrent' in VariableName[0]:
            output_value = getattr(OrcFXAPIObject, VariableName[0])
        else:
            output_value = OrcFXAPIObject.GetData(VariableName[0], VariableName[1])

        return output_value

