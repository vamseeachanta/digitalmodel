# Standard library imports
import os
from typing import Any, Dict

# Third party imports
import pandas as pd


# Reader imports
from digitalmodel.modules.orcaflex.orcaflex_objects import OrcaFlexObjects

of_objects = OrcaFlexObjects()

class OPPLinkedStatistics():
    
    def __init__(self) -> None:
        pass

    def get_linked_statistics(self, cfg: Dict[str, Any], model, file_name) -> Dict[str, Any]:
        ls_groups = cfg['linked_statistics_settings']['groups']

        linked_statistics_for_file = {}
        for ls_group in ls_groups:
            ls_group_label = ls_group['Label']
            df_columns = ['fe_filename', 'Label']
            df = pd.DataFrame()
            for ls_cfg in ls_group['Columns']:
                ls_label = ls_cfg['Label']
                result_array = [file_name, ls_label]
                linked_statistics = self.get_linked_statistics_from_orcaflex_run(model, ls_cfg)
                df_columns = df_columns + linked_statistics['variables']
                result_array = result_array + linked_statistics['values']
                result_df = pd.DataFrame([result_array], columns=df_columns)
                df = pd.concat([df, result_df], ignore_index=True)

            linked_statistics_for_file.update({ls_group_label: df})

        return linked_statistics_for_file

    def get_linked_statistics_from_orcaflex_run(self, model, ls_cfg) -> Dict[str, Any]:

        OrcFXAPIObject,TimePeriod,arclengthRange,objectExtra, VariableName, Statistic_Type = of_objects.get_orcaflex_objects(model, ls_cfg)

        stats = OrcFXAPIObject.LinkedStatistics(VariableName, TimePeriod, objectExtra)

        max_variables = []
        max_values = []
        min_variables = []
        min_values = []

        for idx in range(0, len(VariableName)-1):
            query = stats.Query(VariableName[0], VariableName[idx+1])

            if idx == 0:
                max_variables.append(VariableName[0] + '_' + 'ValueAtMax')
                max_variables.append(VariableName[0] + '_' + 'TimeOfMax')
                max_values.append(query.ValueAtMax)
                max_values.append(query.TimeOfMax)

            max_variables.append(VariableName[idx+1] + '_' + 'LinkedValueAtMax')
            max_values.append(query.LinkedValueAtMax)

            if idx == 0:
                min_variables.append(VariableName[0] + '_' + 'ValueAtMin')
                min_variables.append(VariableName[0] + '_' + 'TimeOfMin')
                min_values.append(query.ValueAtMin)
                min_values.append(query.TimeOfMin)

            min_variables.append(VariableName[idx+1] + '_' + 'LinkedValueAtMin')
            min_values.append(query.LinkedValueAtMin)

        variables = max_variables + min_variables
        values = max_values + min_values

        linked_statistics =  { 'variables': variables, 'values': values }

        return linked_statistics

    def add_file_result(self, linked_statistics, linked_statistics_for_file):
        if not linked_statistics:
            linked_statistics = linked_statistics_for_file
        else:
            for key in linked_statistics_for_file.keys():
                linked_statistics[key] = pd.concat([linked_statistics[key], linked_statistics_for_file[key]], ignore_index=True)
        return linked_statistics

    def save_linked_statistics(self, linked_statistics, cfg):
        if not linked_statistics:
            return

        csv_decimal = 6
        if 'csv_decimal' in cfg.orcaflex['postprocess']['linked_statistics']:
            csv_decimal = cfg.orcaflex['postprocess']['linked_statistics']['csv_decimal']

        linked_statistics_array = []
        for key in linked_statistics.keys():
            file_name = cfg['Analysis']['file_name_for_overwrite'] + '_' + key + '.csv'
            file_name_with_path = os.path.join(cfg["Analysis"]['result_folder'], file_name)
            df = linked_statistics[key]
            df.round(csv_decimal).to_csv(file_name_with_path, index=False)

            linked_statistics_array.append({'data':file_name_with_path})

        cfg[cfg['basename']] = {'linked_statistics': {'groups': linked_statistics_array}}

