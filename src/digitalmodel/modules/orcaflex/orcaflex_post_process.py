# Standard library imports
import copy
import logging
import math

# Third party imports
import pandas as pd

try:
    # Third party imports
    import OrcFxAPI
except:
    logging.debug("OrcFxAPI not available")


# Third party imports
from assetutilities.common.update_deep import update_deep_dictionary

# Reader imports
from digitalmodel.modules.orcaflex.opp import OrcaFlexAnalysis
from digitalmodel.modules.orcaflex.opp_visualization import OPPVisualization
from digitalmodel.modules.orcaflex.orcaflex_utilities import OrcaflexUtilities

ou = OrcaflexUtilities()
ofa = OrcaFlexAnalysis()

opp_visualization = OPPVisualization()

class orcaflex_post_process:

    def __init__(self):
        pass

    def post_process_router(self, cfg):

        orcaflex_license_flag = ou.is_orcaflex_available()
        assert (orcaflex_license_flag)
        if not orcaflex_license_flag:
            raise Exception("Orcaflex license not available.")

        cfg = self.get_cfg_with_master_data(cfg)
        if cfg['orcaflex']['postprocess']['summary']['flag'] or cfg[
                    'orcaflex']['postprocess']['RangeGraph']['flag'] or cfg[
                        'orcaflex']['postprocess']['time_series'][
                            'flag'] or cfg['orcaflex']['postprocess'][
                                'cummulative_histograms']['flag']:
            post_process_data_flag = True
        else:
            post_process_data_flag = False

        if cfg['orcaflex']['postprocess']['visualization']['flag']:
            post_process_visualization_flag = True
        else:
            post_process_visualization_flag = False

        if post_process_data_flag:
            cfg.update({cfg['basename']: {}})
            ofa.post_process(cfg)
            ofa.save_summary(cfg)
        elif post_process_visualization_flag:
            opp_visualization.get_visualizations(cfg)
        else:
            logging.info("No postprocess option to run specified ... End Run.")

        return cfg

    def get_cfg_with_master_data(self, cfg):
        if 'summary_settings_master' in cfg:
            summary_settings_master = cfg['summary_settings_master'].copy()
            summary_settings = cfg['summary_settings']

            for group_index in range(0, len(summary_settings['groups'])):
                group = summary_settings['groups'][group_index].copy()

                if 'Columns' in summary_settings_master['groups'][0]:
                    for column_index in range(0, len(group['Columns'])):
                        column = group['Columns'][column_index].copy()
                        column = update_deep_dictionary(
                            summary_settings_master['groups'][group_index]['Columns'][0], column)
                        group['Columns'][column_index] = copy.deepcopy(column)

                group = update_deep_dictionary(summary_settings_master['groups'][group_index], group)
                summary_settings['groups'][group_index] = copy.deepcopy(group)

            cfg['summary_settings'] = copy.deepcopy(summary_settings)

        if 'time_series_settings_master' in cfg:
            time_series_settings_master = cfg['time_series_settings_master'].copy()
            time_series_settings = cfg['time_series_settings']

            for group_index in range(0, len(time_series_settings['groups'])):
                group = time_series_settings['groups'][group_index].copy()

                if 'Columns' in time_series_settings_master['groups'][0]:
                    for column_index in range(0, len(group['Columns'])):
                        column = group['Columns'][column_index].copy()
                        column = update_deep_dictionary(
                            time_series_settings_master['groups'][group_index]['Columns'][0], column)
                        group['Columns'][column_index] = copy.deepcopy(column)

                group = update_deep_dictionary(time_series_settings_master['groups'][group_index], group)
                time_series_settings['groups'][group_index] = copy.deepcopy(group)

            cfg['time_series_settings'] = copy.deepcopy(time_series_settings)

        return cfg

