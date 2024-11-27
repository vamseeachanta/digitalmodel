# Standard library imports
import copy
import logging
import math
import os

# Third party imports
import pandas as pd
import numpy as np

try:
    # Third party imports
    import OrcFxAPI
except:
    logging.debug("OrcFxAPI not available")


# Third party imports
from assetutilities.common.data import PandasChainedAssignent, SaveData, TransformData
from assetutilities.common.update_deep import update_deep_dictionary

# Reader imports
from digitalmodel.modules.orcaflex.opp_range_graph import OPPRangeGraph
from digitalmodel.modules.orcaflex.opp_summary import OPPSummary
from digitalmodel.modules.orcaflex.opp_time_series import OPPTimeSeries
from digitalmodel.modules.orcaflex.opp_visualization import OPPVisualization
from digitalmodel.modules.orcaflex.orcaflex_objects import OrcaFlexObjects
from digitalmodel.modules.orcaflex.orcaflex_utilities import OrcaflexUtilities
save_data = SaveData()

ou = OrcaflexUtilities()
of_objects = OrcaFlexObjects()
opp_summary = OPPSummary()
opp_ts = OPPTimeSeries()
opp_rg = OPPRangeGraph()
opp_visualization = OPPVisualization()

class OrcaFlexPostProcess():

    def __init__(self, cfg=None):
        self.cfg = cfg
        self.cfg_array = []
        self.get_model_state_information()
        self.data_quality = {}
        self.RAO_df_array = []

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
            self.post_process(cfg)
        elif post_process_visualization_flag:
            opp_visualization.get_visualizations(cfg)
        else:
            logging.info("No postprocess option to run specified ... End Run.")

        return cfg

    def get_cfg_with_master_data(self, cfg):
        if 'summary_settings_master' in cfg:
            summary_settings_master = cfg['summary_settings_master'].copy()
            summary_settings_master_keys = summary_settings_master.keys()
            summary_settings_master_non_groups = summary_settings_master.copy()
            if 'groups' in summary_settings_master_keys:
                summary_settings_master_non_groups.pop('groups')
            cfg['summary_settings'] = update_deep_dictionary(summary_settings_master_non_groups, cfg['summary_settings'])
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

    def get_model_state_information(self):
        if self.cfg is not None and self.cfg['default'].__contains__(
                'model_state_information'):
            if self.cfg['default']['model_state_information']['flag']:
                if self.cfg['default']['model_state_information'].__contains__(
                        'from_csv'):
                    file_name = self.cfg['default']['model_state_information'][
                        'from_csv']['io']
                    attribute = self.cfg['default']['model_state_information'][
                        'from_csv']['label']
                    setattr(self, attribute, pd.read_csv(file_name))


    def post_process_files(self, cfg):
        self.post_process(cfg)
        print("Successful post-process {0} sim files".format(
            len(self.RangeAllFiles)))
        if self.cfg.default['Analysis']['time_series']['flag']:
            opp_ts.router(cfg)

        self.save_summary(cfg)
        self.process_range_graphs(cfg)
        self.save_cfg_files_from_multiple_files(cfg)

    def post_process(self, cfg):

        self.load_matrix = ou.get_load_matrix_with_filenames(cfg)
        # Intialize output arrays
        RangeAllFiles = []
        histogram_all_files = []

        sim_files = cfg.file_management['input_files']['sim']
        cfg[cfg['basename']]['time_series'] = []

        for fileIndex in range(0, len(sim_files)):
            file_name = sim_files[fileIndex]
            model = ou.get_model_from_filename(file_name=file_name, load_matrix=self.load_matrix)
            FileDescription = 'Description'
            FileObjectName = 'Dummy_Object'
            histogram_for_file = [[]] * len(cfg.time_series_settings['groups'])

            self.fileIndex = fileIndex
            print("Post-processing file: {}".format(file_name))
            try:
                RangeAllFiles.append(
                    opp_rg.postProcessRange(model, self.cfg, FileObjectName))
            except:
                RangeAllFiles.append(None)
            if cfg['orcaflex']['postprocess']['time_series']['flag']:
                if cfg['time_series_settings']['data']: 
                    time_series_cfg_output_for_file = opp_ts.get_time_series_data(cfg, model, file_name)
                    cfg_output = {'time_series': time_series_cfg_output_for_file, 'file_name': file_name}
                    cfg[cfg['basename']]['time_series'].append(cfg_output)
            else:
                pass

            histogram_all_files.append(histogram_for_file)
            RangeAllFiles.append(None)

        self.HistogramAllFiles = histogram_all_files
        self.RangeAllFiles = RangeAllFiles

        opp_summary.process_summary(cfg)
