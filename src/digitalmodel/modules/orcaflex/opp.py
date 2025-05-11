# Standard library imports
import copy
import logging

# Third party imports
# Third party imports
from assetutilities.common.update_deep import update_deep_dictionary
from assetutilities.common.data import PandasChainedAssignent
from digitalmodel.modules.orcaflex.opp_linkedstatistics import OPPLinkedStatistics

# Reader imports
from digitalmodel.modules.orcaflex.opp_range_graph import OPPRangeGraph
from digitalmodel.modules.orcaflex.opp_summary import OPPSummary
from digitalmodel.modules.orcaflex.opp_linkedstatistics import OPPLinkedStatistics
from digitalmodel.modules.orcaflex.opp_time_series import OPPTimeSeries
from digitalmodel.modules.orcaflex.opp_visualization import OPPVisualization
from digitalmodel.modules.orcaflex.orcaflex_objects import OrcaFlexObjects
from digitalmodel.modules.orcaflex.orcaflex_utilities import OrcaflexUtilities

ou = OrcaflexUtilities() #noqa
of_objects = OrcaFlexObjects()
opp_summary = OPPSummary()
opp_ls = OPPLinkedStatistics()
opp_ts = OPPTimeSeries()
opp_rg = OPPRangeGraph()
opp_visualization = OPPVisualization()

class OrcaFlexPostProcess():

    def __init__(self, cfg=None):
        pass


    def post_process_router(self, cfg):

        orcaflex_license_flag = ou.is_orcaflex_available()
        assert (orcaflex_license_flag)
        if not orcaflex_license_flag:
            raise Exception("Orcaflex license not available.")

        cfg = self.get_cfg_with_master_data(cfg)

        post_process_data_flag = False
        if cfg['orcaflex']['postprocess']['summary']['flag']:
            post_process_data_flag = True
        if cfg['orcaflex']['postprocess']['linked_statistics']['flag']:
            post_process_data_flag = True
        if cfg['orcaflex']['postprocess']['RangeGraph']['flag']:
            post_process_data_flag = True
        if cfg['orcaflex']['postprocess']['time_series']['flag']:
            post_process_data_flag = True
        if cfg['orcaflex']['postprocess']['cummulative_histograms']['flag']:
            post_process_data_flag = True

        post_process_visualization_flag = False
        if cfg['orcaflex']['postprocess']['visualization']['flag']:
            post_process_visualization_flag = True

        if post_process_data_flag:
            cfg.update({cfg['basename']: {}})
            self.post_process(cfg)

        if post_process_visualization_flag:
            opp_visualization.get_visualizations(cfg)

        if not post_process_data_flag and not post_process_visualization_flag:
            logging.info("No postprocess option to run specified ... End Run.")

        return cfg

    def get_cfg_with_master_data(self, cfg):
        self.get_cfg_with_summary_master_data(cfg)

        self.get_cfg_with_time_series_master_data(cfg)

        self.get_cfg_with_linked_statistics_master_data(cfg)

        return cfg

    def post_process(self, cfg):

        load_matrix = ou.get_load_matrix_with_filenames(cfg)
        # Intialize output arrays
        RangeAllFiles = []
        histogram_all_files = []
        linked_statistics = None
        summary = None
        
        sim_files = cfg.file_management['input_files']['sim']

        for fileIndex in range(0, len(sim_files)):
            file_name = sim_files[fileIndex]
            model_dict = ou.get_model_and_metadata(file_name=file_name)
            model = model_dict['model']
            run_status = model_dict['run_status']
            start_time = model_dict['start_time']
            stop_time = model_dict['stop_time']
            with PandasChainedAssignent():
                load_matrix.loc[(
                    load_matrix['fe_filename'] == file_name),
                                        'run_status'] = run_status
                load_matrix.loc[(
                    load_matrix['fe_filename'] == file_name),
                                        'start_time'] = start_time
                load_matrix.loc[(
                    load_matrix['fe_filename'] == file_name),
                                        'stop_time'] = stop_time

            if model is not None:
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

                if cfg['orcaflex']['postprocess']['summary']['flag']:
                    summary_groups_for_file = opp_summary.get_summary_for_file(cfg, model_dict, file_name)
                    summary = opp_summary.add_file_result_to_all_results(summary, summary_groups_for_file)
                if cfg['orcaflex']['postprocess']['linked_statistics']['flag']:
                    linked_statistics_for_file = opp_ls.get_linked_statistics(cfg, model, file_name)
                    linked_statistics = opp_ls.add_file_result_to_all_results(linked_statistics, linked_statistics_for_file)

                if cfg['orcaflex']['postprocess']['time_series']['flag']:
                    if cfg['time_series_settings']['data']: 
                        opp_ts.get_time_series_data(cfg, model_dict, file_name)
                else:
                    pass

                histogram_all_files.append(histogram_for_file)
                RangeAllFiles.append(None)

        self.HistogramAllFiles = histogram_all_files
        self.RangeAllFiles = RangeAllFiles

        opp_summary.save_summary(summary, cfg)

        opp_ls.save_linked_statistics(linked_statistics, cfg)

    def get_cfg_with_linked_statistics_master_data(self, cfg):
        if 'linked_statistics_settings_master' in cfg:
            linked_statistics_settings_master = cfg['linked_statistics_settings_master'].copy()
            linked_statistics_settings = cfg['linked_statistics_settings']

            for group_index in range(0, len(linked_statistics_settings['groups'])):
                group = linked_statistics_settings['groups'][group_index].copy()

                if 'Columns' in linked_statistics_settings_master['groups'][0]:
                    for column_index in range(0, len(group['Columns'])):
                        column = group['Columns'][column_index].copy()
                        column = update_deep_dictionary(
                            linked_statistics_settings_master['groups'][group_index]['Columns'][0], column)
                        group['Columns'][column_index] = copy.deepcopy(column)

                group = update_deep_dictionary(linked_statistics_settings_master['groups'][group_index], group)
                linked_statistics_settings['groups'][group_index] = copy.deepcopy(group)

            cfg['linked_statistics_settings'] = copy.deepcopy(linked_statistics_settings)

    def get_cfg_with_time_series_master_data(self, cfg):
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

    def get_cfg_with_summary_master_data(self, cfg):
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

