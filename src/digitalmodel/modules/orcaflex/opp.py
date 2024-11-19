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
from digitalmodel.modules.orcaflex.opp_time_series import OPPTimeSeries
from digitalmodel.modules.orcaflex.opp_visualization import OPPVisualization
from digitalmodel.modules.orcaflex.orcaflex_objects import OrcaFlexObjects
from digitalmodel.modules.orcaflex.orcaflex_utilities import OrcaflexUtilities
save_data = SaveData()

ou = OrcaflexUtilities()
of_objects = OrcaFlexObjects()
opp_ts = OPPTimeSeries()
opp_rg = OPPRangeGraph()
opp_visualization = OPPVisualization()

class OrcaFlexPostProcess():

    def __init__(self, cfg=None):
        self.cfg = cfg
        self.cfg_array = []
        self.get_model_state_information()
        load_matrix_columns = ['fe_filename', 'RunStatus']
        self.load_matrix = pd.DataFrame(columns=load_matrix_columns)
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
            self.save_summary(cfg)
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

    def file_management(self, cfg):
        cfg = ou.file_management(cfg)
        return cfg


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

        self.load_matrix = self.get_load_matrix_with_filenames(cfg)
        # Intialize output arrays
        RangeAllFiles = []
        histogram_all_files = []

        sim_files = cfg.file_management['input_files']['sim']
        cfg[cfg['basename']]['time_series'] = []

        for fileIndex in range(0, len(sim_files)):
            file_name = sim_files[fileIndex]
            model = self.get_model_from_filename(file_name=file_name)
            FileDescription = 'Description'
            FileObjectName = 'Dummy_Object'
            histogram_for_file = [[]] * len(cfg.time_series_settings['groups'])

            self.fileIndex = fileIndex
            print("Post-processing file: {}".format(file_name))
            try:
                RangeAllFiles.append(
                    self.postProcessRange(model, self.cfg, FileObjectName))
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

        if 'summary_settings' in cfg: 
            SummaryDFAllFiles = self.process_summary_groups(cfg)
            self.SummaryDFAllFiles = SummaryDFAllFiles

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


    def get_input_data(self, OrcFXAPIObject, VariableName, model):
        if 'Current' in VariableName[0]:
            ActiveCurrent = OrcFXAPIObject.GetData('ActiveCurrent', -1)
            model.environment.SelectedCurrent = ActiveCurrent
            output_value = model.environment.GetData(VariableName[0], VariableName[1])
        else:
            output_value = OrcFXAPIObject.GetData(VariableName[0], VariableName[1])

        return output_value

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

    def save_summary(self, cfg):
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
            SummaryDF = self.SummaryDFAllFiles[SummaryIndex]
            summaryDF_temp = SummaryDF.iloc[:, (
                len(SummaryDF.columns) -
                summary_column_count):len(SummaryDF.columns)]

            loadng_condition_array = []
            if self.load_matrix is not None:
                loadng_condition_array = [None] * len(self.load_matrix.columns)

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

    def save_summary_to_csv(self, SummaryFileNameArray, cfg):
        cfg[cfg['basename']].update({'summary': {}})

        for group_idx in range(0, len(SummaryFileNameArray)):
            df = self.SummaryDFAllFiles[group_idx]
            file_name = os.path.join(cfg['Analysis']['result_folder'],
                    cfg['Analysis']['file_name'] + '_' + SummaryFileNameArray[group_idx] + '.csv')

            df.to_csv(file_name, index=False)

            result_dict = {SummaryFileNameArray[group_idx]: df.to_dict()}
            cfg[cfg['basename']]['summary'].update(result_dict)



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

    def get_load_matrix_with_filenames(self, cfg):
        self.load_matrix['fe_filename'] = cfg.file_management['input_files'][
            'sim']
        self.load_matrix['RunStatus'] = None
        return self.load_matrix

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

            self.file_management(cfg)
            self.load_matrix = self.get_load_matrix_with_filenames(cfg)

            sim_files = cfg.file_management['input_files']['sim']

            for fileIndex in range(0, len(sim_files)):
                file_name = sim_files[fileIndex]
                self.fileIndex = fileIndex
                model = self.get_model_from_filename(file_name=file_name)
                FileDescription = 'Description'
                FileObjectName = 'Dummy_Object'
                print("Post-processing file: {}".format(file_name))

                SimulationFileName = self.get_SimulationFileName(file_name)

                try:
                    SummaryDFAllFiles[SummaryIndex] = self.postProcessSummary(
                            model, SummaryDFAllFiles[SummaryIndex], SummaryIndex,
                            FileDescription, FileObjectName, SimulationFileName,
                            fileIndex, cfg)
                except Exception as e:
                    logging.info(str(e))
                    raise Exception("Error in post processing")

        return SummaryDFAllFiles

    def get_model_from_filename(self, file_name):
        SimulationFileName = self.get_SimulationFileName(file_name)
        model = None
        if os.path.isfile(SimulationFileName):
            try:
                model = self.loadSimulation(SimulationFileName)
                RunStatus = str(model.state)

                if self.load_matrix is not None:
                    with PandasChainedAssignent():
                        self.load_matrix.loc[(
                            self.load_matrix['fe_filename'] == file_name),
                                             'RunStatus'] = RunStatus
                    if RunStatus not in [
                            'Reset', 'InStaticState', 'SimulationStopped', '4'
                    ]:
                        model = None
            except Exception as e:
                logging.info(
                    f"Model: {SimulationFileName} ... Error Loading File")
                logging.info(str(e))

        return model

    def get_SimulationFileName(self, file_name):
        get_filename_without_extension = self.get_filename_without_extension(
            file_name)
        SimulationFileName = get_filename_without_extension + '.sim'
        return SimulationFileName

    def loadSimulation(self, FileName):
        model = OrcFxAPI.Model(FileName)
        return model


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

    def get_seastate_probability(self, file_index):
        seastate_probability = self.simulation_ProbabilityRatio[file_index]
        return seastate_probability

    def get_SimulationDuration(self, file_index):
        SimulationDuration = self.simulation_SimulationDuration[file_index]
        return SimulationDuration

    def get_AddSummary_array(self, SummaryDF, file_index,
                             AddSummaryColumnsArray):
        filename = self.simulation_filenames[file_index]
        simulation_filename = self.get_filename_without_extension(
            filename) + '.sim'

        AddSummary_array = [None] * len(AddSummaryColumnsArray)
        if (SummaryDF is not None) and (not SummaryDF.empty):
            FileSummary_DF = SummaryDF[SummaryDF['FileName'] ==
                                       simulation_filename].copy()
            if not FileSummary_DF.empty:
                AddSummary_array = list(
                    FileSummary_DF[AddSummaryColumnsArray].values[0])

        return AddSummary_array

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


    def LinkedStatistics(self):

        # TODO LinkedStatistics using OrcaFlex. Code currently not working.
        VariableName = 'Effective Tension', 'Bend Moment'
        arclengthRange = OrcFxAPI.oeArcLength(25.0)
        SimulationPeriod = cfg["PostProcess"]['Summary'][SummaryIndex][
            'SimulationPeriod']
        TimePeriod = of_objects.get_TimePeriodObject(SimulationPeriod)
        stats = OrcFXAPIObject.LinkedStatistics(VariableName, TimePeriod,
                                                arclengthRange)
        query = stats.Query('Effective Tension', 'Bend Moment')
        print(query.ValueAtMax)
        print(query.ValueAtMin)

    # Alternatively, use Range graphs for required quantities and obtain them using DFs. (easiest to customize)


    def save_cfg_files_from_multiple_files(self):


        for file_index in range(0, len(self.cfg_array)):
            save_data.saveDataYaml(
                self.cfg_array[file_index],
                self.cfg['Analysis']['cfg_array_file_names']['with_path']
                ['without_ext'][file_index], False)

    def get_files_for_postprocess(self, analysis_type, fileIndex,
                                  input_files_with_extension,
                                  input_files_without_extension):

        filename = self.simulation_filenames[fileIndex]
        filename_components = filename.split('.')
        filename_without_extension = filename.replace(
            '.' + filename_components[-1], "")
        if len(filename_components) > 1:
            input_files_with_extension.append(filename)
            input_files_without_extension.append(filename_without_extension)
            if self.cfg['orcaflex']['analysis']['simulation']:
                analysis_type.append('simulation')
            if self.cfg['orcaflex']['analysis']['static']:
                analysis_type.append('statics')
        elif os.path.isfile(filename_without_extension + '.sim'):
            input_files_without_extension.append(filename_without_extension)
            input_files_with_extension.append(
                input_files_without_extension[fileIndex] + '.yml')
            if self.cfg['orcaflex']['analysis']['simulation']:
                analysis_type.append('simulation')
            if self.cfg['orcaflex']['analysis']['static']:
                analysis_type.append('statics')
        else:
            print('File not found: {0}'.format(filename))

    def get_filename_without_extension(self, filename):
        filename_components = filename.split('.')
        filename_without_extension = filename.replace(
            '.' + filename_components[-1], "")

        return filename_without_extension

    def transform_output(self, cfg):
        trans_data = TransformData()
        trans_data.get_transformed_data(cfg)

        return cfg

    def save_RAOs(self):

        df_array = [item['RAO_df'] for item in self.RAO_df_array]
        df_label_array = [item['Label'] for item in self.RAO_df_array]

        customdata = {
            "FileName":
                self.cfg['Analysis']['result_folder'] +
                self.cfg['Analysis']['file_name'] + '_RAOs.xlsx',
            "SheetNames":
                df_label_array,
            "thin_border":
                True
        }
        save_data.DataFrameArray_To_xlsx_openpyxl(df_array, customdata)

