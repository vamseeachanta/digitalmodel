import os
import logging
import pandas as pd
import numpy as np
import math
import copy
import scipy
from collections import OrderedDict

from assetutilities.common.visualizations import Visualization
from assetutilities.common.utilities import is_file_valid_func
from assetutilities.common.data import SaveData
from assetutilities.common.data import PandasChainedAssignent
from assetutilities.common.data import TransformData
from assetutilities.common.yml_utilities import ymlInput

from digitalmodel.common.time_series_components import TimeSeriesComponents
from digitalmodel.common.ETL_components import ETL_components
from digitalmodel.custom.orcaflex_utilities import OrcaflexUtilities

try:
    import OrcFxAPI
except:
    print("OrcFxAPI not available")

ou = OrcaflexUtilities()


class OrcaFlexAnalysis():

    def __init__(self, cfg=None):
        self.cfg = cfg
        self.cfg_array = []
        self.get_model_state_information()
        load_matrix_columns = ['fe_filename', 'RunStatus']
        self.load_matrix = pd.DataFrame(columns=load_matrix_columns)
        self.data_quality = {}
        self.RAO_df_array = []

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

    def perform_simulations(self):
        self.process_fea()

        if 'postprocess' in self.cfg['orcaflex'] and self.cfg['orcaflex'][
                'postprocess']['flag']:
            self.post_process_files()
        self.save_data()

        try:
            file_type = self.cfg['default']['Analysis']['Analyze']['file_type']
        except:
            file_type = None

        if file_type is not None and file_type in ['script', 'batch_script']:
            print(
                "Processing orcaflex script/batch files for orcaflex input files"
            )
            self.process_scripts()

    def process_fea(self):
        for fileIndex in range(
                0, len(self.cfg['Analysis']['input_files']['with_ext'])):
            filename_with_ext = self.cfg['Analysis']['input_files']['with_ext'][
                fileIndex]
            filename_without_ext = self.cfg['Analysis']['input_files'][
                'no_ext'][fileIndex]

            static_flag = self.cfg['orcaflex']['analysis']['static']
            simulation_flag = self.cfg['orcaflex']['analysis']['simulation']
            iterate_flag = self.cfg['orcaflex']['iterate']['flag']
            iterate_to_target_value_flag = self.cfg['orcaflex']['iterate'][
                'to_target_value']

            save_sim_flag = self.cfg['orcaflex']['analysis']['save_sim']
            save_dat_flag = self.cfg['orcaflex']['analysis']['save_dat']

            model = OrcFxAPI.Model()
            try:
                logging.info("Loading input file ...")
                model = self.clean_model(model, filename_with_ext,
                                         filename_without_ext)
                model.LoadData(filename_with_ext)
                logging.info("Loading input file ... COMPLETE")
            except:
                simulation_flag = False
                iterate_flag = False
                raise ImportError(f"Load data for {filename_with_ext} ... FAIL")

            if static_flag or iterate_flag:
                model, run_success_flag = self.run_static_analysis(
                    filename_with_ext, model)
                if run_success_flag:
                    logging.info("Run statics  ....  SUCCESS")
                else:
                    logging.info("Run statics  ....  FAIL")
                    simulation_flag = save_sim_flag = save_dat_flag = iterate_to_target_value_flag = False

            if simulation_flag:
                model.RunSimulation()
                logging.info("Run simulation successful")

            if save_sim_flag:
                try:
                    model.SaveSimulation(filename_without_ext + '.sim')
                    logging.info("Save simulation successful")
                except:
                    print("Save simulation.. FAILED")

            if save_dat_flag:
                try:
                    model.SaveData(filename_without_ext + '.dat')
                    logging.info("Save data file      ... SUCCESS")
                except:
                    logging.info("Save data file      ... FAILED")

            if iterate_to_target_value_flag:
                iterate_cfg = self.cfg['default']['Analysis']['Analyze'].copy()
                iterate_cfg.update(
                    {'filename_without_ext': filename_without_ext})
                self.iterate_to_target_value(model, iterate_cfg)

        print(
            f"Analysis done for {len(self.cfg['Analysis']['input_files']['with_ext'])} input files"
        )

    def clean_model(self, model, filename_with_ext, filename_without_ext):
        clean_model = model

        UseCalculatedPositions_cfg = self.cfg['orcaflex']['iterate'][
            'UseCalculatedPositions'].copy()
        if UseCalculatedPositions_cfg['flag'] and UseCalculatedPositions_cfg[
                'clean_StaleCalculatedPositions']:
            save_data = SaveData()
            file_yml = ymlInput(filename_with_ext)
            clean_file = {'BaseFile': file_yml['BaseFile']}
            save_data.saveDataYaml(clean_file, filename_without_ext)
            clean_model = OrcFxAPI.Model()
            clean_model.LoadData(filename_with_ext)

        return clean_model

    def run_static_analysis(self, filename_with_ext, model):
        print(f"Static analysis for {filename_with_ext} ... .... ")
        try:
            print(f"First analysis ......")
            model.CalculateStatics()
            print(f"First analysis ... PASS")
            self.save_model_with_calculated_positions(filename_with_ext, model)
            model = self.analysis_with_calculated_positions(model)
            run_success_flag = True
        except:
            run_success_flag = False
            print(f"First static analysis for ... FAIL")

        return model, run_success_flag

    def analysis_with_calculated_positions(self, model):
        try:
            model.CalculateStatics()
        except:
            print(f"Analysis with calculated positions ... FAIL")

        return model

    def save_model_with_calculated_positions(self, filename_with_ext, model):
        calculated_cfg = self.cfg['orcaflex']['iterate'][
            'UseCalculatedPositions'].copy()

        if calculated_cfg['SetLinesToUserSpecifiedStartingShape']:
            model.UseCalculatedPositions(
                SetLinesToUserSpecifiedStartingShape=True)
        elif calculated_cfg['UseStaticLineEndOrientations']:
            model.UseCalculatedPositions(UseStaticLineEndOrientations=True)
        else:
            model.UseCalculatedPositions()
        calculated_positions_filename = filename_with_ext
        if not self.cfg['orcaflex']['iterate']['overwrite_data']:
            calculated_positions_filename = os.path.splitext(filename_with_ext)[
                0] + '_calculated_all' + os.path.splitext(filename_with_ext)[1]
        model.SaveData(calculated_positions_filename)

    def iterate_to_target_value(self, model, iterate_cfg):
        iterations_df = pd.DataFrame(columns=['variable', 'output'])
        model.CalculateStatics()
        model.SaveSimulation(iterate_cfg['filename_without_ext'] + '.sim')

        target_value = iterate_cfg['iterate']['column']['target_value']
        current_iteration = 1
        column_cfg = iterate_cfg['iterate']['column'].copy()

        update_cfg = {
            'model_file':
                iterate_cfg['filename_without_ext'] + '.' +
                iterate_cfg['file_type']
        }
        model_file = OrderedDict(ymlInput(update_cfg['model_file']))
        variable_current_value = model_file['Lines']['Umbilical']['Length[4]']
        output_current_value = round(
            self.process_summary_by_model_and_cfg(model, column_cfg), 3)
        iterations_df.loc[len(iterations_df)] = [
            variable_current_value, output_current_value
        ]

        while abs(output_current_value -
                  target_value) > 0.2 and current_iteration < iterate_cfg[
                      'iterate']['column']['max_iterations']:

            current_iteration += 1

            variable_current_value = self.get_variable_current_value_for_iteration(
                iterations_df, variable_current_value, target_value)

            update_cfg.update({'variable_value': variable_current_value})

            model['Umbilical'].Length[3] = variable_current_value
            # ou.update_model(update_cfg)

            model.SaveData(update_cfg['model_file'])
            model.CalculateStatics()
            model.SaveSimulation(iterate_cfg['filename_without_ext'] + '.sim')
            output_current_value = round(
                self.process_summary_by_model_and_cfg(model, column_cfg), 3)

            iterations_df.loc[len(iterations_df)] = [
                variable_current_value, output_current_value
            ]
            iterations_df.sort_values(by=['output'], inplace=True)

        print(f"Iterations done in {current_iteration} times")
        print(f"Current target value is: {output_current_value}")
        print(f"Variable current value is: {variable_current_value}")

    def get_variable_current_value_for_iteration(self, iterations_df,
                                                 variable_current_value,
                                                 target_value):

        if len(iterations_df) > 1:
            xp = list(iterations_df.output)
            fp = list(iterations_df.variable)
            f = scipy.interpolate.interp1d(xp, fp, fill_value="extrapolate")
            variable_current_value = np.round(f(target_value), decimals=3)

        else:
            variable_current_value += 0.25

        return variable_current_value

    def post_process_files(self, cfg):
        self.post_process(cfg)
        print("Successful post-process {0} sim files".format(
            len(self.RangeAllFiles)))
        if self.cfg.default['Analysis']['time_series']['flag']:
            self.prepare_histogram_bins_for_output()
        if self.cfg.default['Analysis']['cummulative_histograms']['flag']:
            self.generate_cummulative_histograms()
            self.save_histograms()

        self.save_summary(cfg)
        self.process_range_graphs(cfg)
        self.save_cfg_files_from_multiple_files(cfg)

    def save_data(self):
        save_data = SaveData()
        save_data.saveDataYaml(
            self.cfg, self.cfg['Analysis']['result_folder'] +
            self.cfg['Analysis']['file_name'], False)

        if 'postprocess' in self.cfg.default and self.cfg.default[
                'postprocess']['cummulative_histograms']['flag']:
            histogram_data_array = [
                item['data'] for item in self.detailed_histograms_array
            ]
            label_array = [
                item['label'] for item in self.detailed_histograms_array
            ]
            customdata = {
                "FileName":
                    self.cfg['Analysis']['result_folder'] +
                    self.cfg['Analysis']['file_name'] + '_histograms.xlsx',
                "SheetNames":
                    label_array,
                "thin_border":
                    True
            }
            if len(histogram_data_array) > 0:
                save_data.DataFrameArray_To_xlsx_openpyxl(
                    histogram_data_array, customdata)

        if 'Analysis' in self.cfg.default and self.cfg.default[
                'Analysis'].__contains__(
                    'RAOs') and self.cfg.default['Analysis']['RAOs']['flag']:
            self.save_RAOs()

    def post_process_RAOs(self, model, FileObjectName):
        cfg_raos = self.cfg['RAOs'].copy()
        for rao_set in cfg_raos['sets']:

            cfg_time_series_reference = self.get_cfg_time_series_reference(
                cfg_raos, rao_set)
            reference_time_series = self.get_time_series_from_orcaflex_run(
                model, cfg_time_series_reference)

            output_time_series_array = self.get_time_series_output(
                cfg_raos, rao_set, model)

            self.get_RAOs_from_time_series_data(output_time_series_array,
                                                reference_time_series, cfg_raos,
                                                rao_set)

    def get_cfg_fft(self, cfg_fft_common, cfg_fft_custom):
        cfg_fft = cfg_fft_common.copy()
        cfg_fft.update(cfg_fft_custom)

        return cfg_fft

    def get_RAOs_from_time_series_data(self, output_time_series_array,
                                       reference_time_series, cfg_raos,
                                       rao_set):

        RAO_df_Labels = ['frequency'
                        ] + rao_set['time_series']['Output']['Variable_Label']
        RAO_df_columns = self.get_detailed_RAO_df_columns(RAO_df_Labels)
        RAO_ArcLength_df = pd.DataFrame(columns=RAO_df_columns)

        for ArcLength_index in range(0, len(output_time_series_array)):
            ArcLength_Label = rao_set['time_series']['Output']['Label'][
                ArcLength_index]
            RAO_df_array_Labels = [item['Label'] for item in self.RAO_df_array]
            if ArcLength_Label not in RAO_df_array_Labels:
                self.RAO_df_array.append({
                    'Label': ArcLength_Label,
                    'RAO_df': RAO_ArcLength_df.copy()
                })

        cfg_fft_common = cfg_raos['fft'].copy()
        cfg_fft_custom = rao_set['fft'].copy()
        cfg_fft = self.get_cfg_fft(cfg_fft_common, cfg_fft_custom)

        for ArcLength_index in range(0, len(output_time_series_array)):
            ArcLength_Label = rao_set['time_series']['Output']['Label'][
                ArcLength_index]
            RAO_df_array_Labels = [item['Label'] for item in self.RAO_df_array]
            RAO_df_array_index = RAO_df_array_Labels.index(ArcLength_Label)
            df = self.RAO_df_array[RAO_df_array_index]['RAO_df'].copy()
            Variable_RAO_df = pd.DataFrame(columns=RAO_df_columns)
            for Variable_index in range(
                    0, len(output_time_series_array[ArcLength_index])):
                time_series = output_time_series_array[ArcLength_index][
                    Variable_index]
                RAO_raw, RAO_filtered = self.get_RAO(time_series,
                                                     reference_time_series,
                                                     cfg_fft)
                if Variable_index == 0:
                    Variable_RAO_df['frequency'] = RAO_filtered['frequency']
                    Variable_RAO_df = self.assign_file_info_to_Variable_df(
                        Variable_RAO_df, self.fileIndex)
                RAO_df_column_label = rao_set['time_series']['Output'][
                    'Variable_Label'][Variable_index]
                Variable_RAO_df[RAO_df_column_label] = RAO_filtered['complex']

            df = pd.concat([df, Variable_RAO_df])
            self.RAO_df_array[RAO_df_array_index]['RAO_df'] = df.copy()

    def assign_file_info_to_Variable_df(self, Variable_RAO_df, fileIndex):
        df_temp = self.load_matrix.iloc[fileIndex].copy()
        keys = list(df_temp.keys())
        for key in keys:
            Variable_RAO_df[key] = df_temp[key]

        return Variable_RAO_df

    def get_RAO(self, signal, excitation, cfg_fft):
        ts_comp = TimeSeriesComponents(
            {'default': {
                'analysis': {
                    'fft': cfg_fft
                }
            }})
        RAO_raw, RAO_filtered = ts_comp.get_RAO(signal=signal,
                                                excitation=excitation,
                                                cfg_rao=cfg_fft)

        return RAO_raw, RAO_filtered

    def get_time_series_output(self, cfg_raos, rao_set, model):
        cfg_common = cfg_raos['time_series'].copy()
        cfg_group = rao_set['time_series'].copy()
        cfg_time_series_array = self.get_cfg_time_series_custom(
            cfg_group, cfg_common)
        output_time_series_array = []
        for ArcLength_index in range(0, len(cfg_time_series_array)):
            output_ArcLength_time_series_array = []
            for Variable_index in range(
                    0, len(cfg_time_series_array[ArcLength_index])):
                cfg_Variable = cfg_time_series_array[ArcLength_index][
                    Variable_index]
                Variable_time_series = self.get_time_series_from_orcaflex_run(
                    model, cfg_Variable)
                output_ArcLength_time_series_array.append(Variable_time_series)
            output_time_series_array.append(output_ArcLength_time_series_array)

        return output_time_series_array

    def get_cfg_time_series_reference(self, cfg_raos, rao_set):
        cfg_common = cfg_raos['time_series'].copy()
        cfg_group = rao_set['time_series'].copy()
        reference_cfg = cfg_common.copy()
        reference_cfg.update(cfg_group['Reference'])
        return reference_cfg

    def get_cfg_time_series_custom(self, cfg_group, cfg_common):
        cfg_time_series_array = []
        cfg_time_series = cfg_common.copy()
        cfg_time_series.update(cfg_group['Output'])
        for ArcLength_index in range(0, len(cfg_group['Output']['ArcLength'])):
            cfg_time_series_variable_array = []
            ArcLength = cfg_group['Output']['ArcLength'][ArcLength_index]
            cfg_time_series.update({'ArcLength': ArcLength})
            ObjectName = cfg_group['Output'].get('ObjectName', None)
            if ObjectName is not None:
                cfg_time_series.update({'ObjectName': ObjectName})

            for Variable_index in range(0,
                                        len(cfg_group['Output']['Variable'])):
                Variable = cfg_group['Output']['Variable'][Variable_index]
                cfg_time_series.update({'Variable': Variable})
                cfg_time_series_variable_array.append(cfg_time_series.copy())

            cfg_time_series_array.append(cfg_time_series_variable_array.copy())

        return cfg_time_series_array

    def process_range_graphs(self):
        if self.cfg['default']['Analysis']['RangeGraph']['flag']:
            vizualization = Visualization()
            # vizualization.orcaflex_range_plot(RangeAllFiles, self.cfg)
        else:
            print("No postprocessing plots per user request")

    def process_summary_by_model_and_cfg(self, model, cfg):
        RangeDF = pd.DataFrame()
        try:
            objectName = cfg['ObjectName']
            OrcFXAPIObject = model[objectName]
        except:
            OrcFXAPIObject = model[FileObjectName]

        SimulationPeriod = cfg['SimulationPeriod']
        TimePeriod = self.get_TimePeriodObject(SimulationPeriod)

        ArcLengthArray = cfg['ArcLength']
        arclengthRange = self.get_ArcLengthObject(ArcLengthArray)

        VariableName = cfg['Variable']
        AdditionalDataArray = cfg['AdditionalData']

        if cfg['Command'] == 'Range Graph':
            output = self.get_RangeGraph(OrcFXAPIObject, TimePeriod,
                                         VariableName, arclengthRange)

            AdditionalDataName = 'X'
            RangeDF[AdditionalDataName] = output.X

            output_value = self.get_additional_data(cfg, RangeDF, VariableName,
                                                    output, AdditionalDataArray)
        elif cfg['Command'] == 'TimeHistory':
            try:
                output = OrcFXAPIObject.TimeHistory(VariableName, TimePeriod,
                                                    arclengthRange)
            except:
                arclengthRange = None
                output = OrcFXAPIObject.TimeHistory(VariableName, TimePeriod)

            output_value = self.get_additional_data(cfg, RangeDF, VariableName,
                                                    output, AdditionalDataArray)
        elif cfg['Command'] == 'StaticResult':
            output_value = self.get_StaticResult(OrcFXAPIObject, VariableName,
                                                 ArcLengthArray)
        elif cfg['Command'] == 'GetData':
            output_value = self.get_input_data(OrcFXAPIObject, VariableName,
                                               ArcLengthArray)

        return output_value

    def get_input_data(self, OrcFXAPIObject, VariableName, ArcLengthArray):
        output_value = OrcFXAPIObject.GetData(VariableName, -1)

        return output_value

    def get_additional_data(self, cfg, RangeDF, VariableName, output,
                            AdditionalDataArray):
        for AdditionalDataIndex in range(0, len(AdditionalDataArray)):
            AdditionalDataName = AdditionalDataArray[AdditionalDataIndex]
            if cfg['Command'] == 'Range Graph':
                RangeDF[VariableName] = getattr(output, AdditionalDataName)
                if VariableName == "API STD 2RD Method 1":
                    RangeDF[VariableName] = [
                        math.sqrt(x) for x in RangeDF[VariableName]
                    ]
                if AdditionalDataName == "Max":
                    output_value = RangeDF[VariableName].max()
                elif AdditionalDataName == "Min":
                    output_value = RangeDF[VariableName].min()
                elif AdditionalDataName == "Mean":
                    output_value = RangeDF[VariableName].mean()
            else:
                if AdditionalDataName == "Max":
                    output_value = output.max()
                elif AdditionalDataName == "Min":
                    output_value = output.min()
                elif AdditionalDataName == "Mean":
                    output_value = output.mean()

            if cfg.__contains__('transform'):
                trans_cfg = cfg['transform']
                trans_cfg['data'] = output_value
                transformed_cfg = self.transform_output(trans_cfg)
                output_value = transformed_cfg['data']

            return output_value

    def get_StaticResult(self, OrcFXAPIObject, VariableName, ArcLengthArray):
        # Result at End A
        output = OrcFXAPIObject.StaticResult(VariableName, OrcFxAPI.oeEndA)
        # Result at Arc Length X
        objectExtra = OrcFxAPI.oeLine(ArcLength=ArcLengthArray[0])
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
                    result_array = ['Mean', 'Mean'
                                   ] + summaryDF_temp.mean(axis=0).tolist()
                    SummaryDF.loc[len(
                        SummaryDF)] = loadng_condition_array + result_array
                if 'AddMinimumToSummary' in cfg['summary_settings'].keys():
                    result_array = ['Minimum', 'Minimum'
                                   ] + summaryDF_temp.min(axis=0).tolist()
                    SummaryDF.loc[len(
                        SummaryDF)] = loadng_condition_array + result_array
                if 'AddMaximumToSummary' in cfg['summary_settings'].keys():
                    result_array = ['Maximum', 'Maximum'
                                   ] + summaryDF_temp.max(axis=0).tolist()
                    SummaryDF.loc[len(
                        SummaryDF)] = loadng_condition_array + result_array

            try:
                decimalArray = pd.Series([0, 0, 0, 2, 0],
                                         index=SummaryDF.columns.values)
                SummaryDF = SummaryDF.round(decimalArray)
            except Exception as e:
                SummaryDF = SummaryDF.round(2)

        self.saveSummaryToExcel(SummaryFileNameArray, cfg)

        cfg[cfg['basename']].update({'summary_groups': len(summary_groups)})
        print(f"Processed summary files: {len(summary_groups)}")

    def saveSummaryToExcel(self, SummaryFileNameArray, cfg):
        if len(self.SummaryDFAllFiles) > 0:
            save_data = SaveData()
            customdata = {
                "FileName":
                    cfg['Analysis']['result_folder'] +
                    cfg['Analysis']['file_name'] + '.xlsx',
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

    def post_process(self, cfg):
        self.load_matrix = self.get_load_matrix_with_filenames(cfg)
        # Intialize output arrays
        RangeAllFiles = []
        histogram_all_files = []
        SummaryDFAllFiles = [pd.DataFrame()] * len(
            cfg['summary_settings']['groups'])

        sim_files = cfg.file_management['input_files']['sim']

        summary_cfg = cfg['summary_settings'].copy()

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
                try:
                    histogram_for_file = self.post_process_histograms(
                        model, FileObjectName)
                except:
                    pass
                histogram_all_files.append(histogram_for_file)

            for SummaryIndex in range(0,
                                      len(cfg['summary_settings']['groups'])):
                try:
                    SimulationFileName = self.get_SimulationFileName(file_name)
                    SummaryDFAllFiles[SummaryIndex] = self.postProcessSummary(
                        model, SummaryDFAllFiles[SummaryIndex], SummaryIndex,
                        FileDescription, FileObjectName, SimulationFileName,
                        fileIndex, cfg)
                except Exception as e:
                    logging.info(str(e))

            if cfg['orcaflex']['postprocess']['RAOs']['flag']:
                self.post_process_RAOs(model, FileObjectName)

            else:
                pass

            histogram_all_files.append(histogram_for_file)
            RangeAllFiles.append(None)

        self.HistogramAllFiles = histogram_all_files
        self.RangeAllFiles = RangeAllFiles
        self.SummaryDFAllFiles = SummaryDFAllFiles

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

    def postProcessRange(self, model, cfg, FileObjectName):

        # Range Graphs for a simulation
        RangeFile = []

        for RangeGraphIndex in range(0, len(cfg['RangeGraph'])):
            RangeDF = pd.DataFrame()
            # Read Object
            try:
                objectName = cfg['RangeGraph'][RangeGraphIndex]['ObjectName']
                OrcFXAPIObject = model[objectName]
            except:
                OrcFXAPIObject = model[FileObjectName]

            SimulationPeriod = cfg['RangeGraph'][RangeGraphIndex][
                'SimulationPeriod']
            TimePeriod = self.get_TimePeriodObject(SimulationPeriod)
            VariableName = cfg['RangeGraph'][RangeGraphIndex]['Variable']

            try:
                if len(cfg['RangeGraph'][RangeGraphIndex]['ArcLength']) > 1:
                    StartArcLength = cfg['RangeGraph'][RangeGraphIndex][
                        'ArcLength'][0]
                    EndArcLength = cfg['RangeGraph'][RangeGraphIndex][
                        'ArcLength'][1]
                    output = OrcFXAPIObject.RangeGraph(
                        VariableName,
                        TimePeriod,
                        arclengthRange=OrcFxAPI.arSpecifiedArclengths(
                            StartArcLength, EndArcLength))
                else:
                    StartArcLength = cfg['RangeGraph'][RangeGraphIndex][
                        'ArcLength'][0]
                    output = OrcFXAPIObject.RangeGraph(
                        VariableName,
                        TimePeriod,
                        arclengthRange=OrcFxAPI.arSpecifiedArclengths(
                            StartArcLength))
                    # arclengthRange = OrcFxAPI.arSpecifiedArclengths(0, 100)
            except:
                arclengthRange = None
                output = OrcFXAPIObject.RangeGraph(
                    VariableName, TimePeriod, arclengthRange=arclengthRange)
            # Assign Arc Length
            AdditionalDataName = 'X'
            RangeDF[AdditionalDataName] = output.X

            for AdditionalDataIndex in range(
                    0,
                    len(cfg['RangeGraph'][RangeGraphIndex]['AdditionalData'])):
                AdditionalDataName = cfg['RangeGraph'][RangeGraphIndex][
                    'AdditionalData'][AdditionalDataIndex]
                RangeDF[VariableName] = getattr(output, AdditionalDataName)

                if VariableName == "API STD 2RD Method 1":
                    RangeDF[VariableName] = [
                        math.sqrt(x) for x in RangeDF[VariableName]
                    ]
            RangeFile.append(RangeDF)

        if self.cfg['default']['Analysis']['RangeGraph']['flag'] and self.cfg[
                'default']['Analysis']['RangeGraph'][
                    'add_effective_tension_to_cfg']:
            self.add_result_to_cfg(RangeDF, VariableName)

        return RangeFile

    def post_process_histograms(self, model, FileObjectName):
        histogram_array = []
        self.histogram_labels = []

        for time_series_index in range(0, len(self.cfg['time_series'])):
            cfg_temp = self.cfg['time_series'][time_series_index]
            self.histogram_labels.append(cfg_temp['Label'])
            histogram_object = self.get_histogram_object_from_orcaflex_run(
                FileObjectName, cfg_temp, model)
            histogram_array.append(histogram_object)

        return histogram_array

    def get_histogram_object_from_orcaflex_run(self, FileObjectName,
                                               cfg_histogram, model):

        # Read Object
        OrcFXAPIObject = model[FileObjectName]
        SimulationPeriod = cfg_histogram['SimulationPeriod']
        TimePeriod = self.get_TimePeriodObject(SimulationPeriod)
        VariableName = cfg_histogram['Variable']
        ArcLength = cfg_histogram['ArcLength'][0]
        RadialPos_text = cfg_histogram['RadialPos']
        if RadialPos_text == 'Outer':
            RadialPos = 1
        elif RadialPos_text == 'Inner':
            RadialPos = 0
        Theta = cfg_histogram['Theta']
        objectExtra = OrcFxAPI.oeLine(ArcLength=ArcLength,
                                      RadialPos=RadialPos,
                                      Theta=Theta)
        rain_flow_half_cycles = OrcFXAPIObject.RainflowHalfCycles(
            VariableName, TimePeriod, objectExtra=objectExtra)
        bins = self.cfg['default']['Analysis']['rain_flow']['bins']
        rainflow_range = self.cfg['default']['Analysis']['rain_flow']['range']
        histogram_object = np.histogram(rain_flow_half_cycles,
                                        bins=bins,
                                        range=(rainflow_range[0],
                                               rainflow_range[1]))

        return histogram_object[0]

    def get_time_series_from_orcaflex_run(self, model, cfg_time_series):

        time_series = None
        ObjectName = cfg_time_series['ObjectName']
        OrcFXAPIObject = model[ObjectName]
        SimulationPeriod = cfg_time_series['SimulationPeriod']
        TimePeriod = self.get_TimePeriodObject(SimulationPeriod)
        VariableName = cfg_time_series['Variable']
        if cfg_time_series.__contains__('ArcLength'):
            ArcLength = cfg_time_series['ArcLength'][0]
            RadialPos_text = cfg_time_series['RadialPos']
            if RadialPos_text == 'Outer':
                RadialPos = 1
            elif RadialPos_text == 'Inner':
                RadialPos = 0
            Theta = cfg_time_series['Theta']
            objectExtra = OrcFxAPI.oeLine(ArcLength=ArcLength,
                                          RadialPos=RadialPos,
                                          Theta=Theta)
        else:
            Location = cfg_time_series['Location']
            objectExtra = OrcFxAPI.oeEnvironment(Location[0], Location[1],
                                                 Location[2])

        time_series = OrcFXAPIObject.TimeHistory(VariableName,
                                                 TimePeriod,
                                                 objectExtra=objectExtra)
        return time_series

    def add_result_to_cfg(self, RangeDF, VariableName):
        self.cfg['Analysis']['X'] = RangeDF['X'].tolist()
        self.cfg['Analysis'][VariableName] = RangeDF[VariableName].tolist()
        self.cfg_array.append(copy.deepcopy(self.cfg))

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

                output_value = self.process_summary_by_model_and_cfg(
                    model, summary_group_item_cfg)
                summary_from_sim_file.append(output_value)

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

    def get_RangeGraph(self, OrcFXAPIObject, TimePeriod, VariableName,
                       arclengthRange):
        try:
            output = OrcFXAPIObject.RangeGraph(VariableName,
                                               TimePeriod,
                                               arclengthRange=arclengthRange)
        except:
            arclengthRange = None
            output = OrcFXAPIObject.RangeGraph(VariableName,
                                               TimePeriod,
                                               arclengthRange=arclengthRange)
        return output

    def get_TimePeriodObject(self, SimulationPeriod):
        if type(SimulationPeriod) is int:
            TimePeriodObject = SimulationPeriod
        elif SimulationPeriod == 'WhileSimulation':
            TimePeriodObject = OrcFxAPI.PeriodNum.WholeSimulation
        elif SimulationPeriod == 'LatestWave':
            TimePeriodObject = OrcFxAPI.PeriodNum.LatestWave
        elif len(SimulationPeriod) == 2:
            TimePeriodObject = OrcFxAPI.SpecifiedPeriod(SimulationPeriod[0],
                                                        SimulationPeriod[1])
        elif len(SimulationPeriod) == 1:
            TimePeriodObject = OrcFxAPI.SpecifiedPeriod(SimulationPeriod[0])
        else:
            TimePeriodObject = OrcFxAPI.SpecifiedPeriod()

        return TimePeriodObject

    def get_ArcLengthObject(self, ArcLength):

        if ArcLength is None:
            arclengthRange = None
        elif type(ArcLength) is str:
            if 'EndA' in ArcLength:
                arclengthRange = OrcFxAPI.oeEndA
            elif 'EndB' in ArcLength:
                arclengthRange = OrcFxAPI.oeEndB
            elif 'Touchdown' in ArcLength:
                arclengthRange = OrcFxAPI.oeTouchdown
            else:
                raise ValueError
        elif len(ArcLength) == 2:
            StartArcLength = ArcLength[0]
            EndArcLength = ArcLength[1]
            arclengthRange = OrcFxAPI.arSpecifiedArclengths(
                StartArcLength, EndArcLength)
        elif len(ArcLength) == 1:
            StartArcLength = ArcLength[0]
            arclengthRange = OrcFxAPI.arSpecifiedArclengths(
                StartArcLength, StartArcLength)
        else:
            arclengthRange = None

        return arclengthRange

    def prepare_histogram_bins_for_output(self):
        rain_flow_settings = self.cfg['default']['Analysis']['rain_flow']
        start_range = rain_flow_settings['range'][0]
        end_range = rain_flow_settings['range'][1]
        no_of_bins = rain_flow_settings['bins']
        bins = list(np.linspace(start_range, end_range, no_of_bins + 1))
        self.detailed_histograms_array = []
        no_of_files = len(self.simulation_filenames)
        for time_series_index in range(0, len(self.cfg.time_series)):
            time_series_cfg = self.cfg.time_series[time_series_index]
            SummaryDF = None
            AddSummaryColumnsArray = []
            if (len(self.cfg.Summary) > 0) and time_series_cfg.__contains__(
                    'AddSummaryToHistograms'
            ) and time_series_cfg['AddSummaryToHistograms']:
                SummaryDF = self.SummaryDFAllFiles[time_series_index]
                Summary_cfg = self.cfg.Summary[time_series_index]
                AddSummaryColumnsArray = [
                    item['Label'] for item in Summary_cfg['Columns']
                ]
            columns = self.get_detailed_histogram_df_columns(
                rain_flow_settings, AddSummaryColumnsArray)
            detailed_histograms = pd.DataFrame(columns=columns)
            for file_index in range(0, no_of_files):
                histogram = self.HistogramAllFiles[file_index][
                    time_series_index]
                SimulationDuration = self.get_SimulationDuration(file_index)
                seastate_probability = self.get_seastate_probability(file_index)
                if (len(histogram) > 0) and (seastate_probability > 0):
                    loading_condition_array = self.get_loading_condition_array(
                        file_index)
                    AddSummary_array = self.get_AddSummary_array(
                        SummaryDF, file_index, AddSummaryColumnsArray)
                    for bin_index in range(0, no_of_bins):
                        bin_range_array = [bins[bin_index], bins[bin_index + 1]]
                        histogram_cycles = histogram[bin_index]
                        histogram_cycles_per_year = histogram_cycles * (
                            365.25 * 24 * 3600) / SimulationDuration
                        histogram_cycles_per_year_with_probability = histogram_cycles_per_year * seastate_probability
                        histogram_cycle_array = [
                            histogram_cycles, histogram_cycles_per_year,
                            histogram_cycles_per_year_with_probability
                        ]
                        histogram_bin_array = bin_range_array + histogram_cycle_array
                        df_row = loading_condition_array + histogram_bin_array + AddSummary_array
                        detailed_histograms.loc[len(
                            detailed_histograms)] = df_row
            Label = time_series_cfg['Label']

            self.detailed_histograms_array.append({
                'label': Label,
                'data': detailed_histograms
            })

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

    def get_detailed_histogram_df_columns(self, rain_flow_settings,
                                          AddSummaryColumnsArray):
        if self.load_matrix is None:
            load_matrix_columns = []
        else:
            load_matrix_columns = self.load_matrix.columns.to_list()
        if rain_flow_settings.__contains__(
                'BinLabels') and rain_flow_settings['BinLabels'] is not None:
            BinLabels = rain_flow_settings['BinLabels']
        else:
            BinLabels = ['BinStart', 'BinEnd']
        cycle_labels = [
            'Halfcycles_per_simulation', 'Halfcycles_per_yr',
            'Halfcycles_per_yr_with_probability'
        ]
        rainflow_labels = BinLabels + cycle_labels
        columns = load_matrix_columns + rainflow_labels + AddSummaryColumnsArray
        return columns

    def get_detailed_RAO_df_columns(self, RAO_Columns):
        if self.load_matrix is None:
            load_matrix_columns = []
        else:
            load_matrix_columns = self.load_matrix.columns.to_list()
        columns = load_matrix_columns + RAO_Columns
        return columns

    def generate_cummulative_histograms(self):

        self.all_histogram_file_dfs = []
        self.probability_array = []
        self.simulation_duration_array = []
        self.file_label_array = []
        bins = list(
            range(self.cfg['default']['rain_flow']['range'][0],
                  self.cfg['default']['rain_flow']['range'][1],
                  self.cfg['default']['rain_flow']['bins']))
        cummulative_histogram_df = pd.DataFrame(bins, columns=['bins'])
        for time_series_index in range(0, len(self.HistogramAllFiles[0])):
            label = self.histogram_labels[time_series_index]
            cummulative_histogram_df[label] = pd.Series([0] * len(bins))

        for file_index in range(
                0, len(self.cfg['Analysis']['input_files']['no_ext'])):
            file_label = self.simulation_Labels[file_index]['Label']
            self.file_label_array.append(file_label)
            self.probability_array.append(
                self.cfg['Files'][file_index]['probability'])
            self.simulation_duration_array.append(
                self.cfg['Files'][file_index]['simulation_duration'])
            histogram_file_df = pd.DataFrame(bins, columns=['bins'])
            for time_series_index in range(0, len(self.HistogramAllFiles[0])):
                label = self.histogram_labels[time_series_index]
                histogram_file_df[label] = pd.Series(
                    self.HistogramAllFiles[file_index][time_series_index] *
                    (365.25 * 24 * 3600) /
                    self.simulation_duration_array[file_index],
                    index=histogram_file_df.index)
                cummulative_histogram_df[label] = cummulative_histogram_df[
                    label] + histogram_file_df[label] * self.probability_array[
                        file_index] / 100
            self.all_histogram_file_dfs.append(histogram_file_df.copy())

        self.file_label_array.append('cummulative_histograms')
        self.all_histogram_file_dfs.append(cummulative_histogram_df.copy())

        self.qa_histograms()
        self.file_label_array.append('histograms_qa')
        self.all_histogram_file_dfs.append(self.sum_df)

    def qa_histograms(self):
        etl_components = ETL_components(cfg=None)
        self.sum_df = etl_components.get_sum_df_from_df_array(
            self.all_histogram_file_dfs)

    def save_histograms(self):
        save_data = SaveData()
        customdata = {
            "FileName":
                self.cfg['Analysis']['result_folder'] +
                self.cfg['Analysis']['file_name'] + '_histograms.xlsx',
            "SheetNames":
                self.file_label_array,
            "thin_border":
                True
        }
        save_data.DataFrameArray_To_xlsx_openpyxl(self.all_histogram_file_dfs,
                                                  customdata)

    def LinkedStatistics(self):

        # TODO LinkedStatistics using OrcaFlex. Code currently not working.
        VariableName = 'Effective Tension', 'Bend Moment'
        arclengthRange = OrcFxAPI.oeArcLength(25.0)
        SimulationPeriod = cfg["PostProcess"]['Summary'][SummaryIndex][
            'SimulationPeriod']
        TimePeriod = self.get_TimePeriodObject(SimulationPeriod)
        stats = OrcFXAPIObject.LinkedStatistics(VariableName, TimePeriod,
                                                arclengthRange)
        query = stats.Query('Effective Tension', 'Bend Moment')
        print(query.ValueAtMax)
        print(query.ValueAtMin)

    # Alternatively, use Range graphs for required quantities and obtain them using DFs. (easiest to customize)

    def process_scripts(self):
        model = OrcFxAPI.Model()
        for file_index in range(
                0, len(self.cfg['Analysis']['input_files']['with_ext'])):
            file_name = self.cfg['Analysis']['input_files']['with_ext'][
                file_index]
            model.ProcessBatchScript(file_name)

        print("Ran script files successfully")

    def save_cfg_files_from_multiple_files(self):

        save_data = SaveData()

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

    def get_files_for_analysis(self, analysis_type, fileIndex,
                               input_files_with_extension,
                               input_files_without_extension):
        filename = self.simulation_filenames[fileIndex]
        analysis_root_folder = self.cfg['Analysis']['analysis_root_folder']
        file_is_valid, filename = is_file_valid_func(filename,
                                                     analysis_root_folder)
        filename_components = filename.split('.')
        filename_without_extension = filename.replace(
            '.' + filename_components[-1], "")
        if len(filename_components) > 1:
            input_files_with_extension.append(filename)
            input_files_without_extension.append(filename_without_extension)
        elif os.path.isfile(filename_without_extension + '.yml'):
            input_files_without_extension.append(filename_without_extension)
            input_files_with_extension.append(
                input_files_without_extension[fileIndex] + '.yml')
        elif os.path.isfile(filename_without_extension + '.dat'):
            input_files_without_extension.append(filename_without_extension)
            input_files_with_extension.append(
                input_files_without_extension[fileIndex] + '.dat')
        else:
            print('File not found: {0}'.format(filename))

        if self.cfg['orcaflex']['analysis']['simulation']:
            analysis_type.append('simulation')
        if self.cfg['orcaflex']['analysis']['static']:
            analysis_type.append('statics')
        if self.cfg['orcaflex']['iterate']['flag']:
            analysis_type.append('iterate')

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

        save_data = SaveData()

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
