# Standard library imports
import glob
import logging
import math
import os

# Third party imports
import colorama
from assetutilities.common.data import SaveData
from assetutilities.common.yml_utilities import ymlInput
from colorama import Fore, Style

# Reader imports
from digitalmodel.modules.orcaflex.orcaflex_model_utilities import (
    OrcaflexModelUtilities,
)

save_data = SaveData()
colorama.init(convert=True)

try:
    # Third party imports
    import OrcFxAPI
except:
    logging.debug("OrcFxAPI not available")
# Standard library imports
from collections import OrderedDict

# Third party imports
from assetutilities.common.file_management import FileManagement

fm = FileManagement()
omu = OrcaflexModelUtilities()


class OrcaflexUtilities:

    def __init__(self):
        pass

    def is_orcaflex_available(self):
        try:
            model = OrcFxAPI.Model()
            logging.info("Orcaflex license is available .... PASS")
            print(f"{Fore.GREEN}Orcaflex license is available .... PASS{Style.RESET_ALL}")
            return True
        except:
            print(f"{Fore.RED}Orcaflex license is NOT available .... FAIL{Style.RESET_ALL}")
            print("")
            raise Exception("Orcaflex license is NOT available .... FAIL")
            return False

    def save_sim_file(self, model, model_file_name):
        sim_filename = os.path.splitext(model_file_name)[0] + '.sim'
        model.SaveSimulation(sim_filename)

    def update_model(self, cfg=None):
        if cfg is None or cfg['model_file'] is None:
            raise ValueError("model_file is not provided ... FAIL")

        model_file = OrderedDict(ymlInput(cfg['model_file']))
        model_file_updated = copy.deepcopy(model_file)
        model_file_updated['Lines']['Umbilical']['Length[7]'] = cfg[
            'variable_value']

        save_data.saveDataYaml(model_file_updated,
                     os.path.splitext(cfg['model_file'])[0],
                     default_flow_style='OrderedDumper', sort_keys=False)

    def get_sim_file_finish_status(self, model):
        settime = model.general.StageEndTime[len(model.general.StageEndTime) -
                                             1]
        simtime = model.simulationTimeStatus.CurrentTime
        simfinish = True
        if settime > simtime:
            simfinish = False

        sim_status = {
            'finish_flag': simfinish,
            'run_time': {
                'set': settime,
                'last': simtime
            }
        }
        return sim_status

    def update_input_file(self, update_cfg):
        update_cfg['cfg']
        sim_file = update_cfg['filename']

        input_file = input_file_save_as = sim_file[:len(sim_file) -
                                                   4] + update_cfg['cfg'][
                                                       'save_as']
        if not os.path.isfile(input_file):
            input_file = sim_file[:len(sim_file) - 4] + '.dat'
            if not os.path.isfile(input_file):
                raise FileExistsError

        model = OrcFxAPI.Model(input_file)
        # TODO Check for implicit method or otherwise
        # old_value = model.DynamicSolutionMethod
        # new_value = 'Implicit time domain'
        # if old_value != new_value:
        #     model.DynamicSolutionMethod = new_value
        #     logging.info(
        #         f"      DynamicSolutionMethod... old: {old_value} to new: {new_value}"
        #     )

        general_properties = update_cfg['cfg']['general'].copy()
        if general_properties['ImplicitUseVariableTimeStep'] == 'No':
            old_value = model.general.ImplicitUseVariableTimeStep
            new_value = general_properties['ImplicitUseVariableTimeStep']
            if old_value != new_value:
                model.general.ImplicitUseVariableTimeStep = new_value
                logging.info(
                    f"      ImplicitUseVariableTimeStep... changed from : '{old_value}' to '{new_value}'"
                )

            old_value = model.general.ImplicitConstantTimeStep
            new_value = general_properties['TimeStep']
            if old_value != new_value:
                model.general.ImplicitConstantTimeStep = new_value
                logging.info(
                    f"      ImplicitConstantTimeStep... changed from : '{old_value}' to '{new_value}'"
                )

        else:
            old_value = model.general.ImplicitUseVariableTimeStep
            new_value = general_properties['ImplicitUseVariableTimeStep']
            if old_value != new_value:
                model.general.ImplicitUseVariableTimeStep = new_value
                logging.info(
                    f"      ImplicitUseVariableTimeStep... changed from : '{old_value}' to '{new_value}'"
                )

            old_value = model.general.ImplicitVariableMaxTimeStep
            new_value = general_properties['TimeStep']
            if old_value != new_value:
                model.general.ImplicitVariableMaxTimeStep = new_value
                logging.info(
                    f"      ImplicitVaribaleMaxTimeStep... changed from : '{old_value}' to '{new_value}'"
                )

        #tmodel.DynamicSolutionMethod = 'Explicit time domain'
        model.SaveData(input_file_save_as)

    def file_management(self, cfg):
        if cfg.file_management['flag']:
            cfg = self.get_files(cfg)

        return cfg

    def get_files(self, cfg):
        # Third party imports
        from assetutilities.common.utilities import is_dir_valid_func
        
        file_management_directory = self.get_file_management_directory(cfg)
        if file_management_directory is not None:
            analysis_root_folder = cfg['Analysis']['analysis_root_folder']
            file_is_valid, file_management_directory = is_dir_valid_func(file_management_directory,
                                                     analysis_root_folder)


        if cfg.file_management['files']['files_in_current_directory'][
                'flag'] or cfg.file_management['files']['files_in_current_directory']['auto_read']:
            orcaflex_extensions = ['yml', 'yaml', 'dat', 'sim', 'txt']
            input_files = {}

            for file_ext in orcaflex_extensions:
                filename_pattern = cfg['file_management']['files']['files_in_current_directory'].get('filename_pattern', None)
                if filename_pattern is None:
                    glob_search = os.path.join(file_management_directory, f'*.{file_ext}')
                else:
                    glob_search = os.path.join(file_management_directory, f'*{filename_pattern}*.{file_ext}')
                raw_input_files_for_ext = glob.glob(glob_search)
                input_files.update({file_ext: raw_input_files_for_ext})

            cfg.file_management.update({'input_files': input_files})

        else:
            orcaflex_extensions = cfg.file_management['input_files'].keys()
            for file_ext in orcaflex_extensions:
                raw_input_files_for_ext = cfg.file_management['input_files'][
                    file_ext]

                valid_file_count = 0
                for input_file_index in range(0, len(raw_input_files_for_ext)):
                    input_file = raw_input_files_for_ext[input_file_index]
                    if not os.path.isfile(input_file):
                        raw_input_files_for_ext[
                            input_file_index] = os.path.join(
                                cfg.Analysis['analysis_root_folder'],
                                input_file)
                    if os.path.isfile(
                            raw_input_files_for_ext[input_file_index]):
                        valid_file_count = valid_file_count + 1

                logging.info(
                    f"Number of '{file_ext}' input files : {len(raw_input_files_for_ext)} . Valid files are: {valid_file_count}."
                )

        return cfg

    def get_file_management_directory(self, cfg):

        if cfg.file_management['files']['files_in_current_directory']['flag']:
            file_management_directory = cfg.Analysis['analysis_root_folder']
        else:
            file_management_directory = cfg.file_management['files'][
                'files_in_current_directory']['directory']

        return file_management_directory

    def sim_file_analysis_and_update(self, cfg):
        sim_files = cfg.file_management['input_files']['*.sim']
        sim_status_cfg = []
        update_unfinished_cfg = cfg['file_management'][
            'update_unfinished'].copy()
        for sim_file in sim_files:
            logging.info(
                f"Simulation file analysis for : {sim_file} .... START")
            model = OrcFxAPI.Model(sim_file)
            sim_status = {'file': sim_file}
            sim_status.update(self.get_sim_file_finish_status(model))

            if not sim_status['finish_flag'] and update_unfinished_cfg['flag']:
                update_unfinished_cfg.update({'filename': sim_file})
                try:
                    self.update_input_file(update_unfinished_cfg)
                except Exception as e:
                    logging.info(e)
                    logging.info(
                        f"Version incompatibility for file: {sim_file}.")

            sim_status_cfg.append(sim_status)
            logging.info(f"Simulation file analysis for : {sim_file} .... End")

        cfg[cfg['basename']].update({'sim_status': sim_status_cfg})

        return cfg

    def assign_and_summarize_files(self, cfg, analysis_type,
                                   input_files_with_extension,
                                   input_files_without_extension):
        number_of_files_not_found = len(
            self.simulation_filenames) - len(input_files_with_extension)
        if number_of_files_not_found == 0:
            print('Successfully found all input files')
        else:
            print(f'Number of input files missing: {number_of_files_not_found}')
        cfg['Analysis']['input_files'] = {}
        cfg['Analysis']['input_files']['no_ext'] = input_files_without_extension
        cfg['Analysis']['input_files']['with_ext'] = input_files_with_extension
        cfg['Analysis']['input_files']['analysis_type'] = analysis_type

        return cfg

    def get_simulation_filenames(self, cfg):
        if cfg['Files']['data_source'] == 'yml':
            self.simulation_filenames = [
                file_group['Name'] for file_group in cfg['Files']['data']
            ]
            if 'ObjectName' in cfg['Files']['data'][0]:
                self.simulation_ObjectNames = [
                    file_group['ObjectName']
                    for file_group in cfg['Files']['data']
                ]
            if 'SimulationDuration' in cfg['Files']['data'][0]:
                self.simulation_SimulationDuration = [
                    file_group['SimulationDuration']
                    for file_group in cfg['Files']['data']
                ]
            if 'ProbabilityRatio' in cfg['Files']['data'][0]:
                self.simulation_ProbabilityRatio = [
                    file_group['ProbabilityRatio']
                    for file_group in cfg['Files']['data']
                ]
            if 'Label' in cfg['Files']['data'][0]:
                self.simulation_Labels = [
                    file_group['Label'] for file_group in cfg['Files']['data']
                ]
        elif cfg['Files']['data_source'] == 'csv':
            # Third party imports
            import pandas as pd
            self.load_matrix = pd.read_csv(cfg['Files']['csv_filename'])
            self.load_matrix['RunStatus'] = None
            self.simulation_filenames = self.load_matrix['fe_filename']
            self.simulation_ObjectNames = self.load_matrix['ObjectName']
            self.simulation_SimulationDuration = self.load_matrix[
                'SimulationDuration']
            self.simulation_ProbabilityRatio = self.load_matrix[
                'ProbabilityRatio']

            self.simulation_Labels = self.load_matrix.index.to_list()
            self.simulation_Labels = [
                ('LC_' + str(item)) for item in self.simulation_Labels
            ]
        else:
            self.simulation_filenames = []

        return cfg

    def prepare_operating_window_definition(self, cfg):

        # for include_file in cfg['includedfile']:
        #     yaml_file.update({'includefile': include_file})
        for input_set in cfg['inputs']:
            self.prepare_operating_window_for_input_set(input_set, cfg)
            
        return cfg

    def prepare_operating_window_for_input_set(self, input_set, cfg):
        wave_yaml_file = omu.get_wave_template()
        wave = input_set['loads']['wave']
        hs = wave['hs']
        tp = wave['tp']
        WaveDirection = wave['WaveDirection']

        current = input_set['loads']['current']
        ActiveCurrent = current['ActiveCurrent']
        RefCurrentDirection = current['RefCurrentDirection']
        SurfaceCurrentFactor = current['SurfaceCurrentFactor']


        if wave['WaveType'] == 'Airy':
            for hs_item in hs:
                for tp_item in tp:
                    if input_set['loads']['wave']['peakedness']['by_region']:
                        cfg_peakedness = {'region': wave['peakedness']['region'], 'hs': hs_item, 'tp': tp_item}
                        peakedness = self.get_wave_peak_enhancement_factor_by_region(cfg_peakedness)
                    hmax = self.get_theoretical_hmax({'hs': hs_item, 'tp': tp_item, 'peakedness': peakedness})
                    tassociated = self.get_theoretical_tassociated({'tp': tp_item, 'peakedness': peakedness})

                    for WaveDirection_item in WaveDirection:
                        wave_yaml_variation = {'WaveType': wave['WaveType'], 'WaveDirection': WaveDirection_item, 'WaveHeight': hmax, 'WavePeriod': tassociated}
                        wave_yaml_file['WaveTrains'][0].update(wave_yaml_variation)

                        for ActiveCurrent_index in range(0, len(ActiveCurrent)):
                            ActiveCurrent_item = ActiveCurrent[ActiveCurrent_index]
                            current_type = current['current_type'][ActiveCurrent_index]
                            for RefCurrentDirection_item in RefCurrentDirection:
                                for SurfaceCurrentFactor_item in SurfaceCurrentFactor:
                                    
                                    current_yaml_file = {'ActiveCurrent': ActiveCurrent_item, 'Currents': {ActiveCurrent_item: {'RefCurrentDirection': RefCurrentDirection_item, 'CurrentFactor[1]': SurfaceCurrentFactor_item}}}

                                    wave_file_name = f"Hs{'{:.2f}'.format(hs_item)}-WD{'{:03d}'.format(WaveDirection_item)}-Tp{'{:04.1f}'.format(tp_item)}"
                                    current_file_name = f"-AC{current_type}-CD{'{:03d}'.format(RefCurrentDirection_item)}-CF{'{:02.1f}'.format(SurfaceCurrentFactor_item)}"
                                    yml_file_name = wave_file_name + current_file_name
                                    self.get_full_yaml_file_and_save(input_set, wave_yaml_file, current_yaml_file, yml_file_name, cfg)



    def get_full_yaml_file_and_save(self, input_set, wave_yaml_file, current_yaml_file, yml_file_name, cfg):
        if 'includefile' in input_set:
            full_yml_file = {'includefile': input_set['includefile']}
        elif 'BaseFile' in input_set:
            full_yml_file = {'BaseFile': input_set['BaseFile']}

        general_yml = self.get_general_yml(input_set, wave_yaml_file)
        full_yml_file.update(general_yml)
        full_yml_file.update({'Environment': wave_yaml_file})
        full_yml_file['Environment'].update(current_yaml_file)

        output_dir = input_set['output_dir']
        full_output_dir_path = os.path.join(cfg['Analysis']['analysis_root_folder'], output_dir)
        if not os.path.isdir(full_output_dir_path):
            os.makedirs(full_output_dir_path)

        save_data.saveDataYaml(full_yml_file, os.path.join(full_output_dir_path, yml_file_name), default_flow_style=False, sort_keys=False)

    def get_general_yml(self, input_set, wave_yaml_file):
        tperiod_factor = input_set['general']['initial_tperiod_factor'] + input_set['general']['analysis_tperiod_factor']
        WavePeriod = wave_yaml_file['WaveTrains'][0]['WavePeriod']
        StageDuration2 = round(tperiod_factor*WavePeriod, 0)
        general_yml = {'General': {'StageDuration[2]': StageDuration2}}
        return general_yml

    def get_random_wave_seeds(self):
        seeds = [123456, 234567, 345678, 456789, 567890, 678901, 789012, 890123, 901234, 12345, 19918, 51352, 64477, 42864, 89141, 82983, 34067, 65909, 54827, 48305]
        return seeds

    def get_wave_peak_enhancement_factor_by_region(self, cfg_peakedness):
        cfg_tempate = {'region': 'Gayana', 'tp': 10}

        peakedness = 0
        if cfg_peakedness['region'] == "Gayana":
            if cfg_peakedness['tp'] < 10:
                peakedness = 2
            else:
                peakedness = 8

        return peakedness

    def get_theoretical_wave_peak_enhancement_factor(self, cfg_peakedness):
        cfg_tempate = {'hs': 2.5, 'tp': 10}
        tp_over_sqrt_hs = cfg_peakedness['tp']/cfg_peakedness['hs']**0.5
        if tp_over_sqrt_hs > 5:
            peakedness = 1
        elif tp_over_sqrt_hs <3.6:
            peakedness = 5
        else:
            peakedness = math.exp(5.75 - 1.15*tp_over_sqrt_hs)

        peakedness = round(peakedness, 2)

        return round(peakedness, 2)

    def get_theoretical_tz(self, cfg_tz):
        cfg_tempate = {'tp': 10, 'peakedness': 5}
        tz = cfg_tz['tp']*(0.6673 + 0.05037*cfg_tz['peakedness'] - 0.00623*cfg_tz['peakedness']**2 + 0.0003341*cfg_tz['peakedness']**3)

        return round(tz, 2)

    def get_theoretical_hmax(self, cfg_hmax):
        cfg_tempate = {'hs': 2.5, 'peakedness': 5}
        hmax = cfg_hmax['hs']*cfg_hmax['peakedness']
        tz= self.get_theoretical_tz({'tp': cfg_hmax['tp'], 'peakedness': cfg_hmax['peakedness']})
        N = 10800/ tz
        factor = (math.log(N)/2)**0.5
        if factor > 1.86:
            factor = 1.86
        hmax = cfg_hmax['hs']*factor

        return round(hmax, 2)

    def get_theoretical_tassociated(self, cfg_tassociated):
        cfg_tempate = {'tp': 10, 'peakedness': 5}
        tz = self.get_theoretical_tz({'tp': cfg_tassociated['tp'], 'peakedness': cfg_tassociated['peakedness']})
        tassociated = 1.05*tz

        return round(tassociated, 2)