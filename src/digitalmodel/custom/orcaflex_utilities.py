import copy
import os
import glob
import logging
try:
    import OrcFxAPI
except:
    print("OrcFxAPI not available")
from collections import OrderedDict

from assetutilities.common.saveData import saveDataYaml
from assetutilities.common.yml_utilities import ymlInput
from assetutilities.common.file_management import FileManagement

fm = FileManagement()


class OrcaflexUtilities:

    def __init__(self):
        pass

    def is_orcaflex_available(self):
        try:
            model = OrcFxAPI.Model()
            print("Orcaflex license is available")
            return True
        except:
            print("Orcaflex license is NOT available")
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

        saveDataYaml(model_file_updated,
                     os.path.splitext(cfg['model_file'])[0],
                     default_flow_style='OrderedDumper')

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
        file_management_directory = self.get_file_management_directory(cfg)

        if cfg.file_management['files']['files_in_current_directory'][
                'auto_read']:
            orcaflex_extensions = ['yml', 'yaml', 'dat', 'sim', 'txt']
            input_files = {}

            for file_ext in orcaflex_extensions:
                raw_input_files_for_ext = glob.glob(file_management_directory +
                                                    '/*.' + file_ext)
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