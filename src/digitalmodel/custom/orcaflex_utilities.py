import copy
import os
import glob
import OrcFxAPI
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

    def update_input_file(self, model):
        dt = 0.05    ### timestep
        constdt = True

        fname = simf[:len(simf) - 4] + '.dat'
        model = OrcFxAPI.Model(fname)
        model.DynamicSolutionMethod = 'Implicit time domain'
        if constdt:
            model.general.ImplicitUseVariableTimeStep = 'No'
            model.general.ImplicitConstantTimeStep = dt
        else:
            model.general.ImplicitUseVariableTimeStep = 'Yes'
            model.general.ImplicitVaribaleMaxTimeStep = dt
        #tmodel.DynamicSolutionMethod = 'Explicit time domain'
        model.SaveData(fname)

    def file_management(self, cfg):
        if cfg.file_management['flag']:
            cfg = self.get_files(cfg)
        if cfg.file_management['update_unfinished']['flag']:
            cfg = self.sim_file_analysis_and_update(cfg)

        return cfg

    def get_files(self, cfg):
        orcaflex_extensions = ['*.yml', '*.yaml', '*.dat', '*.sim', '*.txt']
        input_files = {}

        if cfg.file_management['files']['files_in_folder']:
            for file_ext in orcaflex_extensions:
                input_files_for_ext = glob.glob(
                    cfg.Analysis['analysis_root_folder'] + '/' + file_ext)
                input_files.update({file_ext: input_files_for_ext})

        cfg.update({cfg['basename']: {'input_files': input_files}})

        return cfg

    def sim_file_analysis_and_update(self, cfg):
        sim_files = cfg.orcaflex_post_process['input_files']['*.sim']
        for sim_file in sim_files:
            model = OrcFxAPI.Model(sim_file)
            sim_status = self.get_sim_file_finish_status(model)
            if not sim_status['finish_flag']:
                self.update_input_file(model)

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