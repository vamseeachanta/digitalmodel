import copy
import os
import glob
import OrcFxAPI
from collections import OrderedDict

from assetutilities.common.saveData import saveDataYaml
from assetutilities.common.yml_utilities import ymlInput


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

    def get_sim_file_finish_flag(self, simfname):
        tmodel = OrcFxAPI.Model(simfname)
        settime = tmodel.general.StageEndTime[len(tmodel.general.StageEndTime) -
                                              1]
        simtime = tmodel.simulationTimeStatus.CurrentTime
        simfinish = True
        if settime > simtime:
            simfinish = False
        return simfinish

    def edit_simfiles_to_finish_runs(self, simfs):
        dt = 0.05    ### timestep
        constdt = True
        simfs = glob.glob('*.sim')
        failfn = 0
        failfs = []

        for simf in simfs:
            sim_file_finish_flag = self.simfinish(simf)
            if not sim_file_finish_flag:
                failfn = failfn + 1
                print(failfn)
                failfs.append(simf)
                fname = simf[:len(simf) - 4] + '.dat'
                tmodel = OrcFxAPI.Model(fname)
                tmodel.DynamicSolutionMethod = 'Implicit time domain'
                if constdt:
                    tmodel.general.ImplicitUseVariableTimeStep = 'No'
                    tmodel.general.ImplicitConstantTimeStep = dt
                else:
                    tmodel.general.ImplicitUseVariableTimeStep = 'Yes'
                    tmodel.general.ImplicitVaribaleMaxTimeStep = dt
                #tmodel.DynamicSolutionMethod = 'Explicit time domain'
                tmodel.SaveData(fname)

    def get_files(self, cfg):
        input_files_without_extension = []
        input_files_with_extension = []
        analysis_type = []
        try:
            self.get_simulation_filenames()
        except:
            print("No simulation files found or resolved")

        for fileIndex in range(0, len(self.simulation_filenames)):
            self.get_files_for_analysis(analysis_type, fileIndex,
                                        input_files_with_extension,
                                        input_files_without_extension)

        post_process_flag = self.cfg['orcaflex']['post_process']['flag']

        if post_process_flag:
            for fileIndex in range(0, len(self.simulation_filenames)):
                self.get_files_for_postprocess(cfg, analysis_type, fileIndex,
                                               input_files_with_extension,
                                               input_files_without_extension)

        self.assign_and_summarize_files(analysis_type,
                                        input_files_with_extension,
                                        input_files_without_extension)

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
                    file_group['Label']
                    for file_group in cfg['Files']['data']
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