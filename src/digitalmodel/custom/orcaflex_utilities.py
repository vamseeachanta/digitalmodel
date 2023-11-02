import copy
import os
import glob
import OrcFxAPI
from collections import OrderedDict

from digitalmodel.common.saveData import saveDataYaml
from digitalmodel.common.yml_utilities import ymlInput


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
