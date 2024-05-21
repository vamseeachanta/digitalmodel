import copy
import os
import glob
import pathlib
import logging
import math
from assetutilities.common.utilities import is_dir_valid_func

from assetutilities.common.yml_utilities import ymlInput
from digitalmodel.common.orcaflex_model_utilities import OrcaflexModelUtilities

from assetutilities.common.data import SaveData
save_data = SaveData()

try:
    import OrcFxAPI
except:
    print("OrcFxAPI not available")
from collections import OrderedDict

from assetutilities.common.yml_utilities import ymlInput
from assetutilities.common.file_management import FileManagement

fm = FileManagement()


class AqwaUtilities:

    def __init__(self):
        pass

    def is_license_available(self):
        # Template code from OrcaFlex
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

