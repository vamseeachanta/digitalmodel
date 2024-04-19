import copy
import os
import glob
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

    def file_management(self, cfg):
        if cfg.file_management['flag']:
            cfg = self.get_files(cfg)

        return cfg

    def get_files(self, cfg):
        file_management_directory = self.get_file_management_directory(cfg)
        if file_management_directory is not None:
            analysis_root_folder = cfg['Analysis']['analysis_root_folder']
            file_is_valid, file_management_directory = is_dir_valid_func(file_management_directory,
                                                     analysis_root_folder)


        if cfg.file_management['files']['files_in_current_directory'][
                'flag'] or cfg.file_management['files']['files_in_current_directory']['auto_read']:
            program_extensions = ['lis']
            input_files = {}

            for file_ext in program_extensions:
                filename_pattern = cfg['file_management']['files']['files_in_current_directory'].get('filename_pattern', None)
                if filename_pattern is None:
                    glob_search = os.path.join(file_management_directory, f'*.{file_ext}')
                else:
                    glob_search = os.path.join(file_management_directory, f'*{filename_pattern}*.{file_ext}')
                raw_input_files_for_ext = glob.glob(glob_search)
                input_files.update({file_ext: raw_input_files_for_ext})

            cfg.file_management.update({'input_files': input_files})

        else:
            program_extensions = cfg.file_management['input_files'].keys()
            for file_ext in program_extensions:
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
