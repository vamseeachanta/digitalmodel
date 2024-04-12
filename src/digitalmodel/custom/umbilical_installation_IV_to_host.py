import os
import math
import pandas as pd
from assetutilities.common.update_deep import update_deep_dictionary

from assetutilities.common.yml_utilities import ymlInput
from assetutilities.common.data import SaveData

from assetutilities.common.utilities import is_file_valid_func

from digitalmodel.common.orcaflex_linetypes import OrcaflexLineTypes
save_data = SaveData()
olt = OrcaflexLineTypes()

class InstallationVtoHost():

    def __init__(self):
        pass

    def installation_phase(self, cfg, phase):
        for step in phase['step']:
            self.installation_step(cfg, step)

        return cfg

    def installation_step(self, cfg, step):
        installation_step_dict = {}

        installation_step_dict = update_deep_dictionary(installation_step_dict, {"includefile": step['includefile']})
        for target_settings in step['target']:
            target_dict = self.get_target_result(cfg, target_settings)
            installation_step_dict = update_deep_dictionary(installation_step_dict, target_dict)

        # Write step file
        analysis_root_folder = cfg['Analysis']['analysis_root_folder']
        filename_path = os.path.join(analysis_root_folder, step['name'])
        save_data.saveDataYaml(installation_step_dict, filename_path, default_flow_style=False)

    def get_target_result(self, cfg, target_settings):
        if target_settings['type'] == 'line_length':
            return self.get_target_line_length(cfg, target_settings)
        elif target_settings['type'] == 'reference_distance':
            return self.get_target_reference_distance(cfg, target_settings)
        else:
            raise NotImplementedError("Target type not implemented.")
        
    def get_target_line_length(self, cfg, target_settings):
        reference_file = target_settings['reference_file']
        analysis_root_folder = cfg['Analysis']['analysis_root_folder']
        file_is_valid, reference_file = is_file_valid_func(reference_file,
                                                     analysis_root_folder)
        reference_file_yml = ymlInput(reference_file)
        Lines = target_settings['Lines']
        reference_line_length = 0
        for line in Lines:
            columns = ['LineType', 'Length', 'TargetSegmentLength']
            reference_file_dict = reference_file_yml['Lines'][line]['LineType, Length, TargetSegmentLength']
            df = pd.DataFrame(reference_file_dict, columns=columns)
            LineType = target_settings['LineType']
            df_filter = df[df['LineType'].isin(LineType)]
            reference_line_length = reference_line_length + df_filter['Length'].sum()

        keychain_target = target_settings['keychain_target']
        s = keychain_target[2]
        indx = int(s[s.find("[")+1:s.find("]")])
        current_line_length = reference_file_yml['Lines'][keychain_target[1]]['LineType, Length, TargetSegmentLength'][indx-1][1]
        length_correction = target_settings['value'] - reference_line_length

        dict  = {}
        if length_correction != 0:
            target_length = round(float(current_line_length + length_correction), 1)
            dict.update({'Lines': {keychain_target[1]: {keychain_target[2]: target_length }}})

        return dict
    
    def get_target_reference_distance(self, cfg, target_settings):
        lay_direction = cfg['installation']['lay_direction']
        host_reference_location = cfg['installation']['host']['reference_location']
        installation_vessel_reference_location = cfg['installation']['installation_vessel']['reference_location']
        reference_distance = target_settings['value']

        keychain_target_vessel = target_settings['keychain_target_vessel']

        initial_heading = target_settings['initial_heading']
        initial_x = host_reference_location[0] + reference_distance*math.cos(math.radians(lay_direction)) - installation_vessel_reference_location[0]*math.cos(math.radians(initial_heading))
        initial_y = host_reference_location[1] + reference_distance*math.sin(math.radians(lay_direction)) - installation_vessel_reference_location[0]*math.sin(math.radians(initial_heading))

        dict  = {}
        dict = update_deep_dictionary(dict, {keychain_target_vessel[0]: {keychain_target_vessel[1]: {'InitialX': round(initial_x,1), 'InitialY': round(initial_y,1), 'InitialHeading': round(initial_heading,1)}}})

        keychain_target_line = target_settings['keychain_target_line']
        endbx = initial_x  + installation_vessel_reference_location[0]*math.cos(math.radians(initial_heading))
        endby = initial_y  + installation_vessel_reference_location[0]*math.sin(math.radians(initial_heading))
        for keychain_target_line_item in keychain_target_line:
            dict = update_deep_dictionary(dict, {"Lines": {keychain_target_line_item[1]: {'EndBX': round(endbx,1), 'EndBY': round(endby,1)}}})

        return dict
