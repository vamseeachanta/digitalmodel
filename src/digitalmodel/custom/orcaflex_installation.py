import os
from collections import OrderedDict

from digitalmodel.common.saveData import saveDataYaml
from digitalmodel.common.yml_utilities import ymlInput


class OrcInstallation:

    def __init__(self):
        pass

    def create_installation_depth_model(self, cfg=None):
        if cfg is None:
            raise ValueError("cfg is None")

        model_file = OrderedDict(ymlInput(cfg['reference_elevation_file']))
        file_directory = os.path.dirname(cfg['reference_elevation_file'])

        for delta_elevation in cfg['delta_elevations']:

            model_file['6DBuoys'][
                cfg['structure']]['InitialZ'] -= delta_elevation
            model_file['3DBuoys'][
                cfg['masterlink']]['InitialZ'] -= delta_elevation
            model_file['Lines'][cfg['crane_wire']]['EndBZ'] -= delta_elevation
            model_file['Lines'][
                cfg['intermediate_sling']]['EndBZ'] -= delta_elevation
            model_file['Lines'][
                cfg['crane_wire']]['Sections'][0]['Length'] += delta_elevation

            file_name = os.path.join(
                file_directory, cfg['output_basefile'] + "_" +
                str(-int(model_file['6DBuoys'][cfg['structure']]['InitialZ']))
            ) + "m"
            saveDataYaml(model_file,
                         file_name,
                         default_flow_style='OrderedDumper')
