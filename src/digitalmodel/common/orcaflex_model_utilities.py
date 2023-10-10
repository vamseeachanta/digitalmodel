import copy
import os
import OrcFxAPI
from collections import OrderedDict

from digitalmodel.common.saveData import saveDataYaml
from digitalmodel.common.yml_utilities import ymlInput
from digitalmodel.common.yml_utilities import WorkingWithYAML

wwy = WorkingWithYAML()


class OrcaflexModelUtilities:

    def __init__(self):
        pass

    def get_6d_buoy_template(self):
        library_yaml_cfg = {
            'filename': 'tests/test_data/6d_buoy/6d_buoy_template.yml'
        }
        buoy_6d_template = wwy.get_library_yaml_file(library_yaml_cfg)

        return buoy_6d_template

    def get_BaseFile_first(self, model):
        '''
        OrderDict to get BaseFile at top of model is not working efficiently.
        Further work needed.
        '''
        updated_model = OrderedDict()

        model_keys = list(model.keys())
        if 'BaseFile' in model_keys:
            updated_model['BaseFile'] = model['BaseFile']

        model_keys.remove('BaseFile')
        for key in model_keys:
            updated_model[key] = model[key].copy()

        return updated_model
