from collections import OrderedDict

from assetutilities.common.yml_utilities import WorkingWithYAML
from modulefinder import ModuleFinder

wwy = WorkingWithYAML()


class OrcaflexModelUtilities:

    def __init__(self):
        pass

    def get_6d_buoy_template(self):
        library_name = 'digitalmodel'
        library_yaml_cfg = {
            'filename': 'base_configs/modules/code_dnvrph103/6d_buoy_template.yml',
            'library_name': library_name
        }
        buoy_6d_template = wwy.get_library_yaml_file(library_yaml_cfg)

        return buoy_6d_template

    def get_umbilical_LineType_template(self):
        library_name = 'digitalmodel'
        library_yaml_cfg = {
            'filename': 'tests/test_data/umbilical_analysis/umbilical_LineType_template.yml',
            'library_name': library_name
        }
        buoy_6d_template = wwy.get_library_yaml_file(library_yaml_cfg)

        return buoy_6d_template

    def get_wave_template(self):
        library_name = 'digitalmodel'
        library_yaml_cfg = {
            'filename': 'tests/test_data/orcaflex_file_preparation/wave_template.yml',
            'library_name': library_name
        }
        template = wwy.get_library_yaml_file(library_yaml_cfg)

        return template


    def get_current_template(self):
        library_name = 'digitalmodel'
        library_yaml_cfg = {
            'filename': 'tests/test_data/orcaflex_file_preparation/current_template.yml',
            'library_name': library_name
        }
        template = wwy.get_library_yaml_file(library_yaml_cfg)

        return template



    def get_wave_template_with_n_waves(self, n_waves):

        wave_template = self.get_wave_template()
        if n_waves != 1:
            raise NotImplementedError('Other options not implemented yet.')

        return wave_template


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
