import pathlib
import pandas as pd
import OrcFxAPI
from digitalmodel.modules.orcaflex.orcaflex_preprocess import OrcaflexPreProcess
from digitalmodel.modules.orcaflex.orcaflex_objects import OrcaFlexObjects
from assetutilities.common.utilities import is_file_valid_func


orcaflex_preprocess = OrcaflexPreProcess()
orcaflex_objects = OrcaFlexObjects()

class Mooring():
    def __init__(self):
        pass

    def router(self, cfg):
        groups = cfg['orcaflex_analysis']['mooring']['groups']
        for group in groups:
            if group['calculation'] == 'pretension':
                self.pretension_analysis(cfg, group)
            else:
                raise ValueError('Invalid calculation type for mooring analysis')

    def pretension_analysis(self, cfg, group):
        # utilize model to add moorings
        # utilize winch commands to add pretensions

        yml_files = cfg['file_management']['input_files']['yml']

        tension_df = self.get_tension(cfg, group)
        length_df = self.get_length(cfg, group)

        target_pretension = group['target_pretension']

        for yml_file_idx in range(0, len(yml_files)):
            yml_file = yml_files[yml_file_idx]
            yml_file_stem = pathlib.Path(yml_file).stem


            model = OrcFxAPI.Model()
            model.LoadData(yml_file)

            mooring_lines = group['mooring_lines']
            for mooring_line in mooring_lines:
                ofx_object_cfg = {'ObjectName': mooring_line['name']}
                ofx_object = orcaflex_objects.get_OrcFXAPIObject(model, ofx_object_cfg)
                if ofx_object is None:
                    raise ValueError('Invalid object name. Code not implemented yet')
                
                
                

            model.SaveData(yml_file)

    def get_tension(self, cfg, group):
        tension_cfg = group['tension']
        tension_filename = tension_cfg['filename']

        analysis_root_folder = cfg['Analysis']['analysis_root_folder']
        is_file_valid, tension_filename = is_file_valid_func(tension_filename, analysis_root_folder)

                
        tension_df = pd.read_csv(tension_filename)
        return tension_df

    def get_length(self, cfg, group):
        length_cfg = group['length']
        length_filename = length_cfg['filename']

        analysis_root_folder = cfg['Analysis']['analysis_root_folder']
        is_file_valid, length_filename = is_file_valid_func(length_filename, analysis_root_folder)

                
        length_df = pd.read_csv(length_filename)
        return length_df