import OrcFxAPI
from digitalmodel.modules.orcaflex.orcaflex_preprocess import OrcaflexPreProcess
from digitalmodel.modules.orcaflex.orcaflex_objects import OrcaFlexObjects

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
        
        for yml_file_idx in range(0, len(yml_files)):
            yml_file = yml_files[yml_file_idx]

            model = OrcFxAPI.Model()
            model.LoadData(yml_file)

            mooring_lines = group['mooring_lines']
            for mooring_line in mooring_lines:
                ofx_object_cfg = {'ObjectName': mooring_line['name']}
                ofx_object = orcaflex_objects.get_OrcFXAPIObject(model, ofx_object_cfg)
                if ofx_object is None:
                    raise ValueError('Invalid object name. Code not implemented yet')
                
                
                

            model.SaveData(yml_file)
            
        #TODO Aborted and changed to restart method