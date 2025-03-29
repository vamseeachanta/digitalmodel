import OrcFxAPI
import logging

from digitalmodel.modules.orcaflex.preprocess.load_vessel import LoadVessel

opreproc_vessel = LoadVessel()

class OrcaflexPreProcess:

    def __init__(self):
        pass

    def router(self, cfg):
        if 'preprocess' in cfg['orcaflex']:
            check_yml_flag = cfg['orcaflex']['preprocess'].get('check_yml', {}).get('flag', False)
            if check_yml_flag:
                self.check_yml_file(cfg)
            load_vessel_flag = cfg['orcaflex']['preprocess'].get('load_vessel', {}).get('flag', False)
            if load_vessel_flag:
                cfg = opreproc_vessel.router(cfg)

        return cfg

    def check_yml_file(self, cfg):
        model = OrcFxAPI.Model()

        yml_files = cfg['file_management']['input_files']['yml']
        for yml_file in yml_files:
            try:
                model.LoadData(yml_file)
                logging.info(f'Load yml file: {yml_file} ... PASS')
            except Exception as e:
                logging.error(f'Load yml file: {yml_file} ... FAIL')
                logging.error(e)

        return cfg
