import logging
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
        yml_files = cfg['file_management']['input_files']['yml']
        check_yml_list = []
        for yml_file in yml_files:
            check_yml_dict = self.check_1_yml_file(yml_file)
            check_yml_list.append(check_yml_dict['check_yml_result'])

        cfg['file_management']['input_files'].update({'yml_orcaflex_check': check_yml_list})

        return cfg

    def check_1_yml_file(self, yml_file):
        try:
            import OrcFxAPI
        except Exception:
            raise RuntimeError("OrcaFlex license not available. Run on different computer")

        model = OrcFxAPI.Model()
        check_yml_result = False
        try:
            model.LoadData(yml_file)
            check_yml_result = True
            logging.debug(f'Load yml file: {yml_file} ... PASS')
        except Exception as e:
            logging.error(f'Load yml file: {yml_file} ... FAIL')
            logging.error(e)

        check_yml_dict = {'check_yml_result': check_yml_result}
        return check_yml_dict