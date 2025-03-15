from digitalmodel.modules.orcaflex.orcaflex_utilities import OrcaflexUtilities

ou = OrcaflexUtilities()


class OrcaflexPreProcess:

    def __init__(self):
        pass

    def router(self, cfg):
        if 'preprocess' in cfg['orcaflex']:
            if cfg['orcaflex']['preprocess']['check_yml']:
                self.check_yml_file(cfg)

        return cfg

    def check_yml_file(self, cfg):
        
        pass

        return cfg
