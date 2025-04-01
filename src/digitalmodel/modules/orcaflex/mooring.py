from digitalmodel.modules.orcaflex.orcaflex_preprocess import OrcaflexPreProcess

orcaflex_preprocess = OrcaflexPreProcess()


class Mooring():
    def __init__(self):
        pass

    def router(self, cfg):
        groups = cfg['analysis']['mooring']['groups']
        for group in groups:
            if group['calculation'] == 'pretension':
                self.pretension_analysis(cfg, group)
            else:
                raise ValueError('Invalid calculation type for mooring analysis')

    def pretension_analysis(self, cfg, group):
        # check yml load
        # utilize model to add moorings
        # utilize winch commands to add pretensions

        pass