from digitalmodel.custom.orcaflex_utilities import OrcaflexUtilities

ou = OrcaflexUtilities()


class OrcaflexFileManagement:

    def __init__(self):
        pass

    def orcaflex_pre_analysis(self, cfg):
            
        if cfg.basename in ['orcaflex_file_preparation']:
            cfg = ou.prepare_operating_window_definition(cfg)
        elif cfg.basename in ['orcaflex_file_management']:
            self.file_management(cfg)

        return cfg

    def file_management(self, cfg):

        if cfg.file_management['update_unfinished']['flag']:
            cfg = ou.sim_file_analysis_and_update(cfg)

        return cfg
    