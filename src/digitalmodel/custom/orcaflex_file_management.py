from digitalmodel.custom.orcaflex_utilities import OrcaflexUtilities

ou = OrcaflexUtilities()


class OrcaflexFileManagement:

    def __init__(self):
        pass

    def file_management(self, cfg):

        if cfg.file_management['update_unfinished']['flag']:
            cfg = ou.sim_file_analysis_and_update(cfg)

        return cfg
    