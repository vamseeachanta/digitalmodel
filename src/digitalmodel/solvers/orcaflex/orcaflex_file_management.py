from digitalmodel.solvers.orcaflex.orcaflex_utilities import OrcaflexUtilities

ou = OrcaflexUtilities()


class OrcaflexFileManagement:
    def __init__(self):
        pass

    def file_management(self, cfg):

        if cfg.basename in ["orcaflex_file_preparation"]:
            if cfg.operating_envelopes:
                cfg = ou.prepare_operating_window_definition(cfg)
            else:
                raise NotImplementedError("Other options not implemented yet.")
        elif cfg.basename in ["orcaflex_file_management"]:
            self.sim_file_analysis_and_update(cfg)

        return cfg

    def sim_file_analysis_and_update(self, cfg):

        if cfg.file_management["update_unfinished"]["flag"]:
            cfg = ou.sim_file_analysis_and_update(cfg)

        return cfg
