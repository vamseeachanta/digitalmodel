# Reader imports
from digitalmodel.solvers.orcaflex.orcaflex_preprocess import OrcaflexPreProcess
from digitalmodel.solvers.orcaflex.orcaflex_utilities import OrcaflexUtilities
from digitalmodel.solvers.orcaflex.orcaflex_analysis import OrcaflexAnalysis
from digitalmodel.solvers.orcaflex.opp import OrcaFlexPostProcess
from digitalmodel.solvers.orcaflex.all_vars import AllVars

ou = OrcaflexUtilities()
orcaflex_preprocess = OrcaflexPreProcess()
orcaflex_analysis = OrcaflexAnalysis()
opp = OrcaFlexPostProcess()
all_vars = AllVars()  # noqa


class OrcaFlex:
    def __init__(self):
        pass

    def router(self, cfg):

        # if "file_management" in cfg and cfg["file_management"]["flag"]:
        #     cfg = ou.file_management(cfg)

        orcaflex_preprocess.router(cfg)

        orcaflex_analysis.router(cfg)

        opp.post_process_router(cfg)

        all_vars.router(cfg)

        return cfg
