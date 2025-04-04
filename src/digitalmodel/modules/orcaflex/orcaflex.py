# Reader imports
from digitalmodel.modules.orcaflex.orcaflex_preprocess import OrcaflexPreProcess
from digitalmodel.modules.orcaflex.orcaflex_utilities import OrcaflexUtilities
from digitalmodel.modules.orcaflex.orcaflex_analysis import OrcaflexAnalysis
from digitalmodel.modules.orcaflex.opp import OrcaFlexPostProcess


ou = OrcaflexUtilities()
orcaflex_preprocess = OrcaflexPreProcess()
orcaflex_analysis = OrcaflexAnalysis()
opp = OrcaFlexPostProcess()


class OrcaFlex:

    def __init__(self):
        pass

    def router(self, cfg):

        # if "file_management" in cfg and cfg["file_management"]["flag"]:
        #     cfg = ou.file_management(cfg)

        orcaflex_preprocess.router(cfg)

        orcaflex_analysis.router(cfg)

        opp.post_process_router(cfg)

        return cfg

