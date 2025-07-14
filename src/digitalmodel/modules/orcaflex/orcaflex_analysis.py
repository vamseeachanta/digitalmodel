# Reader imports
from digitalmodel.modules.orcaflex.orcaflex_preprocess import OrcaflexPreProcess
from digitalmodel.modules.orcaflex.orcaflex_iterative_runs import OrcaflexIterativeRuns
from digitalmodel.modules.orcaflex.orcaflex_utilities import OrcaflexUtilities
from digitalmodel.modules.orcaflex.opp import OrcaFlexPostProcess
from digitalmodel.modules.orcaflex.mooring import Mooring


mooring = Mooring()
ou = OrcaflexUtilities()
oir = OrcaflexIterativeRuns()
opp = OrcaFlexPostProcess()


class OrcaflexAnalysis:
    def __init__(self):
        pass

    def router(self, cfg):

        static_flag = False
        simulation_flag = False
        iterate_flag = False
        mooring_flag = False
        if "analysis" in cfg["orcaflex"]:
            static_flag = cfg["orcaflex"]["analysis"].get("static", False)
            simulation_flag = cfg["orcaflex"]["analysis"].get("simulation", False)
            iterate_flag = (
                cfg["orcaflex"]["analysis"].get("iterate", {}).get("flag", False)
            )
            mooring_flag = (
                cfg["orcaflex"]["analysis"].get("mooring", {}).get("flag", False)
            )

        if static_flag or simulation_flag or iterate_flag:
            orcaflex_license_flag = ou.is_orcaflex_available()
            assert orcaflex_license_flag

        if static_flag or simulation_flag:
            orcaFlex_analysis = OrcaFlexPostProcess(cfg)
            orcaFlex_analysis.perform_simulations()

        if iterate_flag:
            oir.prepare_iterative_runs(cfg)
            if cfg["orcaflex"]["iterate"]["rerun"]:
                oir.run_iterative_simulations()

        if mooring_flag:
            mooring.router(cfg)

        return cfg
