from digitalmodel.modules.orcaflex.opp_unused import OrcaFlexAnalysis
from digitalmodel.common.orcaflex_iterative_runs import OrcaflexIterativeRuns
from digitalmodel.modules.orcaflex.orcaflex_post_process import orcaflex_post_process
from digitalmodel.custom.orcaflex_utilities import OrcaflexUtilities

ou = OrcaflexUtilities()
oir = OrcaflexIterativeRuns()
opp = orcaflex_post_process()


def orcaflex_analysis(cfg):
    if "file_management" in cfg and cfg["file_management"]["flag"]:
        cfg = ou.file_management(cfg)

    orcaFlex_analysis = OrcaFlexAnalysis(cfg)
    orcaFlex_analysis.file_management(cfg) #TODO delete if redundant

    static_flag = cfg['orcaflex']['analysis']['static']
    simulation_flag = cfg['orcaflex']['analysis']['simulation']
    iterate_flag = cfg['orcaflex']['analysis']['iterate']['flag']

    if static_flag or simulation_flag or iterate_flag:
        orcaflex_license_flag = ou.is_orcaflex_available()
        assert (orcaflex_license_flag)

    if static_flag or simulation_flag:
        orcaFlex_analysis.perform_simulations()

    if iterate_flag:
        oir.prepare_iterative_runs(cfg)
        if cfg['orcaflex']['iterate']['rerun']:
            oir.run_iterative_simulations()

    opp.post_process_router(cfg)

    return cfg

