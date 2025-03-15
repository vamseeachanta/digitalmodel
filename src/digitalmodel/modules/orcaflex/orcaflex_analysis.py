# Reader imports
from digitalmodel.modules.orcaflex.orcaflex_preprocess import OrcaflexPreProcess
from digitalmodel.modules.orcaflex.orcaflex_iterative_runs import OrcaflexIterativeRuns
from digitalmodel.modules.orcaflex.orcaflex_utilities import OrcaflexUtilities
from digitalmodel.modules.orcaflex.opp import OrcaFlexPostProcess


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
        if 'analysis' in cfg['orcaflex']:
            static_flag = cfg['orcaflex']['analysis']['static']
            simulation_flag = cfg['orcaflex']['analysis']['simulation']
            iterate_flag = cfg['orcaflex']['analysis']['iterate']['flag']

        if static_flag or simulation_flag or iterate_flag:
            orcaflex_license_flag = ou.is_orcaflex_available()
            assert (orcaflex_license_flag)

        if static_flag or simulation_flag:
            orcaFlex_analysis = OrcaFlexPostProcess(cfg)
            orcaFlex_analysis.perform_simulations()

        if iterate_flag:
            oir.prepare_iterative_runs(cfg)
            if cfg['orcaflex']['iterate']['rerun']:
                oir.run_iterative_simulations()

        return cfg

