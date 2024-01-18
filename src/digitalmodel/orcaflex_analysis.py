from digitalmodel.custom.orcaflex_analysis_components import OrcaFlexAnalysis
from digitalmodel.common.orcaflex_iterative_runs import OrcaflexIterativeRuns

oir = OrcaflexIterativeRuns()


def orcaflex_analysis(cfg):
    orcaFlex_analysis = OrcaFlexAnalysis(cfg)
    orcaFlex_analysis.get_files()

    orcaFlex_analysis.perform_simulations()

    if cfg['orcaflex']['iterate']['flag']:
        oir.prepare_iterative_runs(cfg)
        if cfg['orcaflex']['iterate']['rerun']:
            oir.run_iterative_simulations()

    return cfg
