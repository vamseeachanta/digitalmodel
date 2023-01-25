from digitalmodel.custom.orcaflex_analysis_components import OrcaFlexAnalysis


def orcaflex_analysis(cfg):
    orcaFlex_analysis = OrcaFlexAnalysis(cfg)

    orcaFlex_analysis.get_files()
    orcaFlex_analysis.perform_analysis()

    return cfg
