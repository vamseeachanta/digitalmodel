from common.ApplicationManager import applicationTimer, setupApplicationRuns


@setupApplicationRuns
@applicationTimer
def orcaflex_analysis(cfg):
    from custom.orcaflex_analysis_components import OrcaFlexAnalysis
    orcaFlex_analysis = OrcaFlexAnalysis(cfg)

    orcaFlex_analysis.get_files()
    orcaFlex_analysis.perform_analysis()

    return cfg




if __name__ == '__main__':
    cfg_with_results = orcaflex_analysis(cfg=None)
