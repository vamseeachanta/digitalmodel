from digitalmodel.common.viv_fatigue_analysis_components import VIVFatigueAnalysisComponents

def fatigue_analysis(cfg):
    fatigue_components = VIVFatigueAnalysisComponents(cfg)

    if cfg['default']['Analysis']['wave_fatigue']['orcaflex']:
        fatigue_components.orcaflex_wave_fatigue_analysis()

    if cfg['default']['Analysis']['viv_fatigue']['shear7']:
        fatigue_components.shear7_viv_fatigue_analysis()
        fatigue_components.save_viv_fatigue_life_visualizations()

    if cfg['default']['Analysis']['combined']['fatigue']:
        fatigue_components.get_wave_fatigue_result()
        fatigue_components.get_combined_fatigue()
        fatigue_components.save_results_and_visualizations()

    if cfg['default']['Analysis']['combined']['histograms']:
        fatigue_components.get_viv_histograms()
        fatigue_components.get_viv_cummulative_histograms()
        fatigue_components.save_viv_histograms()
        fatigue_components.get_wave_cummulative_histograms()
        fatigue_components.get_combined_histograms()
        fatigue_components.save_combined_histograms()

    return cfg

