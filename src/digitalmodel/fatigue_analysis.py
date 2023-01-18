# Data preparation
def set_up_application():
    import logging
    import os

    from common.ApplicationManager import configureApplicationInputs
    from common.set_logging import set_logging

    basename = os.path.basename(__file__).split('.')[0]
    application_manager = configureApplicationInputs(basename)
    application_manager.configure()

    # Set logging
    set_logging(application_manager.cfg)
    logging.info(application_manager.cfg)

    return application_manager


def run_cfg_variations(application_manager):
    application_manager.cfg_variation_type = 'pre_analysis'
    run_cfg_variations_by_type(application_manager)
    application_manager.cfg_variation_type = 'post_analysis'
    cfg_variations_array = run_cfg_variations_by_type(application_manager)
    return cfg_variations_array


def run_cfg_variations_by_type(application_manager):
    cfg_variations_array = []
    if application_manager.cfg['cfg_variations'][application_manager.cfg_variation_type] is not None:
        for run_number in range(
                0, len(application_manager.cfg['cfg_variations'][application_manager.cfg_variation_type])):
            application_manager.update_cfg_with_variation(run_number)
            cfg_variations_array.append(fatigue_analysis(application_manager.cfg))

    return cfg_variations_array


def fatigue_analysis(cfg):
    from common.fatigue_analysis_components import FatigueAnalysisComponents
    fatigue_components = FatigueAnalysisComponents(cfg)

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


if __name__ == '__main__':
    application_manager = set_up_application()
    cfg_base = fatigue_analysis(application_manager.cfg)
    cfg_variations_array = run_cfg_variations(application_manager)
