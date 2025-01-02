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
            cfg_variations_array.append(viv_analysis(application_manager.cfg))

    return cfg_variations_array


def viv_analysis(cfg):
    from datetime import datetime
    t_start = datetime.now()

    from common.viv_analysis_components import VIVAnalysisComponents
    viv_components = VIVAnalysisComponents(cfg)

    if cfg['default']['Analysis']['common.mds']:
        viv_components.modal_analysis()
        viv_components.save_modal_visualizations()
        viv_components.save_mode_shapes()

    if cfg['default']['Analysis']['viv_fatigue']['shear7']:
        from common.fatigue_analysis_components import \
            FatigueAnalysisComponents
        fatigue_components = FatigueAnalysisComponents(cfg)
        fatigue_components.shear7_viv_fatigue_analysis()
        fatigue_components.save_viv_fatigue_life_visualizations()

    if cfg['default']['Analysis'].__contains__('current_data') and cfg['default']['Analysis']['current_data']['plot']:
        viv_components.get_current_data()
        viv_components.plot_current_profiles()
        # TODO Exceedance and non-exceedance terms are not consistent in input files and code. Unify and make them consistent
        # TODO make exceedance a function of non-exceedance (a sepearate function can make things erroneous and code may be repetitive)
        viv_components.plot_current_exceedance()
        viv_components.plot_current_non_exceedance()
        print("perform current data assessment .. COMPLETE")

    if cfg['default']['Analysis'].__contains__('wave_data') and cfg['default']['Analysis']['wave_data']['plot']:
        viv_components.get_wave_data()
        viv_components.plot_wave_data()
        viv_components.plot_wave_exceedance()
        # viv_components.plot_wave_non_exceedance()
        print("perform wave data assessment .. COMPLETE")

    t_end = datetime.now()
    print("Time taken to process: {0} seconds".format((t_end - t_start).seconds))
    return cfg


if __name__ == '__main__':
    application_manager = set_up_application()
    cfg_base = viv_analysis(application_manager.cfg)
    cfg_variations_array = run_cfg_variations(application_manager)
