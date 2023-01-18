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
            cfg_variations_array.append(time_series(application_manager.cfg))

    return cfg_variations_array


def time_series(cfg):
    from datetime import datetime
    t_start = datetime.now()

    from common.time_series_components import TimeSeriesComponents
    ts = TimeSeriesComponents(cfg)

    ts.get_raw_data()

    ts.perform_db_analysis()

    if cfg['default']['analysis'].__contains__('run_example') and cfg['default']['analysis']['run_example']:
        ts.run_example()

    if cfg['default']['analysis'].__contains__(
            'moving_average') and cfg['default']['analysis']['moving_average']['flag']:
        ts.simple_moving_average()

    if cfg['default']['analysis'].__contains__('statistics') and cfg['default']['analysis']['statistics']['flag']:
        ts.perform_statistics()

    if cfg['default']['analysis'].__contains__('filter') and cfg['default']['analysis']['filter']['flag']:
        ts.filter_by_value()

    if cfg['default']['analysis'].__contains__('fft') and cfg['default']['analysis']['fft']['flag']:
        ts.perform_fft_analysis()

    if cfg['default']['analysis'].__contains__(
            'custom_calculation_1') and cfg['default']['analysis']['custom_calculation_1']['flag']:
        ts.perform_custom_calculation_1()

    if cfg['default']['analysis'].__contains__(
            'custom_calculation_2') and cfg['default']['analysis']['custom_calculation_2']['flag']:
        ts.perform_custom_calculation_2()

    if cfg['default']['analysis'].__contains__('integration') and cfg['default']['analysis']['integration']['flag']:
        ts.perform_integration()

    ts.prepare_visualizations()

    t_end = datetime.now()
    print("Time taken to process: {0} seconds".format((t_end - t_start).seconds))
    return cfg


if __name__ == '__main__':
    application_manager = set_up_application()
    cfg_base = time_series(application_manager.cfg)
    cfg_variations_array = run_cfg_variations(application_manager)
