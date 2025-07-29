# Data preparation
def set_up_application():
    from loguru import logger
    import os

    from assetutilities.common.ApplicationManager import ConfigureApplicationInputs

    basename = os.path.basename(__file__).split(".")[0]
    application_manager = ConfigureApplicationInputs(basename)
    application_manager.configure()

    # Set logging
    # set_logging(application_manager.cfg)  # Remove if not used
    logger.info(application_manager.cfg)

    return application_manager


def run_cfg_variations(application_manager):
    application_manager.cfg_variation_type = "pre_analysis"
    run_cfg_variations_by_type(application_manager)
    application_manager.cfg_variation_type = "post_analysis"
    cfg_variations_array = run_cfg_variations_by_type(application_manager)
    return cfg_variations_array


def run_cfg_variations_by_type(application_manager):
    cfg_variations_array = []
    cfgs = application_manager.cfg["cfg_variations"]
    cfg_type = application_manager.cfg_variation_type
    if cfgs[cfg_type] is not None:
        for run_number in range(0, len(cfgs[cfg_type])):
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

    analysis = cfg["default"]["analysis"]
    if "run_example" in analysis and analysis["run_example"]:
        ts.run_example()

    if "moving_average" in analysis and analysis["moving_average"]["flag"]:
        ts.simple_moving_average()

    if "statistics" in analysis and analysis["statistics"]["flag"]:
        ts.perform_statistics()

    if "filter" in analysis and analysis["filter"]["flag"]:
        ts.filter_by_value()

    if "fft" in analysis and analysis["fft"]["flag"]:
        ts.perform_fft_analysis()

    if "custom_calculation_1" in analysis and analysis["custom_calculation_1"]["flag"]:
        ts.perform_custom_calculation_1()

    if "custom_calculation_2" in analysis and analysis["custom_calculation_2"]["flag"]:
        ts.perform_custom_calculation_2()

    if "integration" in analysis and analysis["integration"]["flag"]:
        ts.perform_integration()

    ts.prepare_visualizations()

    t_end = datetime.now()
    print("Time taken to process: {0} seconds".format((t_end - t_start).seconds))
    return cfg


if __name__ == "__main__":
    application_manager = set_up_application()
    cfg_base = time_series(application_manager.cfg)
    cfg_variations_array = run_cfg_variations(application_manager)
