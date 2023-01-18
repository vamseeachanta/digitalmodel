# -*- coding: utf-8 -*-
"""
Author: Vamsee Achanta
Date Updated: 2019-05-03
Objective: To generate vertical riser model and evaluate top tension 
Created on May 20 2019
"""


def set_up_application():
    import logging
    import os

    from common.ApplicationManager import configureApplicationInputs
    from common.set_logging import set_logging

    basename = os.path.basename(__file__).split('.')[0]
    application_manager = configureApplicationInputs(basename)
    application_manager.configure()

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
            cfg_variations_array.append(vertical_riser(application_manager.cfg))

    return cfg_variations_array


def vertical_riser(cfg):
    from custom.vertical_riser_components import VerticalRiser

    riser_model = VerticalRiser(cfg)
    riser_model.prepare_riser_model()
    riser_model.prepare_fea_model()
    riser_model.build_fea_model()
    riser_model.prepare_plots_and_save_data()

    if cfg['default']['analysis']['loading_type']['VIV']:
        riser_model.alter_properties_for_Shear7_software()
        riser_model.prepare_fea_model()
        riser_model.build_fea_model(shear7_flag=True)
        riser_model.prepare_shear7_model()
    return cfg


if __name__ == '__main__':
    application_manager = set_up_application()
    cfg_base = vertical_riser(application_manager.cfg)
    cfg_variations_array = run_cfg_variations(application_manager)
