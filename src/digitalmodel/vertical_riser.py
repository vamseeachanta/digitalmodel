# -*- coding: utf-8 -*-
"""
Author: Vamsee Achanta
Date Updated: 2019-05-03
Objective: To generate vertical riser model and evaluate top tension 
Created on May 20 2019
"""

from digitalmodel.custom.vertical_riser_components import VerticalRiser


def vertical_riser(cfg):

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

