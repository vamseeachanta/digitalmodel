def fracture_mechanics(cfg):
    from datetime import datetime
    t_start = datetime.now()

    from pyintegrity.common.fracture_mechanics_components import \
        FractureMechanicsComponents
    fracture_components = FractureMechanicsComponents(cfg)

    if cfg['default']['Analysis']['fracture_mechanics']['flaw_limits']:
        fracture_components.evaluate_FAD()
        fracture_components.critical_flaw_limits()
        fracture_components.get_flaw_growth_for_fatigue_loading()
        fracture_components.get_initial_allowable_flaw_for_life()
        fracture_components.save_result_tables()
        fracture_components.create_visualizations()

    t_end = datetime.now()
    print("Time taken to process: {0} seconds".format((t_end - t_start).seconds))
    return cfg


