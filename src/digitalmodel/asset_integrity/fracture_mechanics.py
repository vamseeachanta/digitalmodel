"""Fracture mechanics assessment entry point for asset integrity."""

import warnings


def fracture_mechanics(cfg):
    """Run configured flaw checks, warning when they are NOT_EVALUATED.

    With ``flaw_limits`` disabled, return the original configuration without
    importing or initializing calculation components. No assessment is made.
    Enabled analysis retains its existing component sequence and return value.
    """
    if not cfg['default']['Analysis']['fracture_mechanics']['flaw_limits']:
        # Explicit disabled-check signal required by issue 2598.
        # https://github.com/vamseeachanta/digitalmodel/issues/2598
        warnings.warn(
            "Fracture mechanics NOT_EVALUATED: flaw_limits is disabled",
            RuntimeWarning,
            stacklevel=2,
        )
        return cfg

    from datetime import datetime
    t_start = datetime.now()

    from .common.fracture_mechanics_components import \
        FractureMechanicsComponents
    fracture_components = FractureMechanicsComponents(cfg)

    fracture_components.evaluate_FAD()
    fracture_components.critical_flaw_limits()
    fracture_components.get_flaw_growth_for_fatigue_loading()
    fracture_components.get_initial_allowable_flaw_for_life()
    fracture_components.save_result_tables()
    fracture_components.create_visualizations()

    t_end = datetime.now()
    print("Time taken to process: {0} seconds".format((t_end - t_start).seconds))
    return cfg


