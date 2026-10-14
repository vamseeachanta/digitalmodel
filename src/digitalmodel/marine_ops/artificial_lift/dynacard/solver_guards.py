# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Result restrictions shared by every downhole solver and its workflow."""

import warnings

import numpy as np

from .exceptions import PhysicsError

# Two solvers at opposite ends of the maximum 2.20% reference-parity bound
# can disagree by 4.40%. This is a comparison envelope, not physical validation.
COMPARISON_LOAD_NRMSE_LIMIT_PCT = 2.0 * 2.20
# Any recorded departure from vertical is disclosed; this is not an accuracy
# claim for a small inclination or a validated negligible-deviation angle.
DEVIATION_REPORTING_LIMIT_DEG = 0.0
COMPARISON_ONLY_FLAGS = {
    "gibbs": "comparison only: load not propagated",
    "finite_difference": "comparison only: not validated",
}
VERTICAL_APPROXIMATION_FLAG = "deviation present, vertical approximation applied"


def validate_stroke(surface_position, downhole_position, downhole_load, method):
    """Reject non-finite output or stroke exceeding the measured surface range.

    Units cancel in the range comparison; both position arrays must use the
    same units. No divergence opt-in can waive this result restriction.
    """
    arrays = [np.asarray(values, dtype=np.float64)
              for values in (surface_position, downhole_position, downhole_load)]
    if any(values.ndim != 1 or len(values) < 3 for values in arrays):
        raise PhysicsError("card arrays need at least three samples", solver=method)
    if len(arrays[1]) != len(arrays[2]):
        raise PhysicsError("downhole position and load lengths differ", solver=method)
    if any(not np.all(np.isfinite(values)) for values in arrays):
        raise PhysicsError("solver card contains non-finite values", solver=method)
    surface_stroke, downhole_stroke = (float(np.ptp(values)) for values in arrays[:2])
    if downhole_stroke > surface_stroke:
        raise PhysicsError(
            "downhole stroke exceeds surface stroke", solver=method,
            details={"surface_stroke": surface_stroke, "downhole_stroke": downhole_stroke},
        )


def deviation_metadata(inclination_deg, include_gravity):
    """Return the reporting flag and recorded maximum inclination in degrees."""
    if inclination_deg is None or len(inclination_deg) == 0:
        return [], None
    angles = np.asarray(inclination_deg, dtype=np.float64)
    if not np.all(np.isfinite(angles)):
        raise PhysicsError("survey contains non-finite inclination")
    maximum = float(np.max(np.abs(angles)))
    flags = []
    if maximum > DEVIATION_REPORTING_LIMIT_DEG and not include_gravity:
        flags.append(VERTICAL_APPROXIMATION_FLAG)
    return flags, maximum


def annotate_results(results, context, method):
    """Label the unchanged comparison paths and the workflow's vertical datum.

    The Gibbs and legacy finite-difference paths do not use the survey. The
    context adapter uses the unchanged gravity-off Everitt-Jennings default.
    """
    results.solver_method = method
    survey = context.survey
    flags, maximum = deviation_metadata(
        None if survey is None else survey.inclination, include_gravity=False,
    )
    results.max_inclination_deg = maximum
    flag = COMPARISON_ONLY_FLAGS.get(method)
    if flag is not None:
        flags.insert(0, flag)
        warnings.warn(flag, UserWarning, stacklevel=3)
    results.solver_flags = flags
    return results
