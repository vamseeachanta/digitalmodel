"""Synthetic scatter and check-status example; no fracture solver is loaded.

Run from the repository root: PYTHONPATH=src python examples/asset_integrity/check_status.py
All readings, geometry, strength and pressures below are invented arithmetic
inputs; they are not measured data or qualified engineering design inputs.

Scatter method: NIST/SEMATECH e-Handbook, online edition accessed 2026-10-21,
sections 1.3.5.8 and 1.3.6.6.6:
https://www.itl.nist.gov/div898/handbook/eda/section3/eda358.htm
https://www.itl.nist.gov/div898/handbook/eda/section3/eda3666.htm
For [-1, 0, 1], mean=0, sum of squares=2, df=2 and sample SD=1 mm.
The df=2 quantile is -2*ln(1-p); equal tails p=0.025 and p=0.975 give
SD endpoints sqrt(2/q(0.975)) and sqrt(2/q(0.025)), or 0.5 and 6.3 mm
to one decimal place. Display rounding does not change the calculation.
"""

import warnings

from digitalmodel.asset_integrity.campaign_comparability import gauge_from_short_interval
from digitalmodel.asset_integrity.fracture_mechanics import fracture_mechanics
from digitalmodel.asset_integrity.rsf_calculations import check_ffs_level1


def main():
    """Print the inputs, scatter answer, invalid-input error and disabled status."""
    differences_mm = [-1.0, 0.0, 1.0]
    gauge = gauge_from_short_interval(differences_mm)
    lower, upper = gauge.difference_sd_ci95
    print(f"Synthetic differences (mm): {differences_mm}")
    print(f"Sample count: {gauge.n}; degrees of freedom: {gauge.n - 1}")
    print(f"Difference SD (mm): {gauge.difference_sd:.1f}")
    print(f"95% SD interval (mm): [{lower:.1f}, {upper:.1f}]")

    # Independent substitution into the existing helper's documented relation:
    # P = 2 * SMYS * E * t / OD = 2 * 12000 * 1 * 0.500 / 12 = 1000 psi.
    # P/P_design = 1000/1000 = 1; no standard acceptance is inferred.
    pressure_inputs = dict(
        t_measured=0.500, t_required=0.250, t_nominal=0.500,
        smys_psi=12000.0, outer_diameter_in=12.0,
    )
    result = check_ffs_level1(**pressure_inputs, design_pressure_psi=1000.0)
    print(f"Pressure ratio (synthetic): {result['remaining_pressure_fraction']:.1f}")
    try:
        check_ffs_level1(**pressure_inputs, design_pressure_psi=0.0)
    except ValueError as error:
        print(f"Rejected pressure: {error}")

    cfg = {"default": {"Analysis": {"fracture_mechanics": {"flaw_limits": False}}}}
    # Capture and display the warning; no component initialization occurs.
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always", RuntimeWarning)
        fracture_mechanics(cfg)
    for warning in caught:
        print(str(warning.message))


if __name__ == "__main__":
    main()
