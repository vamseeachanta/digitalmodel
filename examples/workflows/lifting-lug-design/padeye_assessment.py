# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Generate the synthetic padeye analysis from explicit illustrative inputs."""

from __future__ import annotations

import argparse
from pathlib import Path

from digitalmodel.lifting_lug.assessment import (
    DirectWeld,
    PadeyeAllowables,
    PadeyeAssessment,
    PlateGeometry,
    SlingLoad,
    StressLimit,
    assess_padeye,
)
from digitalmodel.lifting_lug.report import render_report


BASIS = (
    "Synthetic illustration; assumed, based on previous experience; "
    "values are illustrative inputs, not ABS or lifting-standard limits"
)


def synthetic_assessment() -> PadeyeAssessment:
    """Return the worked case; the independent arithmetic is in the tests."""
    # Every numeric input below is synthetic, with BASIS as its assumption source.
    # Nominal stress equations: MIT Stress-Strain Curves (2001), Eq. (1), and
    # MIT Shear and Torsion (2000), p. 2. No resistance value comes from a rule.
    return assess_padeye(
        plate=PlateGeometry(
            thickness_mm=20, pin_diameter_mm=40, hole_diameter_mm=50,
            width_mm=150, centre_to_loaded_edge_mm=75,
        ),
        load=SlingLoad(
            static_load_kn=100, dynamic_amplification=1.5,
            skew_factor=1.2, basis=BASIS,
        ),
        allowables=PadeyeAllowables(
            pin_bearing=StressLimit(300, BASIS),
            net_tension=StressLimit(120, BASIS),
            shear_tearout=StressLimit(100, BASIS),
            weld_direct_shear=StressLimit(100, BASIS),
        ),
        weld=DirectWeld(effective_length_mm=200, effective_throat_mm=8),
    )


def main() -> None:
    """Write only the report path explicitly requested by the caller."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    result = synthetic_assessment()
    args.output.write_text(render_report(result), encoding="utf-8")
    print(f"screening={result.screening_status}; qualification=NOT_EVALUATED")


if __name__ == "__main__":
    main()
