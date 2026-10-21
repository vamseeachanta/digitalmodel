# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic study inputs, methods and answers; no external execution."""

import json
from typing import Any

from flask import Flask

from digitalmodel.common.spec_authoring import ProjectBundle
from digitalmodel.common.units import length_in_metres
from digitalmodel.contract_validation.riser_length import RiserLengthContract
from digitalmodel.solvers.smoke.model_results import numbers
from digitalmodel.specs.engineering_result import EngineeringResult
from digitalmodel.web.engineering_results import render_engineering_result


def build_example() -> dict[str, Any]:
    """Return hand-checkable values and the corresponding HTML fragment.

    Every physical input below is synthetic, not a design or material default.
    Expected answers and arithmetic are documented in the companion guide.
    """
    # NIST SP 811 (2008), Appendix B.8: 10 international feet = 3.048 metres.
    bundle = ProjectBundle.model_validate_json(
        '{"units":"ft","vessel_particulars":{"length":10.0}}'
    )
    dimension_m = length_in_metres(bundle.vessel_particulars["length"], bundle.units)

    # Geometric additivity, synthetic sections: 12.5+7.5+10 = 30 metres.
    # Exact-zero tolerance is an explicit input for these binary-exact operands.
    riser = RiserLengthContract(
        segments_m=(12.5, 7.5, 10.0),
        total_m=30.0,
        absolute_tolerance_m=0.0,
        units="m",
    )

    # OpenStax UP1 (2016), 14.1, eq. 14.4: synthetic gauge p = rho*g*h.
    density_kg_m3, gravity_m_s2, depth_m = 1000.0, 10.0, 2.5
    pressure = EngineeringResult(
        quantity="gauge pressure",
        value=density_kg_m3 * gravity_m_s2 * depth_m,
        units="Pa",
        source="OpenStax UP1 (2016), 14.1, eq. 14.4",
    )
    admitted = EngineeringResult.model_validate_json(pressure.model_dump_json())
    with Flask(__name__).app_context():
        html = render_engineering_result(admitted)

    # OpenStax UP1 (2016), 12.3, eq. 12.36: delta = P*L/(E*A).
    load_n, length_m, youngs_pa, area_m2 = 2000.0, 2.0, 200000000.0, 0.01
    elongation_m = load_n * length_m / (youngs_pa * area_m2)
    # Synthetic serialization/readback only; no solver produces this number.
    readback = numbers(json.loads(json.dumps([elongation_m])))

    return {
        "dimension_m": dimension_m,
        "riser_total_m": riser.total_m,
        "pressure_pa": admitted.value,
        "bar_elongation_m": readback[0],
        "pressure_html": html,
        "native_bar_solution": "NOT_EVALUATED",
        "native_box_export": "NOT_EVALUATED",
    }


if __name__ == "__main__":
    print(json.dumps(build_example(), indent=2))
