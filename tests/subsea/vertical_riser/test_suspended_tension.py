# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Hand-calculated force balance through the existing vertical-riser path.

Sources (equations, not published numerical examples):
* OpenStax, University Physics Volume 1 (2016), section 12.1, first equilibrium
  condition: sum of external forces is zero, hence T_top = T_bottom + w*L.
  https://openstax.org/books/university-physics-volume-1/pages/12-1-conditions-for-static-equilibrium
* BIPM, SI Brochure, ninth edition (2019), Appendix 1, third CGPM (1901),
  declaration on weight, points 2 and 3: weight = mass*g and standard gravity.
  https://www.bipm.org/documents/20126/41483022/SI-Brochure-9.pdf
* NIST, Guide to the SI, Special Publication 811 (2008), Appendix B.8, foot
  and kip: 1 ft = 0.3048 m exactly; 1 kip = 1000 lbf = 4.448222 kN
  (the tabulated conversion precision).
  https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication811e2008.pdf

All dimensions and mass properties below are synthetic test inputs. The wet
mass-equivalent is supplied, not derived from a material density or guessed
buoyancy coefficient. Equal internal/external fluid densities remove the net
contents contribution. No design acceptance limit is evaluated.
"""

import pandas as pd
import pytest

from digitalmodel.subsea.vertical_riser.vertical_riser_components import VerticalRiser


# Known deviation: the implementation's uncited g = 9.81 differs from standard
# gravity: 9.81 / 9.80665 - 1 ~= 3.42e-4. Overpull also uses a rounded
# pound-to-kilogram conversion: 0.4536 * 9.81 / 4.448222 - 1 ~= 3.58e-4.
# Both deviations are positive; for these nonnegative force terms, the
# top-tension relative deviation is a weighted average, below 3.59e-4.
# The 5e-4 relative tolerance covers those known legacy deviations only;
# it is not an engineering acceptance limit.
FORCE_REL_TOL = 5e-4


@pytest.mark.parametrize(
    "overpull_kip, expected_bottom_kn", [(0, 0.0), (1, 4.448222), (25, 111.20555)]
)
@pytest.mark.parametrize(
    "joint_length_ft, joint_count, expected_length_m, expected_weight_kn",
    [
        (100, 1, 30.48, 2.98906692),
        (100, 2, 60.96, 5.97813384),
        (40, 3, 36.576, 3.586880304),
    ],
)
def test_uniform_suspended_pipe_top_effective_tension(
    joint_length_ft,
    joint_count,
    expected_length_m,
    expected_weight_kn,
    overpull_kip,
    expected_bottom_kn,
):
    """Check upper support with zero or specified lower-end overpull.

    Independent hand calculation, using the sources in the module docstring:
    Standard gravity g = 980.665 / 100 = 9.80665 m/s^2.
    w = 10 kg/m * 9.80665 m/s^2 / 1000 N/kN = 0.0980665 kN/m.
    With zero overpull, T_bottom = 0 kN. For the three synthetic arrangements:
    L = 100 * 1 * 0.3048 = 30.48 m; T_top = 0.0980665 * 30.48 = 2.98906692 kN.
    L = 100 * 2 * 0.3048 = 60.96 m; T_top = 0.0980665 * 60.96 = 5.97813384 kN.
    L = 40 * 3 * 0.3048 = 36.576 m; T_top = 0.0980665 * 36.576 = 3.586880304 kN.
    For nonzero overpull, T_bottom = overpull_kip * 4.448222 kN/kip (NIST).
    T_top = T_bottom + w*L; the support reaction has the same magnitude.
    These are force assertions, not bounds on an acceptable riser design.
    """
    # Synthetic flooded pipe with upper support; overpull "value" is in kip.
    riser = VerticalRiser(
        {
            "Analysis": {},
            "stack_up": {
                "overpull": {
                    "top_tensioned": {
                        "bottom_of_component": "Wellhead",
                        "value": overpull_kip,
                    }
                }
            },
            "Material": {"Fluid": {"Rho": 1000}, "SeaWater": {"Rho": 1000}},
        }
    )
    riser.riser_type = "top_tensioned"
    riser.support_spring = True
    # Synthetic water depth exceeds every suspended length in this test.
    riser.water_depth = 1000.0
    # Object dtype preserves the input reader's Python joint-count integers.
    riser.stack_up_table = pd.DataFrame(
        {
            "Component": ["Upper end", "Uniform pipe", "Wellhead"],
            "No. Of. Joints": [0, joint_count, 0],
            "Component Length": [0, joint_length_ft, 0],
            "Component Top Elevation above Mudline": [0, 0, 0],
        },
        dtype=object,
    )
    riser.riser_joints = pd.DataFrame({"Component": ["Uniform pipe"]})
    # Synthetic positive placeholders for geometry/stiffness are not validation
    # targets here; only the supplied wet mass-equivalent enters the force sum.
    # Dry mass is converted below but does not enter this fully submerged sum.
    pipe = dict.fromkeys(
        ("StressOD", "StressID", "drag_diameter", "buoyancy_diameter",
         "internal_fluid_ID", "A", "I", "J", "EI", "EA", "GJ"),
        1.0,
    )
    pipe.update(
        Name="Uniform pipe", MassPerUnitLength=20.0, wet_weight_per_unit_length=10.0
    )
    riser.pipe_properties = [{"section_properties": {"pipe": pipe}}]

    riser.evaluate_stack_up_properties()

    actual = riser.stack_up_properties_df.iloc[1]
    # Length tolerance covers floating-point arithmetic only (metres).
    # Force expectations use standard gravity with the known deviation above.
    assert actual["stack_length"] == pytest.approx(expected_length_m, rel=0, abs=1e-12)
    assert actual["wet_weight_per_unit_length"] == pytest.approx(
        0.0980665, rel=FORCE_REL_TOL, abs=1e-12
    )
    # 20 kg/m * standard gravity / 1000 N/kN, independent of the wet input.
    assert actual["dry_weight_per_unit_length"] == pytest.approx(
        0.196133, rel=FORCE_REL_TOL, abs=1e-12
    )
    assert actual["stack_in_service_weight"] == pytest.approx(
        expected_weight_kn, rel=FORCE_REL_TOL, abs=1e-12
    )
    assert actual["component_bottom_effective_tension"] == pytest.approx(
        expected_bottom_kn, rel=FORCE_REL_TOL, abs=1e-12
    )
    expected_top_kn = expected_bottom_kn + expected_weight_kn
    assert actual["component_top_effective_tension"] == pytest.approx(
        expected_top_kn, rel=FORCE_REL_TOL, abs=1e-12
    )
    assert riser.total_support_tension == pytest.approx(
        expected_top_kn, rel=FORCE_REL_TOL, abs=1e-12
    )
    # Keep the equilibrium identity tight despite the conversion tolerance.
    assert (
        actual["component_top_effective_tension"]
        - actual["component_bottom_effective_tension"]
    ) == pytest.approx(actual["stack_in_service_weight"], rel=0, abs=1e-12)
