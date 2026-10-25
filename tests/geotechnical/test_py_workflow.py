# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Profile assembly and a classical elastic-foundation benchmark.

Beams on Elastic Foundation (1946), section 9, equation 19a, printed page 24:
y0=2 H beta/k=H/(2 EI beta**3), beta=(k/(4EI))**0.25.
Open scan: https://archive.org/details/in.ernet.dli.2015.73919
All soil and numerical-control values in these tests are assumed examples.
"""

import math

import pytest

from digitalmodel.geotechnical.database_schema import ParameterRow, Quantity, Source, UnitValue
from digitalmodel.geotechnical.py_curves import (
    ClayLayer,
    Curve,
    CyclicDegradation,
    SoftClay,
    assemble_py_family,
    clay_layer_from_database,
    solve_lateral_pile,
)


def quantity(name, unit, value=None, minimum=None, maximum=None, soil_unit="assumed clay"):
    source = Source("example", "Assumed arithmetic example", "local example",
                    "Assumed input", "yes", "read")
    row = ParameterRow(name, soil_unit, name, "assumed", unit, "assumed",
                       "assumed", "caller-selected", "example", "example",
                       "BY", "B", source)
    si = UnitValue(unit, value, minimum, maximum)
    return Quantity(row, name, "assumed", unit, "", si, si, "")


def rows():
    return dict(su=quantity("strength", "Pa", minimum=10000, maximum=30000),
                effective_unit_weight=quantity("effective weight", "N/m**3", 6000),
                eps50=quantity("strain", "dimensionless", 0.02),
                j=quantity("J", "dimensionless", 0.5))


SELECTIONS = dict(su=20000, effective_unit_weight=6000, eps50=0.02, j=0.5)


def test_database_adapter_requires_explicit_selections():
    """SI-to-engineering conversion: 20000 Pa=20 kPa; 6000 N/m³=6 kN/m³."""
    layer = clay_layer_from_database(top_m=0, bottom_m=5, quantities=rows(),
                                     selections_si=SELECTIONS)
    assert layer.soil == SoftClay(20, 6, 0.02, 0.5)
    assert layer.source_row_ids == ("strength", "effective weight", "strain", "J")


@pytest.mark.parametrize("change", [dict(su=50000), dict(eps50=0.03),
                                     dict(su=math.nan), dict(j=-1)])
def test_reject_selection_outside_database_value(change):
    with pytest.raises(ValueError):
        clay_layer_from_database(top_m=0, bottom_m=5, quantities=rows(),
                                 selections_si=SELECTIONS | change)


def test_reject_missing_or_incompatible_database_rows():
    for replacement in [dict(su=quantity("wrong unit", "kPa", 20)),
                        dict(su=quantity("other clay", "Pa", 20000, soil_unit="other")),
                        dict(su=quantity("unavailable", "Pa"))]:
        with pytest.raises(ValueError):
            clay_layer_from_database(top_m=0, bottom_m=5, quantities=rows() | replacement,
                                     selections_si=SELECTIONS)
    with pytest.raises(ValueError):
        clay_layer_from_database(top_m=0, bottom_m=5, quantities=rows(), selections_si={})


def profile():
    return (ClayLayer(0, 2, SoftClay(20, 6, 0.02, 0.5), ("upper",)),
            ClayLayer(2, 5, SoftClay(40, 8, 0.02, 0.5), ("lower",)))


def family(**kwargs):
    values = dict(layers=profile(), depths_m=(0, 1, 2, 4, 5), diameter_m=1,
                  displacements_m=(0, 0.05, 0.4), cyclic=False)
    return assemble_py_family(**(values | kwargs))


def test_profile_integrated_stress_and_boundary_assignment():
    """sigma'(2)=6*2=12; sigma'(4)=12+8*2=28 kPa.

    The deeper layer owns z=2: pu=(3*40+12)+.5*40*2=172.
    At z=4: pu=(3*40+28)+.5*40*4=228. At y50 p=pu/2.
    """
    result = family()
    assert result.layer_indices == (0, 0, 1, 1, 1)
    assert result.curves[2].resistances[1] == 86
    assert result.curves[3].resistances[1] == 114
    assert result.source_row_ids[2] == ("lower",)
    assert result.resistance_arrays[3] == result.curves[3].resistances


@pytest.mark.parametrize("depths", [(), (-1, 1), (1, 1), (2, 1), (0, 6), (math.nan,)])
def test_reject_invalid_family_depths(depths):
    with pytest.raises(ValueError):
        family(depths_m=depths)


def test_reject_profile_gaps_overlap_and_missing_mudline():
    for layers in [(), (ClayLayer(1, 5, SoftClay(20, 6, 0.02, 0.5)),),
                   (profile()[0], ClayLayer(3, 5, profile()[1].soil)),
                   (profile()[0], ClayLayer(1, 5, profile()[1].soil))]:
        with pytest.raises(ValueError):
            family(layers=layers)


def linear_case(nodes, **kwargs):
    depths = tuple(12 * i / (nodes - 1) for i in range(nodes))
    spring = Curve((0, 1), (0, 40000), "kN/m", True)
    values = dict(depths_m=depths, ei_kn_m2=10000, curves=(spring,) * nodes,
                  head_shear_kn=100, head_moment_kn_m=0,
                  displacement_tolerance_m=1e-9, force_tolerance_kn=1e-5,
                  max_iterations=100, relaxation=1)
    return solve_lateral_pile(**(values | kwargs))


def test_linear_winkler_published_solution_and_mesh_convergence():
    """Equation 19a: beta=(40000/(4*10000))**.25=1; y0=2*100/40000=.005 m.

    Length 12/beta makes the reflected tip effect negligible (exp(-24)).
    The 2% numerical error budget is a declared test target, not a code limit.
    Natural free-end conditions are approximated and checked by refinement.
    """
    solutions = [linear_case(n) for n in (121, 241, 481)]
    errors = [abs(s.displacements_m[0] - 0.005) for s in solutions]
    assert errors[2] < errors[1] < errors[0]
    assert solutions[-1].displacements_m[0] == pytest.approx(0.005, rel=0.02)
    result = solutions[-1]
    assert result.residual_kn < 1e-5
    assert abs(result.force_balance_kn) < 1e-5
    assert abs(result.moment_balance_kn_m) < 1e-5
    # Published deflection line: y=.005 exp(-z) cos(z).
    assert result.displacements_m[40] == pytest.approx(
        0.005 * math.exp(-1) * math.cos(1), abs=0.0001
    )


def test_lateral_load_sign_and_moment_equilibrium():
    positive = linear_case(121)
    negative = linear_case(121, head_shear_kn=-100)
    assert negative.displacements_m == pytest.approx([-y for y in positive.displacements_m])
    moment = linear_case(121, head_shear_kn=0, head_moment_kn_m=10)
    assert abs(moment.force_balance_kn) < 1e-5
    assert abs(moment.moment_balance_kn_m) < 1e-5
    unloaded = linear_case(121, head_shear_kn=0)
    assert unloaded.displacements_m == (0,) * 121


def test_nonlinear_softening_increases_head_deflection():
    """Assumed 24 m pile: one cyclic backbone compared with explicitly degraded clay."""
    depths = tuple(i / 5 for i in range(121))
    grid = (0,) + tuple(0.00001 * 1.15**i for i in range(100))
    layers = (ClayLayer(0, 24, SoftClay(20, 6, 0.02, 0.5)),)
    solutions = []
    for state in [CyclicDegradation(1, 0.1, 0.15), CyclicDegradation(100, 0.1, 0.15)]:
        curves = assemble_py_family(layers=layers, depths_m=depths, diameter_m=1,
                                    displacements_m=grid, cyclic=True, degradation=state)
        solutions.append(solve_lateral_pile(
            depths_m=depths, ei_kn_m2=1500000, curves=curves.curves,
            head_shear_kn=100, displacement_tolerance_m=1e-8,
            force_tolerance_kn=1e-5, max_iterations=1000, relaxation=0.5,
        ))
    assert solutions[1].displacements_m[0] > solutions[0].displacements_m[0] > 0
    assert all(s.residual_kn < 1e-5 for s in solutions)


@pytest.mark.parametrize("kwargs", [dict(ei_kn_m2=0), dict(ei_kn_m2=math.nan),
                                     dict(relaxation=0), dict(relaxation=1.1),
                                     dict(max_iterations=0), dict(max_iterations=1.5),
                                     dict(force_tolerance_kn=0),
                                     dict(displacement_tolerance_m=-1),
                                     dict(head_shear_kn=math.inf),
                                     dict(curves=()), dict(depths_m=(0, 1, 3, 4, 5))])
def test_reject_invalid_solver_inputs(kwargs):
    with pytest.raises(ValueError):
        linear_case(121, **kwargs)


def test_solver_reports_nonconvergence_and_unsupported_foundation():
    with pytest.raises(RuntimeError, match="converge"):
        linear_case(121, max_iterations=1, relaxation=0.1)
    spring = Curve((0, 1), (0, 0), "kN/m", True)
    with pytest.raises(ValueError, match="support"):
        linear_case(121, curves=(spring,) * 121)
