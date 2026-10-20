# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent force examples; no expected force comes from a solver fixture."""

import math
from pathlib import Path
import runpy

import pytest

from digitalmodel.marine_ops.marine_analysis.catenary.lazy_wave import (
    LazyWaveConfiguration,
    LazyWaveSolver,
)

# Numerical regression precision, not a design acceptance limit: issue #2044,
# Plan-lite tests 2 and 4. See lazy_wave_force_example.md, "Test basis".
REL_TOL = 1e-12


@pytest.fixture(params=[(300.0, 200.0, 400.0, 500.0),
                        (500.0, 800.0, 1200.0, 1300.0)],
                ids=["three-four-five", "five-twelve-thirteen"])
def triangle(request):
    """Hand calculations from NREL/TP-500-41958 (2007), section 2.5.

    Set VA=0 and EA->infinity in (2-43b): d=sqrt(a*a+S*S)-a.
    Synthetic triples (a,S,a+d)=(300,400,500) and (500,1200,1300)
    therefore give d=200 and 800 m. With q=atan(a/S), H=w*a,
    V=w*S by (2-41b), T=w*(a+d) by (2-44). All inputs here are
    constructed arithmetic examples, not measured or design values.
    """
    return request.param


@pytest.fixture(params=[10.0, 25.0], ids=["weight-ten", "weight-twenty-five"])
def hand_case(triangle, request):
    """Use synthetic weights and elevations; the force basis is above.

    The seabed is d+100 m below hang-off, with sag/hog elevations of
    100/200 m. Net buoyed weight -2*w tests unequal segment lengths.
    No density, gravity constant or engineering coefficient is assumed.
    """
    a, drop, length, tension_per_weight = triangle
    weight = request.param
    config = LazyWaveConfiguration(
        hangoff_angle=math.degrees(math.atan2(a, length)),
        hangoff_below_msl=0.0,
        vertical_distance=drop + 100.0,
        sag_bend_elevation=100.0,
        hog_bend_above_seabed=200.0,
        weight_without_buoyancy=weight,
        weight_with_buoyancy=-2.0 * weight,
    )
    return config, (weight * a, weight * length, weight * tension_per_weight)


def test_hand_calculated_hangoff_components(hand_case, triangle):
    """For w=10: H,V,T are 3000,4000,5000 or 5000,12000,13000 N.

    For w=25 multiply those forces by 25/10. The hand derivation is in
    triangle(), independently of any historical solver or retained length.
    """
    config, (horizontal, vertical, tension) = hand_case
    result = LazyWaveSolver().solve(config)
    assert result.hangoff_to_sag.arc_length == pytest.approx(triangle[2], rel=REL_TOL)
    assert result.horizontal_force == pytest.approx(horizontal, rel=REL_TOL)
    assert result.vertical_force == pytest.approx(vertical, rel=REL_TOL)
    assert result.total_tension == pytest.approx(tension, rel=REL_TOL)
    assert math.degrees(math.atan2(result.horizontal_force, result.vertical_force)) == (
        pytest.approx(config.hangoff_angle, rel=REL_TOL)
    )


def test_global_signed_weight_matches_hand_calculated_vertical_force(hand_case):
    """Sum all suspended weights with their signs, starting at touchdown.

    Equation (2-41b) of NREL/TP-500-41958 (2007) applied successively to
    each segment gives V_top=sum(w_i*S_i), since V_touchdown=0. The
    expected V remains the independent hand value, rather than the solver's
    hang-off length. Negative buoyed weights must retain their sign.
    """
    config, (_, vertical, _) = hand_case
    result = LazyWaveSolver().solve(config)
    net_weight = math.fsum(s.weight_per_length * s.arc_length for s in result.segments)
    assert net_weight == pytest.approx(vertical, rel=REL_TOL)
    assert result.vertical_force == pytest.approx(net_weight, rel=REL_TOL)


def test_dictionary_conversion_preserves_physical_force_components(hand_case):
    """Legacy structure exports H,V,T from the same independent force triangle."""
    config, (horizontal, vertical, tension) = hand_case
    solver = LazyWaveSolver()
    summary = solver.to_legacy_dict(solver.solve(config), config)["Summary"]
    assert summary["Fh"] == pytest.approx(horizontal, rel=REL_TOL)
    assert summary["Fv"] == pytest.approx(vertical, rel=REL_TOL)
    assert summary["T"] == pytest.approx(tension, rel=REL_TOL)


def test_runnable_example_reports_the_hand_calculated_answer(capsys):
    """The 3-4-5 example reports 3000,4000,5000 N, as derived above."""
    path = Path(__file__).parents[1] / "examples/domains/mooring/lazy_wave_example.py"
    example = runpy.run_path(str(path))
    config, result = example["solve_example"]()
    assert config.hangoff_vertical_span == 200.0  # Synthetic 300-100 m input.
    assert result.vertical_force == pytest.approx(4000.0, rel=REL_TOL)
    assert result.total_tension == pytest.approx(5000.0, rel=REL_TOL)
    example["main"]()
    output = capsys.readouterr().out
    assert "Horizontal force (Fh): 3000.0 N" in output
    assert "Vertical force (Fv): 4000.0 N" in output
    assert "Total tension (T): 5000.0 N" in output
    assert "Converted summary Fv: 4000.0 N" in output
    assert "Converted summary T: 5000.0 N" in output
