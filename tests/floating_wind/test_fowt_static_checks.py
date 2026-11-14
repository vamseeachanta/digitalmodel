# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Licence-free checks of the generated IEA 15 MW / VolturnUS-S fowt model.

Comparators: published values recorded in basis.yml (Allen et al. 2020,
NREL/TP-5000-76773, Tables 6 and 11), the repository's quasi-static analysis
of the same platform (#2473, docs/reports/fowt-volturnus-mooring-analysis
manifest), and two existing repository catenary solvers. Load cases and run
packages are tested in test_fowt_iea15_packages.py; the committed results,
mass basis and report statements in test_fowt_iea15_results.py.
"""
from __future__ import annotations

import copy
import json
import math
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.floating_wind.fowt_static_checks import (
    catenary_endpoint,
    model_from_spec,
    natural_periods,
    rigid_mass_matrix,
    run_checks,
    solve_catenary,
)
from digitalmodel.floating_wind.iea15_orcaflex_spec import build_spec, load_sources

ROOT = Path(__file__).resolve().parents[2]
MANIFEST = ROOT / "docs" / "reports" / "fowt-volturnus-mooring-analysis.manifest.json"


@pytest.fixture(scope="module")
def sources():
    return load_sources()


@pytest.fixture(scope="module")
def results(sources):
    basis, data = sources
    spec = build_spec(basis, data)
    turbine = data["turbine"]
    return run_checks(spec, data, basis["turbine"]["rated_rotor_thrust"]["value"],
                      [turbine["hub_overhang"]["value"], 0.0, turbine["hub_height"]["value"]],
                      turbine["shaft_tilt"]["value"])


@pytest.fixture(scope="module")
def previous():
    return json.loads(MANIFEST.read_text(encoding="utf-8"))["results"]


# ------------------------------------------------------------- catenary
def test_catenary_round_trip():
    x, z = catenary_endpoint(1.3e6, 2.0e6, 850.0, 5844.0, 3.27e9)
    h, v = solve_catenary(x, z, 850.0, 5844.0, 3.27e9)
    assert h == pytest.approx(1.3e6, rel=1e-8)
    assert v == pytest.approx(2.0e6, rel=1e-8)


def test_grounded_inextensible_limit_matches_repo_tool():
    from digitalmodel.marine_ops.fowt_quasi_static.mooring import grounded_horizontal_tension

    span, rise, length, w = 780.0, 186.0, 850.0, 5844.0
    h, _ = solve_catenary(span, rise, length, w, 1e15)
    assert h == pytest.approx(grounded_horizontal_tension(span, rise, length, w), rel=1e-6)


def _integrate(h: float, v: float, length: float, w: float, ea: float) -> np.ndarray:
    """Endpoint of an elastic catenary by direct integration from the anchor."""
    from scipy.integrate import solve_ivp

    def slope(s, _):
        tv = v - w * (length - s)
        t = math.hypot(h, tv)
        return [h / t * (1 + t / ea), tv / t * (1 + t / ea)]

    return solve_ivp(slope, [0, length], [0, 0], rtol=1e-11, atol=1e-9).y[:, -1]


def test_suspended_elastic_line_against_direct_integration():
    span, rise, length, w, ea = 550.0, 400.0, 690.0, 1000.0, 1e9
    h, v = solve_catenary(span, rise, length, w, ea, guess=(3e5, 6e5))
    assert v > w * length  # fully suspended
    assert _integrate(h, v, length, w, ea) == pytest.approx([span, rise], abs=1e-6)


def test_suspended_inextensible_line_matches_repo_catenary_solver():
    """Agreement in the inextensible limit.

    For EA = 1e9 N the repository CatenarySolver returns a horizontal tension
    6 % above the direct-integration value on this geometry; that difference
    is reported separately and is not a comparator here.
    """
    from digitalmodel.marine_ops.marine_analysis.catenary.solver import (
        CatenaryInput,
        CatenarySolver,
    )

    span, rise, length, w, ea = 550.0, 400.0, 690.0, 1000.0, 1e14
    h, v = solve_catenary(span, rise, length, w, ea, guess=(3e5, 6e5))
    other = CatenarySolver().solve(CatenaryInput(length=length, horizontal_span=span,
                                                 vertical_span=rise, weight_per_length=w,
                                                 ea_stiffness=ea))
    assert h == pytest.approx(other.horizontal_tension, rel=1e-5)
    assert v == pytest.approx(other.vertical_tension_fairlead, rel=1e-5)


def test_rigid_mass_matrix_symmetric(sources):
    model = model_from_spec(build_spec(*sources))
    mass = rigid_mass_matrix(model)
    assert np.allclose(mass, mass.T)
    assert mass[0, 4] == pytest.approx(model.mass * model.cog[2])


def test_flexible_representation_rejected_by_rigid_checks(sources):
    """A flexible hull holds the platform only; hull-only checks would drop tower and RNA."""
    spec = copy.deepcopy(build_spec(*sources))
    fowt = spec["fowt"]
    fowt["representation"] = "flexible"
    fowt["tower"] = {"base_elevation": 15.0, "sections": [
        {"length": 129.386, "outer_diameter": 8.0, "mass_per_length": 10.0,
         "bending_stiffness": 1.0e9, "axial_stiffness": 1.0e8, "torsional_stiffness": 1.0e9,
         "poisson_ratio": 0.3}]}  # synthetic fixture values, not reference-design data
    fowt["rna"] = {"mass": 945.77, "centre_of_mass_from_tower_top": [-7.32, 0.0, 4.6],
                   "inertia_about_centre_of_mass": [[4.0e5, 0, 0], [0, 2.7e5, 0], [0, 0, 2.9e5]]}
    with pytest.raises(NotImplementedError, match="flexible representation"):
        model_from_spec(spec)


# ---------------------------------------------------- still water (LC0)
def test_lc0_equilibrium_converged(results):
    assert np.max(np.abs(results["still_water"]["residual"])) < 1.0


def test_lc0_fairlead_tension_against_allen_table_6(results, sources):
    basis, _ = sources
    published = basis["mooring"]["published_pretension"]["value"]
    for line in results["still_water"]["lines"]:
        assert line["tension"] == pytest.approx(published, rel=0.02)


def test_lc0_net_buoyancy_carried_by_mooring(results):
    vertical = -results["still_water"]["mooring_load"][2]
    hydro_weight = results["net_buoyancy"]
    heave = results["still_water"]["position"][2]
    assert heave < 0  # the platform sinks until mooring weight balances buoyancy
    assert vertical > hydro_weight


# ------------------------------------------------------- natural periods
TOLERANCE = {"surge": 0.10, "heave": 0.05, "pitch": 0.05, "yaw": 0.10}
INDEX = {"surge": 0, "heave": 2, "pitch": 4, "yaw": 5}


@pytest.mark.parametrize("mode", ["surge", "heave", "pitch", "yaw"])
def test_natural_period_against_allen_table_11(results, sources, mode):
    basis, _ = sources
    published = basis["published"]["natural_periods"]["value"][INDEX[mode]]
    computed = results["natural"]["periods"][mode]
    assert computed == pytest.approx(published, rel=TOLERANCE[mode])


@pytest.mark.parametrize("mode", ["surge", "heave", "pitch", "yaw"])
def test_natural_period_against_repository_bem_analysis(results, previous, mode):
    computed = results["natural"]["periods"][mode]
    assert computed == pytest.approx(previous["periods"]["at_tower_top"][INDEX[mode]], rel=0.02)


def test_mooring_stiffness_against_repository_moorpy_model(results, previous):
    ours = np.diag(results["natural"]["mooring_stiffness"])
    theirs = np.diag(np.array(previous["mooring_stiffness"]))
    for i in (0, 1, 2, 5):
        assert ours[i] == pytest.approx(theirs[i], rel=0.02)
    for i in (3, 4):  # rotational terms depend on the trimmed equilibrium
        assert ours[i] == pytest.approx(theirs[i], rel=0.03)


# ------------------------------------------------------ rated thrust (LC4)
def test_surge_only_offset_against_repository_analysis(results, previous):
    """Rotations and draft held, horizontal thrust: the #2473 assumptions."""
    operating = next(c for c in previous["cases"] if c["sea"] == "operating"
                     and c["state"] == "intact" and c["heading"] == 0.0)
    assert results["loaded_surge_only"]["position"][0] == pytest.approx(
        operating["mean"][0], rel=0.05)


def test_free_offset_and_pitch_bounded(results):
    q = results["loaded_free"]["position"]
    assert np.max(np.abs(results["loaded_free"]["residual"])) < 1.0
    assert 15.0 < q[0] < 30.0  # inside the fowt_mooring atlas watch-circle axis
    assert 0.0 < math.degrees(q[4]) < 10.0
    upwind = results["loaded_free"]["lines"][0]
    assert upwind["utilisation"] < 1.0


# ------------------------------------- spec radiation used by the period check
def _spec_with_scaled_added_mass(sources, factor, entries=None):
    spec = copy.deepcopy(build_spec(*sources))
    radiation = spec["fowt"]["hull"]["radiation"]
    for k in range(len(radiation)) if entries is None else entries:
        radiation[k]["added_mass"] = [[factor * v for v in row]
                                      for row in radiation[k]["added_mass"]]
    return spec


@pytest.mark.parametrize("factor, entries", [
    (4.0, None),        # the review's probe: every spec added-mass matrix times four
    (1.01, [1]),        # one finite-frequency entry, 1 %
    (1.01, [0]),        # the infinite-frequency entry, 1 %
])
def test_natural_periods_reject_spec_radiation_not_matching_the_data(sources, factor, entries):
    """The periods use the data table, so the spec the generator emits must
    hold the same added mass; otherwise the check does not describe the
    generated model."""
    _, data = sources
    model = model_from_spec(_spec_with_scaled_added_mass(sources, factor, entries))
    with pytest.raises(ValueError, match="added mass differs"):
        natural_periods(model, data, np.zeros(6))


def test_natural_periods_reject_spec_missing_a_tabulated_period(sources):
    _, data = sources
    spec = copy.deepcopy(build_spec(*sources))
    del spec["fowt"]["hull"]["radiation"][5]
    with pytest.raises(ValueError, match="radiation periods"):
        natural_periods(model_from_spec(spec), data, np.zeros(6))


def test_run_checks_reject_perturbed_spec_radiation(sources):
    basis, data = sources
    turbine = data["turbine"]
    with pytest.raises(ValueError, match="added mass differs"):
        run_checks(_spec_with_scaled_added_mass(sources, 4.0), data,
                   basis["turbine"]["rated_rotor_thrust"]["value"],
                   [turbine["hub_overhang"]["value"], 0.0, turbine["hub_height"]["value"]],
                   turbine["shaft_tilt"]["value"])
