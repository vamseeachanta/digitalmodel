"""Illustrative example regressions for existing rectangular-body methods.

All geometry and coefficient curves below are synthetic. Expected results use
hand formulas or analytic identities, rather than retained source results.
No model export or licensed solver is invoked.
"""

from copy import deepcopy
import math

import numpy as np
import pytest

from digitalmodel.infrastructure.base_solvers.hydrodynamics.code_dnvrph103_hydrodynamics_rectangular import (
    DNVRPH103_hydrodynamics_rectangular,
)
from digitalmodel.hydrodynamics.diffraction.diffraction_units import (
    density_t_m3_to_kg_m3,
    inertia_t_m2_to_kg_m2,
    kg_to_tonnes,
    tonnes_to_kg,
)


@pytest.fixture
def solver():
    """Illustrative example: 8 by 4 by 2 m, using synthetic linear curves."""
    instance = DNVRPH103_hydrodynamics_rectangular()
    ratios = np.linspace(0, 2, 9)
    instance.cfg = {
        "rho_water": 1000.0,
        "inputs": {
            "l": 8.0, "w": 4.0, "h": 2.0,
            "m_air": 2400.0, "m_water": 1400.0,
            "cog": {"x": 0.0, "y": 0.0, "z": 0.0},
            "perforation_ratio": {"x": 0.0, "y": 0.0, "z": 0.2},
        },
        "look_up": {
            "plate": {
                "added_mass": {"ratio": [1.0, 9.0], "value": [0.4, 0.8]},
                "drag": {"ratio": [1.0, 9.0], "value": [0.6, 0.9]},
            },
            "prism": {"drag": {"ratio": [0.25, 4.25], "value": [0.4, 1.2]}},
            "rotational": {
                "ratio": ratios.tolist(),
                "m55_upper": (2 + ratios).tolist(),
                "m55_lower": (4 - ratios).tolist(),
            },
        },
    }
    return instance


@pytest.mark.parametrize("axis,a,b,c", [("x", 2, 4, 8), ("y", 2, 8, 4), ("z", 4, 8, 2)])
def test_axis_dimensions_and_displacement(solver, axis, a, b, c):
    dimensions = getattr(solver, f"get_{axis}_dimensions")()
    assert dimensions == {"a": a, "b": b, "c": c, "volume": (2400 - 1400) / 1000}


@pytest.mark.parametrize("axis", ["x", "y", "z"])
def test_added_mass_from_independent_formula(solver, axis):
    a, b, c = {"x": (2, 4, 8), "y": (2, 8, 4), "z": (4, 8, 2)}[axis]
    # The illustrative curve is exactly C(r) = 0.4 + (r-1)/20.
    coefficient = 0.4 + (b / a - 1) / 20
    reference_volume_m3 = math.pi * a**2 * b / 4
    lam = math.sqrt(a * b) / (c + math.sqrt(a * b))
    correction = 1 + math.sqrt((1 - lam**2) / (2 + 2 * lam**2))
    reduction = 1 if axis != "z" else 0.7 + 0.3 * math.cos(math.pi * (0.2 - 0.05) / 0.34)
    expected_kg = 1000 * coefficient * reference_volume_m3 * correction * reduction
    added_kg, ca = getattr(solver, f"get_translational_{axis}_ca")()
    assert added_kg == pytest.approx(expected_kg, rel=1e-12)
    assert ca == pytest.approx(expected_kg / (2400 - 1400), rel=1e-12)


@pytest.mark.parametrize("fraction", [0.0, 0.05, 0.2, 0.34, 0.5])
def test_perforation_branches_in_fraction_units(solver, fraction):
    if fraction <= 0.05:
        factor = 1.0
    elif fraction < 0.34:
        factor = 0.7 + 0.3 * math.cos(math.pi * (100 * fraction - 5) / 34)
    else:
        factor = math.exp((10 - 100 * fraction) / 28)
    assert solver.get_mass_added_with_perforation(200.0, fraction) == pytest.approx(200 * factor)
    assert solver.get_mass_added_with_perforation(0.0, fraction) == 0.0


@pytest.mark.parametrize("c", [1.0, math.sqrt(8), 8.0])
def test_drag_uses_plate_or_prism_curve(solver, c):
    dimensions = {"a": 2.0, "b": 4.0, "c": c}
    aspect = c / math.sqrt(8)
    expected_deep = 0.6 + (2 - 1) * 3 / 80 if aspect < 1 else 0.4 + (aspect - 0.25) / 5
    area, coefficients = solver.get_translational_cd(dimensions, 0.2)
    assert area == pytest.approx(2 * 4 * (1 - 0.2))
    assert coefficients == pytest.approx({"deep": expected_deep, "splash": max(3 * expected_deep, 2.5)})


@pytest.mark.parametrize("deep", [0.2, 2.5 / 3, 1.4])
def test_splash_floor_and_multiplier(solver, deep):
    assert solver.get_splash_from_deep_drag(deep) == max(2.5, deep * 3)


def test_rotational_drag_moments(solver):
    properties = {"cd": {"deep": {"x": 0.4, "y": 0.6, "z": 0.8}}}
    actual = solver.get_area_moment_of_inertia_by_type(properties, "deep")
    # Integrating |lever|^3 over each rectangular face produces edge^4/32.
    expected = {
        "x": (0.6 * 8 * 2**4 + 0.8 * 8 * 4**4) / 32,
        "y": (0.4 * 4 * 2**4 + 0.8 * 4 * 8**4) / 32,
        "z": (0.4 * 2 * 4**4 + 0.6 * 2 * 8**4) / 32,
    }
    assert actual == pytest.approx(expected)
    assert solver.get_moment_coefficient() == {"x": 1, "y": 1, "z": 1}


@pytest.mark.parametrize("ratio", [0.5, 1.5, 1.6, 2.0])
def test_rotational_fit_branch_with_analytic_synthetic_curves(solver, ratio):
    # Illustrative samples lie exactly on upper(t)=2+t and lower(t)=4-t.
    a, b = 2.0, 2.0 * ratio
    expected = 2 + ratio if ratio < 1.6 else (4 - 1 / ratio) * 2 * ratio**3 / (1 + ratio**2)
    assert solver.get_newman_ca(a, b, ratio, 1 / ratio) == pytest.approx(expected, rel=1e-11)


@pytest.mark.parametrize("ratio", [0.65, 1.8])
def test_sixth_degree_rotational_curve_between_samples(solver, ratio):
    table = solver.cfg["look_up"]["rotational"]
    table["m55_upper"] = [2 + t**6 / 64 for t in table["ratio"]]
    table["m55_lower"] = [4 - t**6 / 64 for t in table["ratio"]]
    a, b = 2.0, 2.0 * ratio
    expected = (2 + ratio**6 / 64) if ratio < 1.6 else (
        (4 - (1 / ratio)**6 / 64) * 2 * ratio**3 / (1 + ratio**2)
    )
    assert solver.get_newman_ca(a, b, ratio, 1 / ratio) == pytest.approx(expected, rel=1e-11)


@pytest.mark.parametrize("axis,a,b", [
    ("x", 4, math.sqrt(8 / math.pi)),
    ("y", 2, math.sqrt(16 / math.pi)),
    ("z", 1, math.sqrt(32 / math.pi)),
])
def test_axis_rotational_coefficient_geometry(solver, axis, a, b):
    ratio = b / a
    expected = 2 + ratio if ratio < 1.6 else (4 - 1 / ratio) * 2 * ratio**3 / (1 + ratio**2)
    assert getattr(solver, f"get_ca_intertial_{axis}")(8, 4, 2) == pytest.approx(expected, rel=1e-11)


def test_hydro_inertia_retains_existing_tonne_output(solver):
    # Existing method returns t.m^2 although configuration masses are kg.
    displaced_tonnes = kg_to_tonnes(2400 - 1400)
    assert solver.get_hydrodynamic_intertia() == pytest.approx({
        "x": displaced_tonnes * (4**2 + 2**2) / 12,
        "y": displaced_tonnes * (8**2 + 2**2) / 12,
        "z": displaced_tonnes * (8**2 + 4**2) / 12,
    })


def test_intrinsic_box_inertia_is_independent_of_origin(solver):
    expected = {"Ix": 2400 * (4**2 + 2**2) / 12,
                "Iy": 2400 * (8**2 + 2**2) / 12,
                "Iz": 2400 * (8**2 + 4**2) / 12}
    assert solver.get_mass_moment_of_inertia() == expected
    solver.cfg["inputs"]["cog"] = {"x": 2, "y": -4, "z": 6}
    assert solver.get_mass_moment_of_inertia() == expected


@pytest.mark.parametrize("bad", [
    None,
    {},
    {"Ix": 4, "Iy": 6},
    {"Ix": True, "Iy": 6, "Iz": 8},
    {"Ix": float("nan"), "Iy": 6, "Iz": 8},
    {"Ix": float("inf"), "Iy": 6, "Iz": 8},
    {"Ix": -4, "Iy": 6, "Iz": 8},
    {"Ix": 0, "Iy": 6, "Iz": 8},
    {"Ix": 20, "Iy": 6, "Iz": 8},
])
def test_supplied_inertia_validation(solver, bad):
    solver.cfg["inputs"]["moments_of_inertia_at_cog"] = bad
    with pytest.raises(ValueError, match="moments_of_inertia_at_cog"):
        solver.get_mass_moment_of_inertia()


def test_supplied_principal_moments_are_returned_without_aliasing(solver):
    supplied = {"Ix": 4.0, "Iy": 6.0, "Iz": 8.0}
    solver.cfg["inputs"]["moments_of_inertia_at_cog"] = supplied
    result = solver.get_mass_moment_of_inertia()
    assert result == supplied
    assert result is not supplied


def test_lookup_extrapolation_is_rejected(solver):
    dimensions = {"a": 2, "b": 40, "c": 8, "volume": 1}
    with pytest.raises(ValueError):
        solver.get_translational_ca(dimensions, 0)


def test_complete_property_pipeline_preserves_input(solver):
    original = deepcopy(solver.cfg)
    result = solver.get_properties(solver.cfg)
    assert solver.cfg == original
    for axis in ("x", "y", "z"):
        mass, coefficient, area, drag = getattr(solver, f"get_translational_{axis}")()
        assert result["translational"]["added_mass"][axis] == mass
        assert result["translational"]["ca"][axis] == coefficient
        assert result["translational"]["area_drag"][axis] == area
        assert result["translational"]["cd"]["deep"][axis] == drag["deep"]
    assert len(result["shape_coordinates"]) == 8
    assert {tuple(point) for point in result["shape_coordinates"]} == {
        (x, y, z) for x in (-4, 4) for y in (-2, 2) for z in (-1, 1)
    }


def test_existing_si_boundary_converters():
    assert tonnes_to_kg(2.4) == 2400
    assert density_t_m3_to_kg_m3(1.0) == 1000
    assert inertia_t_m2_to_kg_m2(4.0) == 4000
    assert kg_to_tonnes(tonnes_to_kg(2.4)) == 2.4
