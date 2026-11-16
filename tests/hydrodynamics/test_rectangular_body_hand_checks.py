"""Independent hand checks of the existing rectangular-body calculator.

Every input is an illustrative example: lengths are in metres, masses in kg,
and density in kg/m^3. All lookup samples are invented analytic data, without
physical qualification. Approximate comparisons allow floating-point roundoff,
not engineering acceptance. No file generation or licensed solver is invoked.

The legacy calculator does not establish positive finite geometry, a valid
displacement, or physically applicable coefficients. These checks characterize
its existing arithmetic and lookup-domain validation; they do not
add or imply geometry validation.
"""

import math
from copy import deepcopy
from itertools import product

import pytest

from digitalmodel.infrastructure.base_solvers.hydrodynamics.code_dnvrph103_hydrodynamics_rectangular import (
    DNVRPH103_hydrodynamics_rectangular,
)


@pytest.fixture
def solver():
    """Illustrative example: 6 by 4 by 2 box, displaced volume 16 m^3.

    Invented tables: added mass 1+r/4, plate drag .3+.1r, prism drag
    1+.2r, rotational upper 2+r/4, and rotational lower 3+r/2.
    The displaced volume differs deliberately from the 48 m^3 envelope.
    """
    body = DNVRPH103_hydrodynamics_rectangular()
    body.cfg = {
        "rho_water": 1000,
        "inputs": {
            "l": 6, "w": 4, "h": 2, "m_air": 24000, "m_water": 8000,
            "cog": {"x": 0, "y": 0, "z": 0},
            "perforation_ratio": {"x": 0, "y": 0, "z": 0},
        },
        "look_up": {
            "plate": {
                "added_mass": {"ratio": [0.5, 4.5], "value": [1.125, 2.125]},
                "drag": {"ratio": [0.5, 4.5], "value": [0.35, 0.75]},
            },
            "prism": {
                "drag": {"ratio": [0.5, 5.5], "value": [1.1, 2.1]},
            },
            "rotational": {
                "ratio": [0, 0.5, 1, 1.5, 2, 2.5, 3],
                "m55_upper": [2, 2.125, 2.25, 2.375, 2.5, 2.625, 2.75],
                "m55_lower": [3, 3.25, 3.5, 3.75, 4, 4.25, 4.5],
            },
        },
    }
    return body


@pytest.mark.parametrize("axis,face", [
    ("x", (2, 4, 6)), ("y", (2, 6, 4)), ("z", (4, 6, 2)),
])
def test_axis_projection_and_displaced_volume(solver, axis, face):
    """Catch swapped axes or substitution of envelope for displaced volume."""
    dimensions = getattr(solver, f"get_{axis}_dimensions")()
    assert dimensions == dict(zip(("a", "b", "c", "volume"), (*face, 16)))


@pytest.mark.parametrize("axis,lam,prefactor,area,deep", [
    ("x", math.sqrt(2) / (3 + math.sqrt(2)), 6000 * math.pi,
     8, 1 + 0.2 * 3 / math.sqrt(2)),
    ("y", math.sqrt(3) / (2 + math.sqrt(3)), 10500 * math.pi,
     12, 1 + 0.2 * 2 / math.sqrt(3)),
    ("z", math.sqrt(6) / (1 + math.sqrt(6)), 33000 * math.pi, 24, 0.45),
])
def test_translational_results_match_hand_specializations(
    solver, axis, lam, prefactor, area, deep,
):
    """Catch interpolation, finite-length factor, axis and normalization errors.

    Plate ratios 2, 3, 1.5 give factors 1.5, 1.75, 1.375.
    Multiplication by pi*rho*a^2*b/4 gives the three listed prefactors.
    The specified mass difference is 16000 kg on every axis.
    """
    result = solver.get_translational_properties()
    mass = prefactor * (1 + math.sqrt((1 - lam**2) / (2 + 2 * lam**2)))
    assert result["added_mass"][axis] == pytest.approx(mass)
    assert result["ca"][axis] == pytest.approx(mass / 16000)
    assert result["area_drag"][axis] == area
    assert result["cd"]["deep"][axis] == pytest.approx(deep)
    assert result["cd"]["splash"][axis] == pytest.approx(max(3 * deep, 2.5))


@pytest.mark.parametrize("streamwise,expected", [
    (math.nextafter(4, 0), 0.7), (4, 1.2),
])
def test_drag_plate_to_prism_boundary(solver, streamwise, expected):
    """For a 2 by 8 face, c=4 gives c/sqrt(ab)=1: select prism."""
    area, drag = solver.get_translational_cd(
        {"a": 2, "b": 8, "c": streamwise, "volume": 16}, 0.25,
    )
    assert area == 12
    assert drag["deep"] == pytest.approx(expected)
    assert drag["splash"] == pytest.approx(max(3 * expected, 2.5))


@pytest.mark.parametrize("deep,splash", [
    (0, 2.5), (0.5, 2.5), (2.5 / 3, 2.5), (1, 3),
])
def test_splash_floor_and_multiplier(solver, deep, splash):
    """Catch a missing floor or an incorrect deep-to-splash multiplier."""
    assert solver.get_splash_from_deep_drag(deep) == pytest.approx(splash)


@pytest.mark.parametrize("porosity,factor", [
    (0, 1), (0.05, 1), (math.nextafter(0.05, 1), 1),
    (0.22, 0.7),
    (math.nextafter(0.34, 0), 0.7 + 0.3 * math.cos(29 * math.pi / 34)),
    (0.34, math.exp(-6 / 7)), (0.66, math.exp(-2)),
])
def test_added_mass_perforation_branches_and_boundaries(solver, porosity, factor):
    """At .22 the cosine argument is pi/2; at .34 the exponent is -6/7.

    The left limit at .34 belongs to the cosine branch; the boundary itself
    belongs to the exponential branch. Their distinct values are retained.
    """
    assert solver.get_mass_added_with_perforation(1000, porosity) == pytest.approx(
        1000 * factor,
    )


def test_perforated_direction_reduces_mass_and_area_separately(solver):
    """Catch omitted porosity forwarding and using area loss for mass loss."""
    solver.cfg["inputs"]["perforation_ratio"]["z"] = 0.66
    lam = math.sqrt(6) / (1 + math.sqrt(6))
    mass = 33000 * math.pi * (1 + math.sqrt((1 - lam**2) / (2 + 2 * lam**2)))
    added_mass, ca, area, drag = solver.get_translational_z()
    assert added_mass == pytest.approx(mass * math.exp(-2))
    assert ca == pytest.approx(mass * math.exp(-2) / 16000)
    assert area == pytest.approx(8.16)
    assert drag == pytest.approx({"deep": 0.45, "splash": 2.5})


def test_all_rotational_drag_area_moments(solver):
    """Catch wrong lever powers, axis pairing, divisor or selected zone.

    On each face, 2*integral(0,d/2) r^3 dr = d^4/32. For example,
    deep x is (2*6*2^4 + 3*6*4^4)/32 = 150 m^5.
    Distinct zone coefficients prevent an accidental zone substitution.
    """
    result = solver.get_area_moment_of_intertia({"cd": {
        "deep": {"x": 1, "y": 2, "z": 3},
        "splash": {"x": 4, "y": 5, "z": 6},
    }})
    assert result["deep"] == {"x": 150, "y": 488, "z": 178}
    assert result["splash"] == {"x": 303, "y": 980, "z": 469}
    assert solver.get_moment_coefficient() == {"x": 1, "y": 1, "z": 1}


@pytest.mark.parametrize("a,b,expected", [
    (2, 1, 2.125), (2, 3, 2.375),
    (5, 8, (3 + 5 / 16) * 1024 / 445),
    (1, 2, 3.25 * 16 / 5),
])
def test_rotational_affine_fit_and_branch_boundary(solver, a, b, expected):
    """Seven affine samples have an exact degree-one polynomial.

    The degree-six fit must reproduce it to roundoff. At b/a=1.6 the
    lower table is evaluated at a/b and multiplied by 2*b^3/[a*(a^2+b^2)].
    """
    actual = solver.get_newman_ca(a, b, b / a, a / b)
    assert actual == pytest.approx(expected, rel=1e-10, abs=1e-10)


@pytest.mark.parametrize("a,b,expected", [(2, 1, 4), (1, 2, 12.8)])
def test_rotational_constant_fit(solver, a, b, expected):
    """A constant table gives 4 at b/a=.5, or 4*16/5 at b/a=2."""
    solver.cfg["look_up"]["rotational"].update({
        "m55_upper": [4] * 7, "m55_lower": [4] * 7,
    })
    assert solver.get_newman_ca(a, b, b / a, a / b) == pytest.approx(expected)


def test_rotational_equivalent_geometry_on_all_axes(solver):
    """Catch swapped semiaxes and omitted lower-branch geometric scaling."""
    transverse = math.sqrt(24 / math.pi)
    expected = {
        "x": 2 + math.sqrt(8 / math.pi) / 12,
        "y": 2 + math.sqrt(12 / math.pi) / 8,
        "z": (3 + 1 / (2 * transverse)) * 2 * transverse**3 / (1 + transverse**2),
    }
    assert solver.get_added_inertia_coefficient() == pytest.approx(expected)


def test_mass_and_fluid_inertia_keep_their_distinct_units(solver):
    """Box integral gives 24000/12*(20,40,52) kg.m^2.

    Fluid inertia uses (24000-8000)/1000/12*(20,40,52) tonne.m^2,
    rather than the structural mass or the gross envelope volume.
    """
    properties = solver.get_properties(solver.cfg)
    assert properties["I"] == {"Ix": 40000, "Iy": 80000, "Iz": 104000}
    assert properties["rotational"]["I"] == pytest.approx({
        "x": 80 / 3, "y": 160 / 3, "z": 208 / 3,
    })


def test_shape_has_all_eight_origin_symmetric_corners(solver):
    """Catch missing vertices, duplicate corners, wrong half-edge or offset."""
    vertices = solver.get_shape_coordinates()
    expected = set(product((-3, 3), (-2, 2), (-1, 1)))
    assert len(vertices) == 8
    assert {tuple(vertex) for vertex in vertices} == expected
    assert tuple(map(sum, zip(*vertices))) == (0, 0, 0)


def test_swapping_length_and_width_swaps_translational_axes(solver):
    """An x/y permutation must preserve scalar laws and exchange x/y results."""
    original = solver.get_translational_properties()
    swapped = DNVRPH103_hydrodynamics_rectangular()
    swapped.cfg = deepcopy(solver.cfg)
    swapped.cfg["inputs"].update({"l": 4, "w": 6})
    result = swapped.get_translational_properties()
    for group in ("added_mass", "ca", "area_drag"):
        assert result[group] == pytest.approx({
            "x": original[group]["y"], "y": original[group]["x"],
            "z": original[group]["z"],
        })
    for zone in ("deep", "splash"):
        assert result["cd"][zone] == pytest.approx({
            "x": original["cd"][zone]["y"], "y": original["cd"][zone]["x"],
            "z": original["cd"][zone]["z"],
        })


@pytest.mark.parametrize("method,dimensions,plate_drag_ratio", [
    ("get_translational_ca", {"a": 1, "b": 6, "c": 1, "volume": 16}, None),
    ("get_translational_cd", {"a": 1, "b": 6, "c": 1, "volume": 16}, None),
    ("get_translational_cd", {"a": 1, "b": 1, "c": 6, "volume": 16}, None),
    ("get_translational_cd", {"a": 1, "b": 1, "c": 0.25, "volume": 16}, [2, 4.5]),
])
def test_out_of_table_queries_raise_instead_of_extrapolating(
    solver, method, dimensions, plate_drag_ratio,
):
    """Catch extrapolation beyond either supplied plate or prism domain.

    The last case narrows the plate domain so a positive physical face
    ratio is below its lower endpoint; geometry remains unchanged.
    """
    if plate_drag_ratio is not None:
        solver.cfg["look_up"]["plate"]["drag"]["ratio"] = plate_drag_ratio
    with pytest.raises(ValueError):
        getattr(solver, method)(dimensions, 0)
