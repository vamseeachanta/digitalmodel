"""Structural inertia regressions; no solver binding or licensed data is used.

Source: OpenStax, University Physics Volume 1 (2016), section 10.5,
Eq. 10.19 (I = integral r^2 dm), uniform rod derivation, and Eq. 10.20.
https://openstax.org/books/university-physics-volume-1/pages/10-5-calculating-moments-of-inertia

Synthetic hand calculation: a homogeneous 1200 kg box, x/y/z lengths 6/4/2 m.
Integrating y^2 + z^2 over the centred box gives
Ix = 1200*(4^2 + 2^2)/12 = 2000 kg.m^2;
Iy = 1200*(2^2 + 6^2)/12 = 4000 kg.m^2;
Iz = 1200*(6^2 + 4^2)/12 = 5200 kg.m^2.
The 1/12 comes from integral(-a/2,a/2) u^2 du / a = a^2/12.
The chosen offsets are synthetic coordinates, not measured engineering data.
"""

from copy import deepcopy

import pytest

from digitalmodel.infrastructure.base_solvers.hydrodynamics.code_dnvrph103_hydrodynamics_circular import (
    DNVRPH103_hydrodynamics_circular,
)
from digitalmodel.infrastructure.base_solvers.hydrodynamics.code_dnvrph103_hydrodynamics_rectangular import (
    DNVRPH103_hydrodynamics_rectangular,
)


def make_solver(cog):
    """Use the synthetic box defined in the module hand calculation."""
    solver = DNVRPH103_hydrodynamics_rectangular()
    solver.cfg = {"inputs": {
        "m_air": 1200, "l": 6, "w": 4, "h": 2,
        "cog": dict(zip(("x", "y", "z"), cog)),
    }}
    return solver


@pytest.mark.parametrize("cog", [(0, 0, 0), (1, 2, 3), (-5, 7, -11)])
def test_box_inertia_is_about_centre_of_mass(cog):
    """All origins give (2000,4000,5200), per the module hand calculation."""
    solver = make_solver(cog)
    assert solver.get_mass_moment_of_inertia() == {
        "Ix": 2000, "Iy": 4000, "Iz": 5200,
    }


@pytest.mark.parametrize("cog", [(0, 0, 0), (1, 2, 3), (-5, 7, -11)])
def test_export_preserves_centre_of_mass_and_converts_to_tonne_metres_squared(cog):
    """Orcina Common data: inertia at CoM; CoM is relative to the origin.

    https://www.orcina.com/webhelp/OrcaFlex/Content/html/6Dbuoys,Commondata.htm
    Synthetic hand calculation above gives 2000/4000/5200 kg.m^2.
    In the workflow's tonne/metre units these are 2/4/5.2 t.m^2.
    """
    solver = make_solver(cog)
    # Synthetic displacement and drag placeholders exercise the real exporter;
    # no hydrodynamic coefficient is being calculated or qualified here.
    solver.cfg.update({"rho_water": 1000})
    solver.cfg["inputs"].update({"m_water": 0, "cov": {"x": 0, "y": 0, "z": 0}})
    xyz = {"x": 0, "y": 0, "z": 0}
    properties = {"I": solver.get_mass_moment_of_inertia(), "translational": {
        "area_drag": xyz, "cd": {"deep": xyz}, "ca": xyz,
    }}
    buoy = {}
    solver.update_6d_buoy_translational(properties, "deep", buoy)
    solver.update_6d_buoy_inertia(properties, buoy)
    assert buoy["Mass"] == 1.2
    assert buoy["MomentsOfInertia"] == [2, 4, 5.2]
    assert buoy["CentreOfMass"] == list(cog)


@pytest.mark.parametrize("cog", [(0, 0, 0), (-5, 7, -11)])
def test_supplied_principal_inertia_overrides_box_estimate(cog):
    """Independent six-point distribution, OpenStax (2016), Eq. 10.19.

    Six 200 kg point masses at (+/-2,0,0), (0,+/-3,0), (0,0,+/-4) m
    total 1200 kg and have centre of mass at zero. Summing m*r^2 gives
    Ix=400*(9+16)=10000, Iy=400*(4+16)=8000, Iz=400*(4+9)=5200 kg.m^2.
    Translation of the coordinate origin leaves these intrinsic values fixed.
    NIST SP 811 (2008), section 5.1.1, Table 6 gives the tonne conversion.
    """
    solver = make_solver(cog)
    supplied = {"Ix": 10000, "Iy": 8000, "Iz": 5200}
    solver.cfg["inputs"]["moments_of_inertia_at_cog"] = supplied
    inertia = solver.get_mass_moment_of_inertia()
    assert inertia == supplied
    # Mutating a returned calculation must not rewrite the input distribution.
    assert inertia is not supplied
    buoy = {}
    solver.update_6d_buoy_inertia({"I": inertia}, buoy)
    assert buoy["MomentsOfInertia"] == [10, 8, 5.2]


@pytest.mark.parametrize("supplied", [
    None, [], {}, {"Ix": 10000, "Iy": 8000},
    {"Ix": 10000, "Iy": 8000, "Iz": 5200, "Ixy": 0},
    {"Ix": "10000", "Iy": 8000, "Iz": 5200},
    {"Ix": True, "Iy": 8000, "Iz": 5200},
    {"Ix": float("nan"), "Iy": 8000, "Iz": 5200},
    {"Ix": float("inf"), "Iy": 8000, "Iz": 5200},
    {"Ix": -10000, "Iy": 8000, "Iz": 5200},
    {"Ix": 0, "Iy": 8000, "Iz": 5200},
    {"Ix": 20000, "Iy": 8000, "Iz": 5200},
])
def test_invalid_supplied_inertia_is_rejected_without_fallback(supplied):
    """Reject malformed input and impossible diagonal moments.

    OpenStax (2016), Eq. 10.19: Iy+Iz-Ix = 2*integral x^2 dm >= 0,
    with cyclic permutations. Thus 20000 > 8000+5200 is impossible.
    Positive finite diagonals are required for this three-dimensional input.
    Other values are deliberately invalid synthetic inputs, not coefficients.
    """
    solver = make_solver((0, 0, 0))
    solver.cfg["inputs"]["moments_of_inertia_at_cog"] = supplied
    with pytest.raises(ValueError, match="moments_of_inertia_at_cog"):
        solver.get_mass_moment_of_inertia()


def test_supplied_planar_distribution_at_triangle_boundary():
    """OpenStax (2016), Eq. 10.19, synthetic point masses in the yz plane.

    Two 0.05 kg masses at y=+/-1 m and two 0.15 kg masses at z=+/-1 m
    give Ix=0.1+0.3=0.4, Iy=0.3, Iz=0.1 kg.m^2. Thus Ix=Iy+Iz.
    Positive moments of a planar distribution satisfy the triangle boundary;
    subtracting floating-point 0.3 from 0.4 must not falsely reject it.
    """
    solver = make_solver((0, 0, 0))
    supplied = {"Ix": 0.4, "Iy": 0.3, "Iz": 0.1}
    solver.cfg["inputs"].update({"m_air": 0.4, "moments_of_inertia_at_cog": supplied})
    assert solver.get_mass_moment_of_inertia() == supplied


@pytest.mark.parametrize("scale", [1e-200, 1, 1e200])
@pytest.mark.parametrize("axes", [("Ix", "Iy", "Iz"), ("Iy", "Iz", "Ix"), ("Iz", "Ix", "Iy")])
def test_supplied_planar_boundary_tolerates_binary_rounding(scale, axes):
    """Two 0.05 kg masses at y=+/-1 and two 0.35 kg at z=+/-1 m.

    Summing m*r^2 gives (0.8, 0.7, 0.1) kg.m^2, with Ix=Iy+Iz.
    Scaling all masses and permuting axes preserves physical validity.
    """
    solver = make_solver((0, 0, 0))
    supplied = dict(zip(axes, [value * scale for value in (0.8, 0.7, 0.1)]))
    solver.cfg["inputs"]["moments_of_inertia_at_cog"] = supplied
    assert solver.get_mass_moment_of_inertia() == supplied


@pytest.mark.parametrize("scale", [1e-200, 1, 1e200])
def test_supplied_inertia_rejects_triangle_excess_beyond_roundoff(scale):
    """A relative excess of 1e-10 is larger than binary rounding error."""
    solver = make_solver((0, 0, 0))
    solver.cfg["inputs"]["moments_of_inertia_at_cog"] = {
        "Ix": 0.8 * (1 + 1e-10) * scale, "Iy": 0.7 * scale, "Iz": 0.1 * scale,
    }
    with pytest.raises(ValueError, match="triangle inequalities"):
        solver.get_mass_moment_of_inertia()


def test_supplied_finite_moments_with_overflowing_sum():
    solver = make_solver((0, 0, 0))
    supplied = {"Ix": 1e308, "Iy": 1e308, "Iz": 1e308}
    solver.cfg["inputs"]["moments_of_inertia_at_cog"] = supplied
    assert solver.get_mass_moment_of_inertia() == supplied


def test_displaced_box_estimate_warns_about_mass_distribution(caplog):
    solver = make_solver((1, 2, 3))
    solver.get_mass_moment_of_inertia()
    assert "homogeneous-box estimate" in caplog.text
    assert "moments_of_inertia_at_cog" in caplog.text


@pytest.mark.parametrize("supplied", [False, True])
def test_centred_box_or_supplied_inertia_does_not_warn(caplog, supplied):
    solver = make_solver((1, 2, 3) if supplied else (0, 0, 0))
    if supplied:
        solver.cfg["inputs"]["moments_of_inertia_at_cog"] = {
            "Ix": 10000, "Iy": 8000, "Iz": 5200,
        }
    solver.get_mass_moment_of_inertia()
    assert not caplog.records


@pytest.mark.parametrize("cog", [(0, 0, 0), (1, 2, 3), (-5, 7, -11)])
def test_circular_inertia_and_export_are_about_centre_of_mass(cog):
    """Uniform cylinder: mass 1200 kg, radius 2 m, height 6 m.

    Integrating y^2+z^2 gives m*(R^2/4+h^2/12)=4800 kg.m^2;
    integrating x^2+y^2 gives m*R^2/2=2400 kg.m^2. The existing
    circular structural estimate uses r/2 for R, so its input r is 4 m.
    """
    solver = DNVRPH103_hydrodynamics_circular()
    solver.cfg = {"inputs": {
        "m_air": 1200, "r": 4, "h": 6,
        "cog": dict(zip(("x", "y", "z"), cog)),
    }}
    inertia = solver.get_mass_moment_of_inertia()
    assert inertia == {"Ix": 4800, "Iy": 4800, "Iz": 2400}
    buoy = {}
    solver.update_6d_buoy_inertia({"I": inertia}, buoy)
    assert buoy["MomentsOfInertia"] == [4.8, 4.8, 2.4]


@pytest.fixture(params=["circular", "rectangular", "shared"])
def buoy_exporters(request):
    """Exercise the real methods and the dependency-free shared helpers."""
    if request.param == "shared":
        from digitalmodel.infrastructure.base_solvers.hydrodynamics.buoy_export import (
            update_mass_inertia_buoy_data,
            update_rotational_buoy_data,
        )
        return update_rotational_buoy_data, update_mass_inertia_buoy_data
    solver_class = {
        "circular": DNVRPH103_hydrodynamics_circular,
        "rectangular": DNVRPH103_hydrodynamics_rectangular,
    }[request.param]
    solver = solver_class()
    return solver.update_6d_buoy_rotational, solver.update_6d_buoy_inertia


@pytest.fixture
def export_properties():
    """Synthetic supplied values pin formatting without calculating coefficients."""
    return {
        "rotational": {
            "am": {
                "splash": {"x": 1.23456, "y": -2.34567, "z": 3.45678},
                "deep": {"x": 4.56789, "y": 5.67891, "z": -6.78912},
            },
            "cd": {"x": 7.89123, "y": -8.91234, "z": 9.12345},
            "I": {"x": 10.23456, "y": 11.34567, "z": -12.45678},
            "ca": {"x": 13, "y": -14.56789, "z": 15.67891},
        },
        "I": {"Ix": 1234.56, "Iy": 2345.67, "Iz": 3456.78},
        "unrelated": {"keep": [42]},
    }


@pytest.mark.parametrize("zone, area_moment", [
    ("splash", [1.235, -2.346, 3.457]),
    ("deep", [4.568, 5.679, -6.789]),
])
def test_rotational_export_contract(buoy_exporters, export_properties, zone, area_moment):
    """Retain axis order, rounding, float coefficients and unrelated entries."""
    properties_before = deepcopy(export_properties)
    buoy = {"sentinel": {"keep": [42]}, "DragAreaMoment": [99]}
    assert buoy_exporters[0](export_properties, zone, buoy) is None
    assert buoy == {
        "sentinel": {"keep": [42]},
        "DragAreaMoment": area_moment,
        "DragMomentCoefficient": [7.891, -8.912, 9.123],
        "HydrodynamicInertia": [10.235, 11.346, -12.457],
        "AddedInertiaCoefficient": [13.0, -14.568, 15.679],
    }
    assert list(buoy) == [
        "sentinel", "DragAreaMoment", "DragMomentCoefficient",
        "HydrodynamicInertia", "AddedInertiaCoefficient",
    ]
    assert all(type(value) is float for value in buoy["AddedInertiaCoefficient"])
    assert export_properties == properties_before


def test_mass_inertia_export_contract(buoy_exporters, export_properties):
    """Convert supplied kg.m^2 to t.m^2 and retain unrelated entries."""
    properties_before = deepcopy(export_properties)
    buoy = {"sentinel": {"keep": [42]}, "MomentsOfInertia": [99]}
    assert buoy_exporters[1](export_properties, buoy) is None
    assert buoy == {
        "sentinel": {"keep": [42]}, "MomentsOfInertia": [1.235, 2.346, 3.457],
    }
    assert export_properties == properties_before


def test_rotational_export_missing_key_retains_partial_updates(
    buoy_exporters, export_properties,
):
    """A missing coefficient still raises after the preceding three updates."""
    del export_properties["rotational"]["ca"]["y"]
    properties_before = deepcopy(export_properties)
    buoy = {"sentinel": 42, "AddedInertiaCoefficient": [99]}
    with pytest.raises(KeyError, match="y"):
        buoy_exporters[0](export_properties, "deep", buoy)
    assert buoy == {
        "sentinel": 42, "AddedInertiaCoefficient": [99],
        "DragAreaMoment": [4.568, 5.679, -6.789],
        "DragMomentCoefficient": [7.891, -8.912, 9.123],
        "HydrodynamicInertia": [10.235, 11.346, -12.457],
    }
    assert export_properties == properties_before


def test_mass_inertia_export_missing_key_does_not_update(
    buoy_exporters, export_properties,
):
    """An incomplete principal-inertia vector raises before mutating output."""
    del export_properties["I"]["Iy"]
    properties_before = deepcopy(export_properties)
    buoy = {"sentinel": 42, "MomentsOfInertia": [99]}
    with pytest.raises(KeyError, match="Iy"):
        buoy_exporters[1](export_properties, buoy)
    assert buoy == {"sentinel": 42, "MomentsOfInertia": [99]}
    assert export_properties == properties_before
