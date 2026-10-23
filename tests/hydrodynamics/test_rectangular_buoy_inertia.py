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

import pytest

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
