# ABOUTME: Parity and regression tests for the Everitt-Jennings downhole card solver.
# ABOUTME: Expected values come from the reference implementation, stored in the fixture.
"""Tests for :mod:`...dynacard.everitt_jennings`.

The `7699227` fixture is a real (anonymized) deviated well carrying the
reference implementation's own expected downhole-card extremes under
``Performance.Calculated_DownholeCard_*``. Those four numbers are the parity
target; the remaining tests guard the defect that motivated this solver.
"""

import json
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings import (
    EverittJenningsSolver,
    RodString,
    Survey,
    estimate_damping_coeff,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings import (
    units as U,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.solver import (
    _march,
    _normal_force_per_length,
)

FIXTURE = Path(__file__).parent / "testdata" / "7699227.json"

# The fixture leaves viscosity null, so damping is estimated from a default.
# Parity holds to ~2.5%; see the viscosity sweep noted on the tracking issue —
# the residual is insensitive to viscosity, so it is implementation error
# rather than an untuned input.
PARITY_TOLERANCE_PCT = 3.0
TUBING_ID_IN = 2.441


@pytest.fixture(scope="module")
def well():
    """Load the reference well: rod taper, survey, surface card, expectations."""
    data = json.loads(FIXTURE.read_text())
    equipment, card, params = (
        data["equipmentData"],
        data["cardData"],
        data["InputParameters"],
    )

    rods_raw = equipment["Rods"]
    diameters_m = np.array([r["Diameter"] for r in rods_raw]) * U.IN2M
    lengths_m = np.array([r["TotalLength"] for r in rods_raw]) * U.FT2M
    couplings_m = np.array([r["CouplingOD"] for r in rods_raw]) * U.IN2M
    moduli = np.array([r["ModulusOfElasticity"] for r in rods_raw]) * U.PSI2PA

    # Published rod weight per foot already includes couplings, so derive an
    # effective density that reproduces the true mass per unit length.
    areas = np.pi * (diameters_m / 2.0) ** 2
    mass_per_m = np.array([r["Weight"] for r in rods_raw]) * U.LB2KG / U.FT2M

    rods = RodString(
        diameters=diameters_m,
        lengths=lengths_m,
        densities=mass_per_m / areas,
        moduli=moduli,
        coupling_diameters=couplings_m,
    )

    survey_raw = data["surveyData"]
    survey = Survey(
        measured_depth=np.array([s["MD"] for s in survey_raw]) * U.FT2M,
        inclination=np.array([s["Inclination"] for s in survey_raw]) * U.DEG2RAD,
        azimuth=np.array([s["Azimuth"] for s in survey_raw]) * U.DEG2RAD,
    )

    return {
        "rods": rods,
        "survey": survey,
        "position": np.array(card["Position"], dtype=float) * U.IN2M,
        "load": np.array(card["Load"], dtype=float) * U.LB2N,
        "spm": params["StrokesPerMinute"],
        "pump_diameter": equipment["Pump"]["Diameter"] * U.IN2M,
        "fluid_density": params["FluidDensity"],  # already kg/m^3
        "casing_id": params["CasingID"] * U.IN2M,
        "expected": {
            key.replace("Calculated_DownholeCard_", ""): value
            for key, value in data["Performance"].items()
            if key.startswith("Calculated_DownholeCard_")
        },
    }


def solve(well, n_nodes=200, viscosity=0.01, tubing_id_in=TUBING_ID_IN):
    """Run the solver against the fixture well."""
    return EverittJenningsSolver(n_nodes=n_nodes, viscosity=viscosity).solve(
        position=well["position"],
        load=well["load"],
        rods=well["rods"],
        strokes_per_minute=well["spm"],
        pump_diameter=well["pump_diameter"],
        tubing_id=tubing_id_in * U.IN2M,
        fluid_density=well["fluid_density"],
        survey=well["survey"],
    )


@pytest.mark.parametrize(
    "quantity,accessor",
    [
        ("Position_min", lambda c: float(c.position.min())),
        ("Position_max", lambda c: float(c.position.max())),
        ("Load_min", lambda c: float(c.load.min())),
        ("Load_max", lambda c: float(c.load.max())),
    ],
)
def test_parity_with_reference(well, quantity, accessor):
    """Each downhole-card extreme matches the reference implementation."""
    card = solve(well)
    expected = well["expected"][quantity]
    actual = accessor(card)
    error_pct = 100.0 * abs(actual - expected) / abs(expected)
    assert error_pct < PARITY_TOLERANCE_PCT, (
        f"{quantity}: expected {expected:.4f}, got {actual:.4f} "
        f"({error_pct:.2f}% > {PARITY_TOLERANCE_PCT}% tolerance)"
    )


def test_downhole_stroke_is_shorter_than_surface(well):
    """Rod stretch means plunger travel is always less than surface stroke."""
    card = solve(well)
    surface_stroke = float(well["position"].max() - well["position"].min())
    assert card.stroke < surface_stroke, (
        f"downhole stroke {card.stroke:.4f} m must be below the surface "
        f"stroke {surface_stroke:.4f} m"
    )


def test_stroke_ratio_matches_reference(well):
    """The stroke reduction ratio is the sharpest single parity check."""
    card = solve(well)
    surface_stroke = float(well["position"].max() - well["position"].min())
    expected_ratio = (
        well["expected"]["Position_max"] - well["expected"]["Position_min"]
    ) / surface_stroke
    assert card.stroke / surface_stroke == pytest.approx(expected_ratio, rel=0.01)


def test_gravity_enabled_solver_keeps_vertical_weight_at_boundary(well):
    """Deviation gravity must retain the explicit vertical load datum."""
    card = solve(well)

    np.testing.assert_allclose(
        card.simulation.boundary,
        well["load"] - card.simulation.buoyant_weight,
    )


def test_vertical_gravity_matches_buoyant_boundary_datum():
    """Both gravity branches must use the same submerged rod-weight datum."""
    diameter_for_one_square_metre = np.sqrt(4.0 / np.pi)
    rods = RodString(
        diameters=np.array([diameter_for_one_square_metre]),
        lengths=np.array([10.0]),
        densities=np.array([1_000.0]),
        moduli=np.array([1.0e6]),
    )
    inputs = {
        "position": np.zeros(8),
        "load": np.full(8, 100_000.0),
        "rods": rods,
        "strokes_per_minute": 10.0,
        "pump_diameter": 0.5,
        "tubing_id": 2.0,
        "fluid_density": 500.0,
        "survey": Survey.vertical(10.0),
        "damping": np.zeros(2),
    }
    distributed = EverittJenningsSolver(
        n_nodes=101, include_gravity=True, smooth_window=0
    ).solve(**inputs)
    boundary = EverittJenningsSolver(
        n_nodes=101, include_gravity=False, smooth_window=0
    ).solve(**inputs)

    # One grid cell carries (1,000 - 500) kg/m * 9.80665 m/s2 * 0.1 m
    # = 490.3325 N. The two finite-difference boundary representations may
    # differ by that terminal cell, but never by the 49,033.25 N fluid uplift.
    np.testing.assert_allclose(distributed.load, boundary.load, atol=490.3325)
    assert distributed.simulation.cumulative_buoyant[0] == pytest.approx(
        distributed.simulation.buoyant_weight
    )
    assert distributed.simulation.cumulative_buoyant[-1] == pytest.approx(0.0)


def test_solution_is_node_converged(well):
    """Doubling the spatial resolution must not move the answer materially."""
    coarse = solve(well, n_nodes=100)
    fine = solve(well, n_nodes=400)
    assert fine.stroke == pytest.approx(coarse.stroke, rel=0.01)


def test_downhole_load_is_not_an_affine_rescale_of_surface(well):
    """Regression for #1857.

    The defect this solver replaces produced a downhole load that was exactly
    ``0.88 * surface - constant`` — correlation 1.0 to floating-point
    precision. An affine map preserves every rod-vibration harmonic, so such a
    card cannot diagnose anything. The load must be rebuilt from strain.
    """
    card = solve(well)
    # The surface card and downhole card share a sample count, so they can be
    # compared point-for-point.
    surface_load = well["load"]
    assert len(card.load) == len(surface_load)

    correlation = abs(np.corrcoef(surface_load, card.load)[0, 1])
    assert correlation < 0.99, (
        f"downhole load correlates with surface load at r={correlation:.6f}; "
        "the load is not being transformed"
    )

    slope, intercept = np.polyfit(surface_load, card.load, 1)
    residual = float(np.max(np.abs(card.load - (slope * surface_load + intercept))))
    assert residual > 1.0, (
        f"downhole load is an affine rescale of the surface load "
        f"(max residual {residual:.3e} N)"
    )


def test_damping_varies_with_stroke_direction(well):
    """Upstroke and downstroke damping must differ, or the card has no area."""
    up, down = estimate_damping_coeff(
        od_rod=0.875 * U.IN2M,
        od_connect=1.875 * U.IN2M,
        od_pump=1.75 * U.IN2M,
        id_well=TUBING_ID_IN * U.IN2M,
        mu=0.01,
        l_tap=290.0,
        ro_rod=7850.0,
    )
    assert up != down
    assert np.isfinite(up) and np.isfinite(down)


def test_damping_refuses_input_below_correlation_validity_floor(well):
    """A casing ID where a tubing ID belongs must raise, not return NaN.

    The coupling-drag correlation raises a difference to a fractional power;
    below the validity floor the base goes negative and the entire card
    silently becomes NaN.
    """
    with pytest.raises(ValueError, match="validity floor"):
        solve(well, tubing_id_in=well["casing_id"] / U.IN2M)


def test_rod_string_requires_at_least_one_section():
    """The rod taper is the one input that cannot be defaulted."""
    with pytest.raises(ValueError, match="at least one section"):
        RodString(diameters=np.array([]), lengths=np.array([]))


def test_rod_string_rejects_mismatched_section_arrays():
    """Diameters and lengths describe the same sections and must agree."""
    with pytest.raises(ValueError, match="same number of sections"):
        RodString(
            diameters=np.array([0.0222, 0.0190]),
            lengths=np.array([290.0]),
        )


def test_curvature_friction_uses_axial_force_per_unit_length():
    """The curvature term must be a force/length before PDE integration."""
    n_x, n_t = 3, 4
    displacement = np.zeros((n_x, n_t))
    displacement[1] = np.array([0.01, 0.01, 0.02, 0.01])
    shape = (n_x, n_t)
    elastic_modulus = np.full(shape, 1_000.0)
    area = np.ones(shape)
    zeros = np.zeros(shape)
    buoyant_mass_per_length = np.ones(shape)
    inclination_gradient = np.full(shape, 3.0)

    solution, _, _ = _march(
        displacement,
        n_x,
        n_t,
        1.0,
        0.5,
        0.2,
        zeros,
        buoyant_mass_per_length,
        np.array([10.0, 5.0, 0.0]),
        10.0,
        zeros,
        elastic_modulus,
        area,
        zeros,
        inclination_gradient,
        zeros,
        1.0,
    )

    # Reduced F = EA * du/dx = 1,000 N * 0.01 m / 0.5 m = 20 N.
    # Actual local F adds 1 kg/m * 10 m/s2 * 0.5 m below = 5 N.
    # N' = 25 N * 3 /m = 75 N/m.
    # du_f = mu * N' * dx^2 / EA = 0.2 * 75 * 0.5^2 / 1,000
    # = 0.00375 m.
    assert solution[2, 1] == pytest.approx(0.02 + 0.00375)


def test_inclination_normal_load_adds_gravity_and_curvature():
    """Gravity and curvature act in the same inclination-plane direction."""
    normal_load = _normal_force_per_length(
        1.0,
        10.0,
        np.pi / 2.0,
        3.0,
        0.0,
        5.0,
    )

    # w_b*sin(phi) + F*dphi/ds = 1*10*1 + 5*3 = 25 N/m.
    assert normal_load == pytest.approx(25.0)


def test_wraparound_curvature_friction_opposes_motion():
    """The periodic time boundary must use the same motion sign as its peers."""
    n_x, n_t = 3, 4
    displacement = np.zeros((n_x, n_t))
    displacement[1] = np.array([0.02, 0.01, 0.01, 0.02])
    shape = (n_x, n_t)
    elastic_modulus = np.full(shape, 1_000.0)
    area = np.ones(shape)
    zeros = np.zeros(shape)
    inclination_gradient = np.full(shape, 3.0)

    solution, _, _ = _march(
        displacement, n_x, n_t, 1.0, 0.5, 0.2, zeros, zeros,
        np.zeros(n_x), 0.0, zeros, elastic_modulus, area, zeros,
        inclination_gradient, zeros, 1.0,
    )

    # F = 1,000 N * 0.02 m / 0.5 m = 40 N; N' = 40 * 3 = 120 N/m.
    # |du_f| = 0.2 * 120 * 0.5^2 / 1,000 = 0.006 m. Since u decreases
    # from j=0 to j=1, friction subtracts that increment from the 0.04 m base.
    assert solution[2, 0] == pytest.approx(0.04 - 0.006)


class TestEffectiveRodDensity:
    """Rod density is derived from measured weight, not assumed bare steel.

    The solver marches at c = sqrt(E/rho), so this is not a cosmetic input.
    Assuming 490 lb/ft3 gave 14.01% median nRMSE against measured vendor
    downhole cards; deriving from catalogued weight gave 0.74% (dm#1897).
    """

    def test_derived_from_weight_matches_catalogue_sonic_velocity(self):
        """Independent check: the derived density must reproduce 16,300 ft/s.

        Rod catalogues state a sonic velocity our derivation never sees, so
        agreement is a genuine cross-check rather than a restatement. Bare
        steel at 490 lb/ft3 gives 16,982 ft/s -- 4.2% off, and the reason the
        old default was wrong.
        """
        from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.adapter import (
            _effective_density_lb_ft3,
        )
        from digitalmodel.marine_ops.artificial_lift.dynacard.models import RodSection

        # 0.75 in rod, catalogued coupling-inclusive weight 1.63 lb/ft
        section = RodSection(diameter=0.75, length=1000.0, weight_per_foot=1.63)
        rho = _effective_density_lb_ft3(section)
        assert rho == pytest.approx(531.3, abs=1.0)

        E_lbf_ft2 = 30.5e6 * 144.0
        c_ft_s = np.sqrt(E_lbf_ft2 * 32.174 / rho)
        assert c_ft_s == pytest.approx(16_300.0, rel=0.005)

    def test_falls_back_to_steel_without_a_measured_weight(self):
        """No weight supplied is the only case where assuming steel is honest."""
        from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.adapter import (
            _effective_density_lb_ft3,
        )
        from digitalmodel.marine_ops.artificial_lift.dynacard.models import RodSection

        assert _effective_density_lb_ft3(
            RodSection(diameter=0.75, length=1000.0)
        ) == pytest.approx(490.0)
