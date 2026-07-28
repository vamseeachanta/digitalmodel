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
