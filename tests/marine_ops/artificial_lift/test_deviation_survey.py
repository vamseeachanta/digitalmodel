# ABOUTME: Tests that the wellbore survey is ingested and actually drives the solver.
# ABOUTME: Expected values are derived from named physical inputs, never fitted to output.
"""Deviation must reach the solver, and must change the answer when it does.

Every gate in dm#1894 shares one shape: the deviation machinery exists, runs,
and returns plausible numbers while contributing exactly zero. A test that only
checks a survey object was built would pass against that defect, so the tests
here are written to fail if the survey is inert:

* the load-datum tests assert the gravity term is *exactly* zero for a vertical
  well, so switching gravity on cannot silently move a well that has no
  deviation to move;
* the liveness tests assert the downhole card moves by the along-hole weight
  deficit that the survey itself implies, computed here from rod density,
  fluid density and inclination -- never read back from the solver.
"""

import json
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.data_loader import (
    load_from_json_file,
    parse_legacy_json,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings import (
    units as U,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.adapter import (
    rod_string_from_context,
    survey_from_context,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.solver import (
    EverittJenningsSolver,
    RodString,
    Survey,
    _normal_force_per_length,
)

DATA_DIR = Path(__file__).with_name("test_data")

# cleansed_well_005 is the only deviated fixture: 31 stations, kicking off at
# 1,797 ft and building to 39.98 deg. cleansed_well_001 is a two-station
# vertical record; 003 and 004 carry no survey block at all.
DEVIATED_FIXTURE = DATA_DIR / "cleansed_well_005.json"
VERTICAL_FIXTURE = DATA_DIR / "cleansed_well_001.json"

GRAVITY_M_S2 = 9.80665


def _minimal_legacy_payload(survey_stations):
    """Smallest legacy record that parses, carrying the survey under test."""
    return {
        "CardDetails": {"Api14": "TEST"},
        "InputParameters": {"StrokesPerMinute": 6.0},
        "cardData": {"Position": [0.0, 1.0, 0.0, -1.0], "Load": [1.0] * 4},
        "equipmentData": {
            "Rods": [{"Diameter": 0.875, "TotalLength": 2000.0, "Count": 80}],
            "Pump": {"Diameter": 1.75, "Depth": 2000.0},
        },
        "surveyData": survey_stations,
    }


# ---------------------------------------------------------------------------
# Ingestion: the survey must survive the loader
# ---------------------------------------------------------------------------
def test_deviated_well_survey_is_ingested_with_every_station():
    """All 31 stations of the deviated fixture must reach the context."""
    context = load_from_json_file(DEVIATED_FIXTURE)

    assert len(context.survey.measured_depth) == 31


def test_ingested_survey_preserves_the_maximum_inclination():
    """The build angle must arrive intact, in degrees, as recorded."""
    context = load_from_json_file(DEVIATED_FIXTURE)

    assert max(context.survey.inclination) == 39.98


def test_vertical_two_station_survey_is_still_ingested():
    """A vertical record is a survey too; it must not be discarded as empty."""
    context = load_from_json_file(VERTICAL_FIXTURE)

    assert len(context.survey.measured_depth) == 2


def test_survey_reaches_the_solver_converted_to_radians():
    """The adapter owns the unit crossing, so the solver must see radians."""
    context = load_from_json_file(DEVIATED_FIXTURE)
    rods = rod_string_from_context(context)

    survey = survey_from_context(context, rods)

    assert float(np.max(survey.inclination)) == pytest.approx(
        39.98 * U.DEG2RAD, rel=1e-12
    )


def test_unusable_station_is_dropped_and_the_rest_survive():
    """A null inclination costs one station, not the whole trajectory.

    Abstaining on the entire survey because one station is incomplete would
    silently return a well to the vertical assumption, which is the failure
    this issue exists to remove.
    """
    payload = _minimal_legacy_payload(
        [
            {"MD": 0.0, "Inclination": 0.0, "Azimuth": 10.0},
            {"MD": 900.0, "Inclination": None, "Azimuth": 10.0},
            {"MD": 1800.0, "Inclination": 30.0, "Azimuth": 10.0},
        ]
    )

    context = parse_legacy_json(payload)

    assert len(context.survey.measured_depth) == 2


def test_loader_abstains_exactly_when_the_survey_is_unusable():
    """Absent and degenerate surveys abstain; a usable one must still load.

    Two stations at one depth divide by zero in the inclination gradient, and
    a missing block has nothing to place, so both must leave the context
    without a survey. Asserting the usable case in the same test keeps the
    abstention a real discrimination rather than the loader ignoring surveys
    wholesale -- which is precisely the behaviour on record before this fix.
    """
    stations = [
        {"MD": 0.0, "Inclination": 0.0, "Azimuth": 0.0},
        {"MD": 900.0, "Inclination": 15.0, "Azimuth": 0.0},
        {"MD": 900.0, "Inclination": 30.0, "Azimuth": 0.0},
    ]
    degenerate = parse_legacy_json(_minimal_legacy_payload(stations))
    absent = parse_legacy_json(_minimal_legacy_payload(None))

    distinct = [dict(station) for station in stations]
    distinct[2]["MD"] = 1800.0
    usable = parse_legacy_json(_minimal_legacy_payload(distinct))

    assert degenerate.survey is None
    assert absent.survey is None
    assert len(usable.survey.measured_depth) == 3


# ---------------------------------------------------------------------------
# The contact force must follow the soft-string formula the issue states
# ---------------------------------------------------------------------------
def test_contact_force_sums_gravity_and_curvature_components():
    """N' = sqrt((F dphi/ds + w_b sin phi)^2 + (F dpsi/ds sin phi)^2).

    Gravity pulls the rod onto the low side and curvature in a build presses
    it the same way, so the two add. Subtracting them cancels a real contact
    load and can drive the normal force to zero where it is largest.
    """
    buoyant_line_density = 10.0  # kg/m
    gravity = 10.0  # m/s^2
    inclination = np.pi / 6.0  # sin = 0.5 exactly
    axial_force = 1000.0  # N
    inclination_gradient = 0.03  # rad/m
    azimuth_gradient = 0.008  # rad/m

    normal_force = _normal_force_per_length(
        buoyant_line_density,
        gravity,
        inclination,
        inclination_gradient,
        azimuth_gradient,
        axial_force,
    )

    # gravity  = 10 * 10 * 0.5             = 50 N/m
    # curvature= 1000 * 0.03               = 30 N/m
    # azimuth  = 1000 * 0.008 * 0.5        =  4 N/m
    expected = np.sqrt((50.0 + 30.0) ** 2 + 4.0 ** 2)
    assert normal_force == pytest.approx(expected, rel=1e-12)


# ---------------------------------------------------------------------------
# The load datum must not move when gravity is switched on
# ---------------------------------------------------------------------------
def _uniform_string(length_m=1500.0):
    """Single-taper string, so the weight deficit has a closed form."""
    return RodString(
        diameters=np.array([0.0222]),
        lengths=np.array([length_m]),
        densities=np.array([7850.0]),
        moduli=np.array([2.05e11]),
    )


def _solve_at_inclination(inclination_deg, include_gravity, n_nodes=150):
    """Solve one synthetic card down a constant-inclination hole."""
    rods = _uniform_string()
    samples = np.linspace(0.0, 2.0 * np.pi, 200)
    survey = Survey(
        measured_depth=np.array([0.0, rods.total_length]),
        inclination=np.full(2, np.deg2rad(inclination_deg)),
        azimuth=np.zeros(2),
    )
    solver = EverittJenningsSolver(
        n_nodes=n_nodes,
        include_gravity=include_gravity,
        friction_coefficient=0.0,
        smooth_window=0,
    )
    return solver.solve(
        position=np.sin(samples),
        load=40000.0 + 6000.0 * np.sin(samples),
        rods=rods,
        strokes_per_minute=6.0,
        pump_diameter=0.0445,
        tubing_id=0.062,
        fluid_density=1000.0,
        survey=survey,
    )


def test_vertical_well_is_bit_for_bit_identical_with_gravity_on():
    """cos(0) = 1, so a vertical well has no along-hole deficit to apply.

    Any difference at all means the static rod weight is being counted in a
    different place rather than the same place, which is the datum defect that
    produced the factor-of-7 inversion in dm#1893.
    """
    with_gravity = _solve_at_inclination(0.0, include_gravity=True)
    without_gravity = _solve_at_inclination(0.0, include_gravity=False)

    assert np.array_equal(with_gravity.load, without_gravity.load)


def test_gravity_offset_equals_the_along_hole_weight_deficit():
    """Turning gravity on must add exactly W_b (1 - cos phi), no more.

    The boundary removes the full buoyant string weight, but only the
    along-hole component W_b cos phi is actually carried, so the solver has to
    put W_b (1 - cos phi) back. The 1% tolerance is the one-cell quadrature
    deficit at the pump: the distributed term is applied at interior nodes
    only, giving (n_x - 2) / (n_x - 1) = 148/149 of the closed-form value.
    """
    inclination_deg = 60.0
    with_gravity = _solve_at_inclination(inclination_deg, include_gravity=True)
    without_gravity = _solve_at_inclination(inclination_deg, include_gravity=False)

    buoyant_weight = with_gravity.simulation.buoyant_weight
    expected = buoyant_weight * (1.0 - np.cos(np.deg2rad(inclination_deg)))

    offset = float(np.mean(with_gravity.load) - np.mean(without_gravity.load))
    assert offset == pytest.approx(expected, rel=0.01)


def test_load_offset_ratio_tracks_inclination_exactly():
    """Vary the survey and the output must move by the predicted factor.

    This is the liveness check. The grid quadrature factor is common to both
    solves and cancels in the ratio, so the expected value is exact rather
    than tolerance-bounded: it depends on nothing but the two inclinations.
    """
    def offset(inclination_deg):
        on = _solve_at_inclination(inclination_deg, include_gravity=True)
        off = _solve_at_inclination(inclination_deg, include_gravity=False)
        return float(np.mean(on.load) - np.mean(off.load))

    measured_ratio = offset(60.0) / offset(30.0)

    expected_ratio = (1.0 - np.cos(np.deg2rad(60.0))) / (
        1.0 - np.cos(np.deg2rad(30.0))
    )
    assert measured_ratio == pytest.approx(expected_ratio, rel=1e-6)


def test_real_deviated_card_moves_by_its_own_survey_weight_deficit():
    """On the real deviated well, the survey must drive the card it produces.

    The expected shift is integral w_b(s) (1 - cos phi(s)) ds over the rod
    string, built here from rod density, fluid density and the recorded
    inclinations. Nothing in the oracle is read back from the solver, so a
    survey that is parsed and then ignored cannot pass this.
    """
    context = load_from_json_file(DEVIATED_FIXTURE)
    rods = rod_string_from_context(context)

    # Built straight from the fixture rather than through the loader. Reading
    # the trajectory back out of the code under test would make the oracle
    # collapse to 0 == 0 the moment the survey stopped being ingested, which
    # is exactly the defect these tests exist to catch.
    stations = json.loads(DEVIATED_FIXTURE.read_text())["surveyData"]
    real_survey = Survey(
        measured_depth=np.array([s["MD"] for s in stations]) * U.FT2M,
        inclination=np.array([s["Inclination"] for s in stations]) * U.DEG2RAD,
        azimuth=np.array([s["Azimuth"] for s in stations]) * U.DEG2RAD,
    )
    vertical_survey = Survey.vertical(rods.total_length)

    position = np.asarray(context.surface_card.position, dtype=float) * U.IN2M
    load = np.asarray(context.surface_card.load, dtype=float) * U.LB2N
    fluid_density_si = context.fluid_density * U.LBPFT32KGPM3

    def solve(survey):
        solver = EverittJenningsSolver(
            n_nodes=200,
            viscosity=0.01,
            friction_coefficient=0.0,
            include_gravity=True,
            smooth_window=0,
        )
        return solver.solve(
            position=position,
            load=load,
            rods=rods,
            strokes_per_minute=context.spm,
            pump_diameter=context.pump.diameter * U.IN2M,
            tubing_id=2.441 * U.IN2M,
            fluid_density=fluid_density_si,
            survey=survey,
        )

    deviated = solve(real_survey)
    vertical = solve(vertical_survey)
    simulation = deviated.simulation

    # Buoyant weight per unit length of each taper, from first principles.
    buoyant_line_weight = (
        (rods.densities - fluid_density_si) * rods.areas * GRAVITY_M_S2
    )
    cuts = np.zeros(len(rods.lengths) + 1)
    cuts[1:] = np.cumsum(rods.lengths)
    per_node = np.full(simulation.n_x, np.nan)
    for index in range(len(cuts) - 1):
        span = (cuts[index] <= simulation.x) & (simulation.x <= cuts[index + 1])
        per_node[span] = buoyant_line_weight[index]

    deficit = 1.0 - np.cos(real_survey.phi(simulation.x))
    # The distributed term is applied at interior nodes only.
    expected = float(np.sum(per_node[1:-1] * deficit[1:-1]) * simulation.dx)

    offset = float(np.mean(deviated.load) - np.mean(vertical.load))
    assert offset == pytest.approx(expected, rel=1e-3)
