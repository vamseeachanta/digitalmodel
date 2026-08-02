"""
Regression tests for the three SLWR geometry-kernel defects catalogued in issue #1949.

Each test class pins one defect. Every assertion here failed before the repair and
is written so that it would fail again if the behaviour regressed — none of them are
true by construction.

Defect 1 -- sign-blindness on buoyancy
    ``abs()`` was applied to ``weight_with_buoyancy`` throughout, so a configuration
    with *insufficient* buoyancy (positive net weight over the buoyed length) returned
    results bit-identical to a correctly buoyant one. Half of any buoyancy search space
    was physically invalid and scored as valid.

Defect 2 -- ``vertical_distance`` was dead input
    Sweeping it 100 m -> 5000 m changed nothing. The solver was an open-loop geometric
    construction with no water-depth closure, so it could return a configuration that
    never reached the seabed.

Defect 3 -- ``hangoff_bend_radius`` was an unbound independent input
    ``_solve_hangoff_section`` derived a bend radius from (depth, angle) while
    ``_solve_sag_hog_sections`` and the force balance used a separately supplied value.
    On the shipped fixture the two disagreed by 5.5x in arc length and 115x in
    horizontal force.

The reference cases in ``tests/fixtures/test_vectors/mooring_risers/
lazy_wave_reference_cases.yaml`` are de-identified parameter -> result pairs from a
historical result set produced by the ancestor of this solver. They pin the closure
identity and the bend-radius binding against real prior output rather than against
this implementation's own arithmetic.
"""

import math
from pathlib import Path

import pytest
import yaml

from digitalmodel.marine_ops.marine_analysis.catenary.lazy_wave import (
    LazyWaveConfigurationError,
    LazyWaveConfiguration,
    LazyWaveSolver,
    derive_hangoff_bend_radius,
)


REFERENCE_CASES_PATH = (
    Path(__file__).parent
    / "fixtures"
    / "test_vectors"
    / "mooring_risers"
    / "lazy_wave_reference_cases.yaml"
)


def _reference_cases():
    data = yaml.safe_load(REFERENCE_CASES_PATH.read_text())
    return data["cases"]


REFERENCE_CASES = _reference_cases()


@pytest.fixture
def solver():
    return LazyWaveSolver()


def make_config(**overrides):
    """A physically valid lazy-wave configuration, with overrides applied.

    The hang-off sits 50 m below MSL in 500 m of water measured from the hang-off
    down to the seabed; the sag bend is 150 m above the seabed, so the hang-off
    section spans 500 - 150 = 350 m vertically.
    """
    base = dict(
        hangoff_angle=15.0,
        hangoff_below_msl=50.0,
        hog_bend_above_seabed=300.0,
        sag_bend_elevation=150.0,
        weight_without_buoyancy=1000.0,
        weight_with_buoyancy=-500.0,
        vertical_distance=500.0,
    )
    base.update(overrides)
    return LazyWaveConfiguration(**base)


class TestDefect1SignBlindnessOnBuoyancy:
    """Insufficient buoyancy must not alias onto correct buoyancy."""

    def test_positive_net_weight_over_buoyed_length_is_rejected(self):
        """w_buoy > 0 means the 'buoyed' section is still net-heavy.

        No upward hog arc can form, so there is no lazy-wave configuration to
        report. Previously this returned a result identical to w_buoy = -500.
        """
        with pytest.raises(LazyWaveConfigurationError) as exc:
            make_config(weight_with_buoyancy=+500.0)
        assert "buoyan" in str(exc.value).lower()

    def test_zero_net_weight_over_buoyed_length_is_rejected(self):
        """w_buoy == 0 is neutrally buoyant: the hog bend radius diverges."""
        with pytest.raises(LazyWaveConfigurationError):
            make_config(weight_with_buoyancy=0.0)

    def test_sign_of_buoyancy_is_not_erased_by_magnitude(self, solver):
        """The +/- pair must not produce the same numbers.

        This is the direct regression on the ``abs()`` defect: previously
        w_buoy = +500 and w_buoy = -500 gave bit-identical arc length, horizontal
        distance and forces.
        """
        buoyant = solver.solve(make_config(weight_with_buoyancy=-500.0))
        with pytest.raises(LazyWaveConfigurationError):
            solver.solve(make_config(weight_with_buoyancy=+500.0))
        # And the valid branch still produces a usable answer.
        assert buoyant.total_arc_length > 0

    def test_non_positive_bare_riser_weight_is_rejected(self):
        """A bare riser that is not net-heavy cannot hang off in a catenary."""
        with pytest.raises(LazyWaveConfigurationError):
            make_config(weight_without_buoyancy=-1000.0)
        with pytest.raises(LazyWaveConfigurationError):
            make_config(weight_without_buoyancy=0.0)

    def test_buoyancy_magnitude_still_moves_the_objective(self, solver):
        """A buoyancy search needs a non-flat objective across the valid range."""
        arcs = [
            solver.solve(make_config(weight_with_buoyancy=wb)).total_arc_length
            for wb in (-400.0, -600.0, -800.0, -1200.0)
        ]
        assert len(set(arcs)) == len(arcs), f"objective is flat across buoyancy: {arcs}"


class TestDefect2WaterDepthClosure:
    """``vertical_distance`` must drive the solution and the geometry must close."""

    def test_vertical_distance_changes_the_solution(self, solver):
        """Previously a 100 m -> 5000 m sweep produced identical output."""
        results = {
            vd: solver.solve(make_config(vertical_distance=vd))
            for vd in (400.0, 500.0, 800.0, 2000.0)
        }
        arcs = [r.total_arc_length for r in results.values()]
        assert len(set(arcs)) == len(arcs), f"vertical_distance is dead input: {arcs}"

        # Deeper water must mean a longer riser.
        ordered = [results[vd].total_arc_length for vd in (400.0, 500.0, 800.0, 2000.0)]
        assert ordered == sorted(ordered)

    def test_hangoff_section_spans_hangoff_down_to_the_sag_bend(self, solver):
        """The hang-off section's vertical span is set by closure, not by MSL depth.

        d_hangoff = vertical_distance - sag_bend_elevation. Previously the solver
        used ``hangoff_below_msl`` here, which is a reporting datum, not a span.
        """
        config = make_config(vertical_distance=500.0, sag_bend_elevation=150.0)
        results = solver.solve(config)
        assert math.isclose(
            results.hangoff_to_sag.vertical_distance, 350.0, rel_tol=1e-12
        )

    def test_returned_geometry_reaches_the_seabed(self, solver):
        """Signed vertical spans must sum to exactly the water depth.

        Down: hang-off -> sag, hog -> buoyancy end, buoyancy end -> touchdown.
        Up:   sag -> buoyancy start, buoyancy start -> hog.
        """
        for vd in (400.0, 500.0, 1200.0, 3000.0):
            results = solver.solve(make_config(vertical_distance=vd))
            net_descent = (
                results.hangoff_to_sag.vertical_distance
                - results.sag_to_buoyancy.vertical_distance
                - results.buoyancy_to_hog.vertical_distance
                + results.hog_to_buoyancy_end.vertical_distance
                + results.buoyancy_to_touchdown.vertical_distance
            )
            assert math.isclose(net_descent, vd, abs_tol=1e-9), (
                f"geometry does not reach the seabed for vertical_distance={vd}: "
                f"net descent {net_descent}"
            )

    def test_closure_error_is_reported_and_is_zero(self, solver):
        results = solver.solve(make_config())
        assert hasattr(results, "vertical_closure_error")
        assert abs(results.vertical_closure_error) < 1e-9
        assert abs(results.summary["VerticalClosureError"]) < 1e-9

    def test_configuration_that_cannot_close_is_rejected(self):
        """The sag bend cannot sit at or above the hang-off point."""
        with pytest.raises(LazyWaveConfigurationError):
            make_config(vertical_distance=150.0, sag_bend_elevation=150.0)
        with pytest.raises(LazyWaveConfigurationError):
            make_config(vertical_distance=100.0, sag_bend_elevation=150.0)

    def test_hog_bend_must_sit_above_the_sag_bend(self):
        with pytest.raises(LazyWaveConfigurationError):
            make_config(hog_bend_above_seabed=150.0, sag_bend_elevation=150.0)
        with pytest.raises(LazyWaveConfigurationError):
            make_config(hog_bend_above_seabed=140.0, sag_bend_elevation=150.0)

    def test_sag_bend_must_sit_above_the_seabed(self):
        with pytest.raises(LazyWaveConfigurationError):
            make_config(sag_bend_elevation=0.0)
        with pytest.raises(LazyWaveConfigurationError):
            make_config(sag_bend_elevation=-10.0)


class TestDefect3HangoffBendRadiusBinding:
    """The hang-off bend radius is derived from (depth, angle), never free."""

    def test_bend_radius_is_derived_when_not_supplied(self, solver):
        config = make_config()
        expected = derive_hangoff_bend_radius(
            vertical_span=500.0 - 150.0, hangoff_angle=15.0
        )
        assert math.isclose(config.hangoff_bend_radius, expected, rel_tol=1e-12)
        assert math.isclose(
            solver.solve(config).hangoff_to_sag.bend_radius, expected, rel_tol=1e-12
        )

    def test_derivation_matches_the_closed_form(self):
        d, q = 2800.0, 8.0
        angle = math.radians(90.0 - q)
        expected = d * math.cos(angle) / (1.0 - math.cos(angle))
        assert math.isclose(
            derive_hangoff_bend_radius(vertical_span=d, hangoff_angle=q),
            expected,
            rel_tol=1e-12,
        )

    def test_inconsistent_supplied_bend_radius_is_rejected(self):
        """2000 m against a derived 56.5 m must not pass silently.

        This is the exact inconsistency the shipped example, unit-test fixture and
        benchmark entry all encoded.
        """
        with pytest.raises(LazyWaveConfigurationError) as exc:
            make_config(hangoff_bend_radius=2000.0)
        assert "bend radius" in str(exc.value).lower()

    def test_consistent_supplied_bend_radius_is_accepted(self):
        derived = derive_hangoff_bend_radius(
            vertical_span=500.0 - 150.0, hangoff_angle=15.0
        )
        config = make_config(hangoff_bend_radius=derived)
        assert math.isclose(config.hangoff_bend_radius, derived, rel_tol=1e-12)

    def test_one_bend_radius_is_used_everywhere(self, solver):
        """Hang-off, sag and touchdown segments share the single derived radius."""
        results = solver.solve(make_config())
        r = results.hangoff_to_sag.bend_radius
        assert math.isclose(results.sag_to_buoyancy.bend_radius, r, rel_tol=1e-12)
        assert math.isclose(results.buoyancy_to_touchdown.bend_radius, r, rel_tol=1e-12)

    def test_forces_use_the_derived_bend_radius(self, solver):
        """Fh = R_derived * w. Previously Fh used the unbound supplied radius."""
        config = make_config()
        results = solver.solve(config)
        expected_fh = (
            results.hangoff_to_sag.bend_radius * config.weight_without_buoyancy
        )
        assert math.isclose(results.horizontal_force, expected_fh, rel_tol=1e-12)
        expected_fv = expected_fh + config.weight_without_buoyancy * (
            results.hangoff_to_sag.arc_length
        )
        assert math.isclose(results.vertical_force, expected_fv, rel_tol=1e-12)


class TestHistoricalReferenceCases:
    """Pin the repaired kernel against historical output from the ancestor solver."""

    @pytest.mark.parametrize("case", REFERENCE_CASES, ids=lambda c: c["id"])
    def test_matches_historical_result(self, solver, case):
        config = LazyWaveConfiguration(**case["inputs"])
        results = solver.solve(config)
        e = case["expected"]

        def close(actual, key, rel_tol=1e-9):
            assert math.isclose(actual, e[key], rel_tol=rel_tol), (
                f"{key}: got {actual!r}, historical {e[key]!r}"
            )

        close(results.hangoff_to_sag.vertical_distance, "hangoff_d")
        close(results.hangoff_to_sag.bend_radius, "hangoff_bend_radius")
        close(results.hangoff_to_sag.arc_length, "hangoff_S")
        close(results.hangoff_to_sag.horizontal_distance, "hangoff_X")

        close(results.sag_to_buoyancy.vertical_distance, "sag_to_buoyancy_d")
        close(results.sag_to_buoyancy.arc_length, "sag_to_buoyancy_S")
        close(results.sag_to_buoyancy.horizontal_distance, "sag_to_buoyancy_X")

        close(results.buoyancy_to_hog.bend_radius, "buoyancy_to_hog_R")
        close(results.buoyancy_to_hog.vertical_distance, "buoyancy_to_hog_d")
        close(results.buoyancy_to_hog.arc_length, "buoyancy_to_hog_S")

        close(results.hog_to_buoyancy_end.vertical_distance, "hog_to_buoyancy_d")
        close(results.hog_to_buoyancy_end.arc_length, "hog_to_buoyancy_S")

        close(results.buoyancy_to_touchdown.vertical_distance, "buoyancy_to_touchdown_d")
        close(results.buoyancy_to_touchdown.arc_length, "buoyancy_to_touchdown_S")

        close(results.total_arc_length, "total_S")
        close(results.total_horizontal_distance, "total_X")
        close(results.horizontal_force, "Fh")
        close(results.vertical_force, "Fv")

    @pytest.mark.parametrize("case", REFERENCE_CASES, ids=lambda c: c["id"])
    def test_historical_cases_close_on_the_seabed(self, solver, case):
        config = LazyWaveConfiguration(**case["inputs"])
        results = solver.solve(config)
        assert abs(results.vertical_closure_error) < 1e-9

    @pytest.mark.parametrize("case", REFERENCE_CASES, ids=lambda c: c["id"])
    def test_historical_hangoff_span_is_the_closure_identity(self, case):
        """d_hangoff == vertical_distance - sag_bend_elevation held in every run."""
        i = case["inputs"]
        assert math.isclose(
            case["expected"]["hangoff_d"],
            i["vertical_distance"] - i["sag_bend_elevation"],
            rel_tol=1e-12,
        )

    @pytest.mark.parametrize("case", REFERENCE_CASES, ids=lambda c: c["id"])
    def test_historical_bend_radius_was_derived_not_free(self, case):
        """The historical bend radius is reproduced by (span, angle) alone."""
        i, e = case["inputs"], case["expected"]
        assert math.isclose(
            derive_hangoff_bend_radius(
                vertical_span=i["vertical_distance"] - i["sag_bend_elevation"],
                hangoff_angle=i["hangoff_angle"],
            ),
            e["hangoff_bend_radius"],
            rel_tol=1e-9,
        )
