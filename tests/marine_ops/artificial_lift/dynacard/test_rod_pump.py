# ABOUTME: Regression tests for the API RP 11L rod-pump module.
# ABOUTME: Two field fixtures -- Reed Goodman's well and the Rowlan validation case.
"""Tests for :mod:`...dynacard.rod_pump`.

Fixture 1 is the driving case from the Collide "Dynamometer Discussions"
thread (July 2026): a 4,200 ft single-diameter string that shows four clear
load undulations per upstroke.

Fixture 2 is O. Lynn Rowlan (Echometer), "Over Travel Occurs on Both the
Upstroke and Down Stroke", Sucker Rod Pumping Workshop / SWPSC, slide 13 —
one well and one rod string at three pumping speeds, which isolates what does
and does not change with speed.
https://www.swpshortcourse.org/conference/2017/abstract/17-over-travel-can-occur-both-upstroke-and-down-stroke
"""

import csv
import math
from pathlib import Path

import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.rod_pump import (
    ValidityError,
    analyse,
    analyse_card,
    divergence_onset,
    intervals_are_distinguishable,
    load_datum_check,
    natural_frequency,
    peak_interval,
    peak_times,
    time_from_card_position,
    undulations_per_half_stroke,
    volumetric_efficiency,
)

CARD_CSV = (
    Path(__file__).resolve().parents[4]
    / "docs" / "collide_pe" / "dynacard-undulations" / "dynacard_digitized.csv"
)

# ---------------------------------------------------------------------------
# Fixture 1 -- Reed Goodman's well
# ---------------------------------------------------------------------------
REED = dict(
    rod_diameter_in=0.75,
    rod_length_ft=4200.0,
    surface_stroke_in=41.0,
    strokes_per_minute=6.4,
    plunger_diameter_in=1.25,
    fluid_level_ft=4300.0,
    specific_gravity=0.85,
    tubing_pressure_psi=150.0,
    casing_pressure_psi=25.0,
)


@pytest.fixture(scope="module")
def reed():
    return analyse(**REED)


def test_reed_natural_frequency(reed):
    """No = 245,000/4200 = 58.3 SPM, and dt = 60/No = 1.03 s."""
    assert reed.natural_frequency_spm == pytest.approx(58.33, abs=0.05)
    assert reed.peak_interval_s == pytest.approx(1.03, abs=0.01)


def test_reed_single_diameter_string_has_unit_taper_factor(reed):
    """Fc = 1.000 for a single-diameter string, so No' == No."""
    assert reed.taper_adjusted_frequency_spm == pytest.approx(
        reed.natural_frequency_spm
    )


def test_reed_speed_ratio_and_undulations(reed):
    """N/No' = 0.110 gives 4.56 undulations -- 4 clear humps were observed."""
    assert reed.speed_ratio == pytest.approx(0.110, abs=0.001)
    assert reed.undulations_per_half_stroke == pytest.approx(4.56, abs=0.02)
    # The observation is 4 clear humps plus a partial one near the top.
    assert 4 <= reed.undulations_per_half_stroke < 5


def test_reed_rod_string_elastic_properties(reed):
    """Er, Kr, Skr and Wr against the hand-computed values."""
    string = reed.rod_string
    assert string.elastic_constant == pytest.approx(8.762e-7, rel=1e-3)
    assert string.spring_rate_lb_per_in == pytest.approx(271.7, abs=0.5)
    assert string.stroke_spring_product_lb == pytest.approx(11_141, abs=10)
    assert string.weight_in_air_lb == pytest.approx(6_863, abs=5)


def test_reed_fluid_load_and_dimensionless_group(reed):
    """Fo = 2,096 lb at SG 0.85, giving Fo/Skr = 0.188."""
    assert reed.rod_string.fluid_load_lb == pytest.approx(2_096, abs=5)
    assert reed.rod_string.fo_over_skr == pytest.approx(0.188, abs=0.002)


def test_reed_plunger_stroke_is_shorter_than_surface_stroke(reed):
    """Rod stretch of 7.7 in shortens the 41 in surface stroke to ~33.3 in."""
    assert reed.rod_stretch_in == pytest.approx(7.7, abs=0.1)
    assert reed.plunger_stroke_in == pytest.approx(33.3, abs=0.1)
    assert reed.plunger_stroke_in < REED["surface_stroke_in"]


def test_reed_pump_displacement_uses_plunger_stroke(reed):
    """PD = 38.8 bfpd on plunger stroke, not the ~48 bfpd surface-stroke figure."""
    assert reed.pump_displacement_bpd == pytest.approx(38.8, abs=0.3)
    # Guard the specific error this module exists to prevent.
    surface_stroke_pd = 0.1484 * (math.pi / 4 * 1.25 ** 2) * 41.0 * 6.4
    assert surface_stroke_pd == pytest.approx(47.8, abs=0.3)
    assert reed.pump_displacement_bpd < surface_stroke_pd


def test_reed_overtravel_widens_the_displacement_range():
    """Overtravel of 2 in lifts plunger stroke to 35.3 in and PD to 41.1 bfpd."""
    result = analyse(**REED, overtravel_in=2.0)
    assert result.plunger_stroke_in == pytest.approx(35.3, abs=0.1)
    assert result.pump_displacement_bpd == pytest.approx(41.1, abs=0.3)


def test_reed_efficiency_is_undetermined_without_runtime_and_bo(reed_=None):
    """Efficiency must not be reported when runtime or Bo is unknown."""
    result = analyse(**REED, measured_rate_bpd=23.0)
    assert result.volumetric_efficiency is None
    assert any("volumetric efficiency" in note for note in result.undetermined)


def test_reed_efficiency_computed_when_runtime_and_bo_supplied():
    """With both supplied, efficiency is reported."""
    result = analyse(
        **REED,
        measured_rate_bpd=23.0,
        runtime_hours_per_day=24.0,
        formation_volume_factor=1.0,
    )
    assert result.volumetric_efficiency == pytest.approx(23.0 / 38.8, rel=0.02)


def test_reed_wave_dominated_regime_is_flagged_not_faulted(reed):
    """N/No' below 0.15 is expected behaviour, and must be surfaced as such."""
    assert reed.speed_ratio < 0.15
    assert any("wave-dominated" in note for note in reed.warnings)


# ---------------------------------------------------------------------------
# Fixture 1 -- the load datum anomaly
# ---------------------------------------------------------------------------
def test_reed_load_datum_check_flags_impossible_mprl():
    """MPRL of 9,274 lb exceeds the 6,863 lb air weight by 2,411 lb.

    Friction acts upward on the downstroke, so it lowers polished-rod load and
    widens the card. It cannot raise the whole card, so this has no mechanical
    explanation and points at the data or the string record.
    """
    warnings = load_datum_check(
        minimum_load_lb=9_274.0, rod_weight_in_air_lb=6_863.0
    )
    assert len(warnings) == 1
    assert "2,411 lb" in warnings[0]
    assert "load-cell" in warnings[0]


def test_load_datum_check_passes_a_consistent_card():
    """A card whose MPRL sits below the buoyed weight raises nothing."""
    assert load_datum_check(5_500.0, 6_863.0, 6_116.0) == []


@pytest.mark.skipif(not CARD_CSV.exists(), reason="digitized card not present")
def test_reed_card_metrics_from_digitized_card(reed):
    """Card metrics against the published digitization, and the datum warning."""
    positions, up, down = [], [], []
    with CARD_CSV.open() as handle:
        for row in csv.DictReader(handle):
            positions.append(float(row["position_in"]))
            up.append(float(row["upstroke_load_lb"]))
            down.append(float(row["downstroke_load_lb"]))

    card = analyse_card(
        position_in=positions + positions[::-1],
        load_lb=up + down[::-1],
        strokes_per_minute=6.4,
        rod_weight_in_air_lb=reed.rod_string.weight_in_air_lb,
        buoyant_rod_weight_lb=reed.rod_string.buoyant_weight_lb,
    )
    assert card.peak_load_lb == pytest.approx(12_438, abs=40)
    assert card.peak_load_position_in == pytest.approx(7.5, abs=0.5)
    assert card.minimum_load_lb == pytest.approx(9_274, abs=40)
    assert card.minimum_load_position_in == pytest.approx(35.5, abs=0.5)
    assert card.load_range_lb == pytest.approx(3_164, abs=60)
    assert card.polished_rod_hp == pytest.approx(1.13, abs=0.15)
    # This real card trips the datum check.
    assert any("exceeds the rod string's weight in air" in w for w in card.warnings)


# ---------------------------------------------------------------------------
# Fixture 1 -- timing uncertainty off a position-axis card
# ---------------------------------------------------------------------------
@pytest.mark.parametrize(
    "position_in,expected_velocity,expected_uncertainty",
    [(7.5, 10.62, 0.14), (23.5, 13.59, 0.11), (35.0, 9.71, 0.15)],
)
def test_reed_timing_uncertainty_grows_as_velocity_falls(
    position_in, expected_velocity, expected_uncertainty
):
    """dt = dx/v, and v falls towards both stroke ends."""
    measurement = time_from_card_position(
        position_in, stroke_in=41.0, strokes_per_minute=6.4,
        position_uncertainty_in=1.5,
    )
    assert measurement.uncertainty == pytest.approx(expected_uncertainty, abs=0.01)


def test_reed_peak_intervals_are_not_distinguishable():
    """The 1.24 s and 0.95 s intervals cannot be reported as distinct.

    Their uncertainties overlap at +/-1.5 in digitizing error, so the honest
    statement is the mean interval against the predicted 1.03 s.
    """
    times = [
        time_from_card_position(p, 41.0, 6.4, 1.5) for p in (7.5, 23.5, 35.0)
    ]
    assert not intervals_are_distinguishable(times)


# ---------------------------------------------------------------------------
# Fixture 2 -- Rowlan validation case
# ---------------------------------------------------------------------------
ROWLAN_NO_PRIME = 45.88
# The slide reads 5.22 SPM on the card panel and 5.44 SPM in both time-plot
# legends. Card fixtures use 5.22; time-domain fixtures use 5.44.
ROWLAN_CARD_SPEEDS = (4.85, 5.22, 6.12)
ROWLAN_TIME_SPEEDS = (4.85, 5.44, 6.12)


def test_rowlan_peak_interval_matches_the_slide():
    """60/45.88 = 1.3078 s against the stated 1.31 s."""
    interval = peak_interval(ROWLAN_NO_PRIME)
    assert interval == pytest.approx(1.3078, abs=0.001)
    assert abs(interval - 1.31) / 1.31 * 100 < 0.5


def test_rowlan_peak_interval_is_invariant_across_pumping_speed():
    """Peak spacing is a rod-string property; changing N does not move it."""
    intervals = {peak_times(spm, ROWLAN_NO_PRIME).interval_s
                 for spm in ROWLAN_TIME_SPEEDS}
    assert len(intervals) == 1
    assert intervals.pop() == pytest.approx(1.3078, abs=0.001)


@pytest.mark.parametrize(
    "spm,expected_n", [(4.85, 4.73), (5.44, 4.22), (6.12, 3.75)]
)
def test_rowlan_undulation_count_rises_as_speed_falls(spm, expected_n):
    """Slower pumping gives the string more time to ring per half stroke."""
    assert undulations_per_half_stroke(spm, ROWLAN_NO_PRIME) == pytest.approx(
        expected_n, abs=0.01
    )


@pytest.mark.parametrize(
    "spm,expected_half_cycle", [(4.85, 6.19), (5.44, 5.51), (6.12, 4.90)]
)
def test_rowlan_half_cycle_times(spm, expected_half_cycle):
    """Top of stroke occurs at 30/N seconds."""
    assert peak_times(spm, ROWLAN_NO_PRIME).top_of_stroke_s == pytest.approx(
        expected_half_cycle, abs=0.01
    )


def test_rowlan_card_width_increases_monotonically_with_speed():
    """Over-travel proxy: the card widens as the unit is sped up.

    INFERENCE, not a measured quantity. The slide labels the arrows 57.9 /
    71.1 / 77.9 in but does not state what they measure. Reading them as
    plunger travel along the ``Wrf + Fo Max`` reference line follows Rowlan's
    own reference-load-line method, but that reading is ours.

    The assertion is therefore deliberately weak: monotonicity with pumping
    speed, which holds under any consistent interpretation of the arrows. Do
    not promote these to absolute checks without confirming what the slide
    measures.
    """
    widths = {4.85: 57.9, 5.22: 71.1, 6.12: 77.9}
    ordered = [widths[spm] for spm in sorted(widths)]
    assert ordered == sorted(ordered)
    assert ordered[0] < ordered[-1]


# ---------------------------------------------------------------------------
# The peak-phase result
# ---------------------------------------------------------------------------
def test_upstroke_peaks_align_across_speeds_but_downstroke_peaks_do_not():
    """Upstroke ringing shares a trigger; downstroke ringing does not.

    Upstroke ringing is excited at bottom of stroke (t = 0) for every speed,
    so those peak trains coincide. Downstroke ringing is excited at top of
    stroke (t = 30/N), which moves with speed, so those trains separate.
    """
    trains = [peak_times(spm, ROWLAN_NO_PRIME, max_time_s=14.0)
              for spm in ROWLAN_TIME_SPEEDS]

    first_upstroke = {round(t.upstroke[0], 6) for t in trains}
    assert len(first_upstroke) == 1, "upstroke peaks must share a phase"

    first_downstroke = {round(t.downstroke[0], 6) for t in trains}
    assert len(first_downstroke) == len(trains), "downstroke peaks must separate"


def test_divergence_onset_is_the_earliest_top_of_stroke():
    """Overlaid traces track until the fastest unit turns around, at 4.90 s."""
    assert divergence_onset(list(ROWLAN_TIME_SPEEDS)) == pytest.approx(4.90, abs=0.01)


# ---------------------------------------------------------------------------
# Validity envelope -- fail closed
# ---------------------------------------------------------------------------
def test_envelope_refuses_resonant_speed_ratio():
    """N/No' above 0.35 invalidates the RP 11L correlations."""
    with pytest.raises(ValidityError, match="exceeds 0.35"):
        analyse(**{**REED, "strokes_per_minute": 30.0})


def test_envelope_refuses_tapered_string_without_taper_factor():
    """Fc = 1.000 is only valid for a single-diameter string."""
    with pytest.raises(ValidityError, match="taper factor"):
        analyse(**REED, is_tapered=True)


@pytest.mark.parametrize("geometry", ["mark_ii", "rotaflex", "hydraulic"])
def test_envelope_refuses_non_class_i_unit_geometry(geometry):
    """Long-stroke and Mark II units have different kinematics."""
    with pytest.raises(ValidityError, match="Class I"):
        analyse(**{**REED, "unit_geometry": geometry})


def test_non_steel_sonic_velocity_trips_the_natural_frequency_cross_check():
    """The 245,000 constant assumes steel; fibreglass must not silently pass."""
    with pytest.raises(ValueError, match="not steel"):
        natural_frequency(4200.0, sonic_velocity_ft_s=4_000.0)


def test_natural_frequency_routes_agree_for_steel():
    """245,000/L and 15c/L agree to about 0.2% -- the cross-check passes."""
    assert natural_frequency(4200.0) == pytest.approx(58.33, abs=0.05)


def test_volumetric_efficiency_returns_none_without_runtime():
    """No silent 24 h assumption."""
    assert volumetric_efficiency(23.0, 38.8, None, 1.0) is None
    assert volumetric_efficiency(23.0, 38.8, 24.0, None) is None
