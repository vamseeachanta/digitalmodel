"""Slice 5: no implicit acquisition metadata or seabed suitability defaults.

Limits in these tests are chosen inputs. Validation covers mathematical domains
and the line geometry of NOAA FPM (2020), 1.5.6.2, not an operational threshold.
"""

from dataclasses import replace

import pytest

from digitalmodel.subsea.survey_qa import (
    DepthUncertainty,
    Line,
    Offsets,
    Seabed,
    apply_corrections,
    estimate_latency,
    estimate_pitch,
    estimate_roll,
    estimate_yaw,
    generate_swath,
    screen_overlap,
)


@pytest.mark.parametrize(
    "changes, message",
    [
        ({"sensor_offsets_m": None}, "sensor offsets"),
        ({"sensor_offsets_m": (0, 0)}, "sensor offsets"),
        ({"sensor_offsets_m": (0, float("nan"), 0)}, "finite"),
        ({"svp": ()}, "sound-velocity profile"),
        ({"svp": ((0, 1500), (20, 1510))}, "constant"),
        ({"svp": ((20, 1500), (0, 1500))}, "increase"),
        ({"svp": ((0, -1),)}, "positive"),
        ({"svp": ((0, 1500, 1),)}, "rows"),
        ({"sonar_times_s": (0, 2, 1)}, "paired"),
        ({"sonar_times_s": tuple(reversed(range(21)))}, "monotonic"),
        ({"navigation_times_s": (0,) * 21}, "monotonic"),
        ({"navigation_times_s": tuple(i + 0.01 for i in range(21))}, "synchronised"),
        ({"sonar_times_s": (float("nan"),) + tuple(range(1, 21))}, "finite"),
        ({"sync_tolerance_s": -1}, "nonnegative"),
    ],
)
def test_acquisition_refuses_bad_metadata(survey, changes, message):
    with pytest.raises(ValueError, match=message):
        replace(survey().acquisition, **changes)


def test_supplied_sync_tolerance(survey):
    """A chosen 0.02 s tolerance accepts a chosen 0.01 s clock mismatch."""
    a = replace(
        survey().acquisition,
        sync_tolerance_s=0.02,
        navigation_times_s=tuple(i + 0.01 for i in range(21)),
    )
    assert a.sync_tolerance_s == 0.02


@pytest.mark.parametrize("estimator", [estimate_pitch, estimate_latency])
def test_flat_bottom_refused_for_slope_tests(survey, estimator):
    second = survey(direction=-1) if estimator is estimate_pitch else survey(speed=4)
    with pytest.raises(ValueError, match="slope"):
        estimator(survey(), second)


def test_cross_slope_alone_refused(survey):
    bed = Seabed(50, 0, 0.2)
    with pytest.raises(ValueError, match="along-track"):
        estimate_pitch(survey(bed=bed), survey(bed=bed, direction=-1))


@pytest.mark.parametrize("bed", [Seabed(50, 0.2, 0), Seabed(50, 0, 0, (0, 0, 5))])
def test_roll_requires_flat_feature_free_bottom(survey, bed):
    with pytest.raises(ValueError, match="flat"):
        estimate_roll(survey(bed=bed), survey(bed=bed, direction=-1))


def test_line_geometry_refusals(survey):
    bed = Seabed(50, 0.2, 0)
    with pytest.raises(ValueError, match="reciprocal"):
        estimate_roll(survey(), survey())
    with pytest.raises(ValueError, match="different speeds"):
        estimate_latency(survey(bed=bed), survey(bed=bed))
    with pytest.raises(ValueError, match="same direction"):
        estimate_latency(survey(bed=bed), survey(bed=bed, direction=-1))
    with pytest.raises(ValueError, match="same speed"):
        estimate_pitch(survey(bed=bed), survey(bed=bed, direction=-1, speed=4))
    with pytest.raises(ValueError, match="coincident"):
        estimate_pitch(survey(bed=bed), survey(bed=bed, direction=-1, track_y=10))
    with pytest.raises(ValueError, match="nadir"):
        estimate_pitch(
            survey(bed=bed, beams=(-20, 20)),
            survey(bed=bed, direction=-1, beams=(-20, 20)),
        )
    with pytest.raises(ValueError, match="spread"):
        estimate_roll(survey(beams=(0,)), survey(direction=-1, beams=(0,)))
    with pytest.raises(ValueError, match="same seabed"):
        estimate_roll(survey(), survey(bed=bed, direction=-1))


def test_yaw_requires_associated_overlap_feature(survey):
    with pytest.raises(ValueError, match="feature"):
        estimate_yaw(survey(track_y=-20), survey(track_y=20))
    bed = Seabed(50, 0, 0, (0, 0, 5))
    with pytest.raises(ValueError, match="feature"):
        estimate_yaw(
            survey(bed=bed, track_y=-20, beams=(0,)), survey(bed=bed, track_y=20)
        )


def test_sensor_lever_arm_applied_and_removed(survey):
    """Chosen lever arm (1,2,3) and b=10: origin=(-19,2,3), bottom=50;
    beam depth below sensor is 47. Inverse matrices return (-19,12,50).
    """
    s = survey()
    a = replace(s.acquisition, sensor_offsets_m=(1, 2, 3))
    offsets = Offsets(0.5, 1, 1.5, 0.08)
    raw = generate_swath(s.seabed, s.line, a, (10,), offsets, noise_std_m=0, seed=7)
    assert raw.origins_m[0] == (-19, 2, 3)
    assert apply_corrections(raw, offsets).xyz_m[0] == pytest.approx((-19, 12, 50))


@pytest.mark.parametrize(
    "changes, message",
    [
        ({"xyz_m": ()}, "nonempty"),
        ({"xyz_m": ((0, 0, float("nan")),) * 105}, "finite"),
        ({"origins_m": ((0, 0, 0),)}, "length"),
        ({"feature_index": 999}, "feature index"),
    ],
)
def test_corrupted_swath_refused(survey, changes, message):
    with pytest.raises(ValueError, match=message):
        replace(survey(), **changes)


def test_invalid_generator_domains(survey):
    s = survey()
    with pytest.raises(ValueError, match="unique"):
        generate_swath(
            s.seabed, s.line, s.acquisition, (0, 0), Offsets(), noise_std_m=0, seed=7
        )
    with pytest.raises(ValueError, match="nonnegative"):
        generate_swath(
            s.seabed, s.line, s.acquisition, (0,), Offsets(), noise_std_m=-1, seed=7
        )
    with pytest.raises(ValueError, match="below"):
        generate_swath(
            Seabed(1, 1, 0),
            s.line,
            s.acquisition,
            (0,),
            Offsets(),
            noise_std_m=0,
            seed=7,
        )
    with pytest.raises(ValueError, match="direction"):
        Line(0, 0, 0, 2)
    with pytest.raises(ValueError, match="positive"):
        Line(0, 0, 1, 0)
    with pytest.raises(ValueError, match="strictly"):
        Offsets(pitch_deg=90)


def test_missing_overlap_and_invalid_budget(survey):
    # Coefficient basis: IHO S-44 ed. 6.1.0 (2022), Table 1, Special Order.
    with pytest.raises(ValueError, match="no overlap"):
        screen_overlap(survey(), survey(track_y=100), budget=None, xy_tolerance_m=0)
    with pytest.raises(ValueError, match="nonnegative"):
        screen_overlap(survey(), survey(), budget=None, xy_tolerance_m=-1)
    with pytest.raises(ValueError, match="reference"):
        DepthUncertainty("Special", "6.1.0", 0.25, 0.0075, "")
    with pytest.raises(ValueError, match="nonnegative"):
        DepthUncertainty("Special", "6.1.0", -1, 0.0075, "Table 1")


@pytest.mark.parametrize("seed", [None, "random", 1.5])
def test_seed_must_be_explicit_integer(survey, seed):
    s = survey()
    with pytest.raises(ValueError, match="seed"):
        generate_swath(
            s.seabed,
            s.line,
            s.acquisition,
            (0,),
            Offsets(),
            noise_std_m=0.02,
            seed=seed,
        )


def test_missing_timestamps_refused(survey):
    with pytest.raises(ValueError, match="timestamps"):
        replace(survey().acquisition, sonar_times_s=None)
