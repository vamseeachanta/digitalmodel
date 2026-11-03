# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent counting examples for the optional metocean statistics adapter.

All data are synthetic test inputs, not design criteria. Counts follow the
NIST/SEMATECH e-Handbook (2012), section 1.3.3.14, histogram frequency definition.
Time conversion follows BIPM SI Brochure, ninth edition (2019), table 8.
"""

import builtins
from datetime import datetime, timedelta, timezone

import pandas as pd
import pytest
from metocean_stats.stats.general import nb_hours_below_threshold

from digitalmodel.data_systems.metocean_statistics import hours_below_threshold


def records(values, *, start=datetime(2024, 12, 31, 23), step=timedelta(minutes=30)):
    """Build synthetic, equally spaced observations; each represents one interval."""
    return [
        {"timestamp": start + i * step, "wave_height": value}
        for i, value in enumerate(values)
    ]


def test_hand_count_across_year_boundary_and_strict_threshold():
    """Two half-hour samples per year: [1, 2] then [3, 1].

    For threshold 2: counts are 1 and 1, so hours are 0.5 and 0.5.
    For threshold 3: counts are 2 and 1, so hours are 1.0 and 0.5.
    Equality is excluded. The last sample contributes its full interval,
    matching nb_hours_below_threshold in metocean-stats 1.2.0.
    """
    result = hours_below_threshold(
        iter(records([1, 2, 3, 1])), parameter="wave_height", thresholds=[2, 3]
    )
    assert result == {2.0: {2024: 0.5, 2025: 0.5}, 3.0: {2024: 1.0, 2025: 0.5}}


def test_matches_direct_backend_call():
    """Independent total: three samples below 4 times 2 h equals 6 h."""
    observations = records(
        [1, 2, 4, 3], start=datetime(2025, 1, 1), step=timedelta(hours=2)
    )
    frame = pd.DataFrame(observations).set_index("timestamp")
    direct = nb_hours_below_threshold(frame, "wave_height", [4])
    result = hours_below_threshold(
        observations, parameter="wave_height", thresholds=[4]
    )
    assert direct.tolist() == [[6.0]]
    assert result == {4.0: {2025: 6.0}}


def test_no_samples_or_all_samples_below_threshold():
    """[1, 2] at half-hour spacing: below 1 gives 0 h; below 3 gives 1 h."""
    assert hours_below_threshold(
        records([1, 2]), parameter="wave_height", thresholds=[1, 3]
    ) == {1.0: {2024: 0.0}, 3.0: {2024: 1.0}}


def test_aware_times_are_grouped_by_utc_year():
    """Local 01:00 with offset +02:00 is 23:00 UTC in the previous year.

    Both samples fall in that UTC year: 2 times 0.5 h equals 1 h.
    """
    start = datetime(2025, 1, 1, 1, tzinfo=timezone(timedelta(hours=2)))
    assert hours_below_threshold(
        records([1, 1], start=start), parameter="wave_height", thresholds=[2]
    ) == {2.0: {2024: 1.0}}


@pytest.mark.parametrize("values", [[], [1]])
def test_requires_enough_samples_to_infer_interval(values):
    with pytest.raises(ValueError, match="at least two"):
        hours_below_threshold(records(values), parameter="wave_height", thresholds=[2])


@pytest.mark.parametrize("bad_value", [None, float("nan"), float("inf"), "1", True])
def test_rejects_unknown_or_nonnumeric_observations(bad_value):
    with pytest.raises(ValueError, match="finite numeric"):
        hours_below_threshold(
            records([1, bad_value]), parameter="wave_height", thresholds=[2]
        )


def test_rejects_missing_field():
    observations = records([1, 2])
    del observations[-1]["wave_height"]
    with pytest.raises(ValueError, match="wave_height"):
        hours_below_threshold(observations, parameter="wave_height", thresholds=[2])


@pytest.mark.parametrize(
    "thresholds", [[], [2, 2], [None], [float("nan")], [float("inf")], [True], ["2"]]
)
def test_rejects_ambiguous_thresholds(thresholds):
    with pytest.raises(ValueError, match="threshold"):
        hours_below_threshold(
            records([1, 2]), parameter="wave_height", thresholds=thresholds
        )


@pytest.mark.parametrize("offsets", [[0, 0], [1, 0], [0, 1, 3]])
def test_rejects_duplicate_reversed_or_gapped_times(offsets):
    observations = [
        {"timestamp": datetime(2025, 1, 1) + timedelta(hours=i), "wave_height": 1}
        for i in offsets
    ]
    with pytest.raises(ValueError, match="strictly increasing and regular"):
        hours_below_threshold(observations, parameter="wave_height", thresholds=[2])


@pytest.mark.parametrize("bad_time", [None, pd.NaT, "2025-01-01", float("nan")])
def test_rejects_invalid_or_implicit_timestamp_parsing(bad_time):
    observations = records([1, 2])
    observations[-1]["timestamp"] = bad_time
    with pytest.raises(ValueError, match="timestamp"):
        hours_below_threshold(observations, parameter="wave_height", thresholds=[2])


def test_rejects_missing_timestamp():
    observations = records([1, 2])
    del observations[-1]["timestamp"]
    with pytest.raises(ValueError, match="timestamp"):
        hours_below_threshold(observations, parameter="wave_height", thresholds=[2])


def test_rejects_mixed_naive_and_aware_times():
    observations = records([1, 2])
    observations[-1]["timestamp"] = observations[-1]["timestamp"].replace(
        tzinfo=timezone.utc
    )
    with pytest.raises(ValueError, match="mixed"):
        hours_below_threshold(observations, parameter="wave_height", thresholds=[2])


def test_missing_backend_explains_optional_extra(monkeypatch):
    original_import = builtins.__import__

    def without_backend(name, *args, **kwargs):
        if name.startswith("metocean_stats"):
            raise ModuleNotFoundError("unavailable", name="metocean_stats")
        return original_import(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", without_backend)
    with pytest.raises(ImportError, match=r"digitalmodel\[metocean\]"):
        hours_below_threshold(records([1, 2]), parameter="wave_height", thresholds=[2])


def test_adapter_import_does_not_require_backend(monkeypatch):
    """The optional backend must be requested only when an analysis is called."""
    import importlib

    from digitalmodel.data_systems import metocean_statistics

    original_import = builtins.__import__

    def without_backend(name, *args, **kwargs):
        if name.startswith("metocean_stats"):
            raise ModuleNotFoundError("unavailable", name="metocean_stats")
        return original_import(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", without_backend)
    importlib.reload(metocean_statistics)


def test_missing_transitive_dependency_is_not_misreported(monkeypatch):
    """An incomplete backend install must retain its actual import failure."""
    original_import = builtins.__import__

    def incomplete_backend(name, *args, **kwargs):
        if name.startswith("metocean_stats"):
            raise ModuleNotFoundError("unavailable transitive dependency", name="scipy")
        return original_import(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", incomplete_backend)
    with pytest.raises(ModuleNotFoundError) as caught:
        hours_below_threshold(records([1, 2]), parameter="wave_height", thresholds=[2])
    assert caught.value.name == "scipy"


@pytest.mark.parametrize("parameter", [None, ""])
def test_requires_parameter_name(parameter):
    with pytest.raises(ValueError, match="parameter"):
        hours_below_threshold(records([1, 2]), parameter=parameter, thresholds=[2])
