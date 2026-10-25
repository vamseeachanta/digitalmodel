"""Tests for digitalmodel.orcaflex.weather_window module."""

import importlib
import importlib.util
import json
from datetime import datetime, timedelta, timezone
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.orcaflex.weather_window import (
    OperabilityInput,
    OperabilityTable,
    ScatterDiagram,
    SeasonalOperability,
    WoWEstimate,
    analyse_persistence,
)


class TestScatterDiagram:
    """Tests for wave scatter diagram."""

    def test_default_scatter_sums_to_one(self):
        """Default scatter diagram probabilities should sum to 1."""
        sd = ScatterDiagram()
        scatter = sd.generate_default_scatter()
        total = sum(sum(row) for row in scatter)
        assert total == pytest.approx(1.0, abs=0.01)

    def test_probability_below_large_limit(self):
        """Large Hs limit should capture most probability."""
        sd = ScatterDiagram()
        prob = sd.total_probability_below(hs_limit=100.0)
        assert prob > 0.95

    def test_probability_below_zero_limit(self):
        """Zero Hs limit should capture no probability."""
        sd = ScatterDiagram()
        prob = sd.total_probability_below(hs_limit=0.0)
        assert prob == 0.0

    def test_get_occurrences_shape(self):
        """Occurrences array should match bin dimensions."""
        sd = ScatterDiagram()
        arr = sd.get_occurrences()
        assert arr.shape == (len(sd.hs_bins), len(sd.tp_bins))


class TestOperability:
    """Tests for operability calculations."""

    def test_operability_percentage(self):
        """Operability should be between 0 and 100%."""
        op_input = OperabilityInput(hs_limit=2.5)
        result = op_input.calculate_operability()
        assert 0 <= result["operability_pct"] <= 100
        assert result["operability_pct"] + result["non_operability_pct"] == pytest.approx(100.0)

    def test_higher_limit_higher_operability(self):
        """Higher Hs limit should give higher operability."""
        sd = ScatterDiagram()
        op1 = OperabilityInput(hs_limit=1.5, scatter=sd).calculate_operability()
        op2 = OperabilityInput(hs_limit=3.5, scatter=sd).calculate_operability()
        assert op2["operability_pct"] >= op1["operability_pct"]

    def test_operability_table(self):
        """Operability table should increase monotonically with Hs limit."""
        ot = OperabilityTable(hs_limits=[1.0, 2.0, 3.0, 4.0, 5.0])
        table = ot.generate_table()
        assert len(table) == 5
        for i in range(len(table) - 1):
            assert table[i + 1]["operability_pct"] >= table[i]["operability_pct"]


class TestPersistence:
    """Tests for persistence (weather window) analysis."""

    def test_all_below_threshold(self):
        """All data below threshold should give one large window."""
        hs = np.ones(100) * 1.0  # all 1.0 m
        result = analyse_persistence(hs, hs_limit=2.0, time_step_hours=3.0)
        assert result.num_windows == 1
        assert result.mean_window_hours == pytest.approx(300.0)  # 100 * 3

    def test_all_above_threshold(self):
        """All data above threshold should give zero windows."""
        hs = np.ones(100) * 5.0
        result = analyse_persistence(hs, hs_limit=2.0)
        assert result.num_windows == 0
        assert result.mean_window_hours == 0.0

    def test_alternating_pattern(self):
        """Alternating above/below should give many short windows."""
        hs = np.array([1.0, 5.0] * 50)  # alternating
        result = analyse_persistence(hs, hs_limit=2.0, time_step_hours=1.0)
        assert result.num_windows == 50
        assert result.mean_window_hours == pytest.approx(1.0)

    def test_total_hours_below(self):
        """Total hours below should be counted correctly."""
        hs = np.array([1.0, 1.0, 5.0, 1.0])
        result = analyse_persistence(hs, hs_limit=2.0, time_step_hours=3.0)
        assert result.total_hours_below == pytest.approx(9.0)  # 3 points * 3 hours


class TestSeasonalOperability:
    """Tests for seasonal operability."""

    def test_best_months_default(self):
        """Default North Sea data should have summer months as best."""
        so = SeasonalOperability()
        best = so.best_months(min_operability=70.0)
        assert "Jul" in best
        assert "Aug" in best

    def test_annual_average(self):
        """Annual average should be computed correctly."""
        so = SeasonalOperability(operability_pct=[50.0] * 12)
        assert so.annual_average() == pytest.approx(50.0)

    def test_no_months_with_high_threshold(self):
        """Very high threshold should return no suitable months."""
        so = SeasonalOperability()
        best = so.best_months(min_operability=99.0)
        assert len(best) == 0


class TestWoW:
    """Tests for Waiting-on-Weather estimate."""

    def test_wow_basic(self):
        """WoW should be positive for less than 100% operability."""
        wow = WoWEstimate(operation_duration_hours=72.0, operability_pct=70.0)
        result = wow.estimate_wow_hours()
        assert result["base_wow_hours"] > 0
        assert result["design_wow_hours"] > result["base_wow_hours"]

    def test_wow_100_pct_operability(self):
        """100% operability should give zero WoW."""
        wow = WoWEstimate(operability_pct=100.0)
        result = wow.estimate_wow_hours()
        assert result["base_wow_hours"] == 0.0

    def test_wow_total_duration(self):
        """Total duration should be operation + design WoW."""
        wow = WoWEstimate(operation_duration_hours=48.0, operability_pct=50.0, contingency_factor=1.0)
        result = wow.estimate_wow_hours()
        assert result["total_operation_hours"] == pytest.approx(
            wow.operation_duration_hours + result["design_wow_hours"]
        )


def sample(offset=0, **updates):
    """Build an explicitly synthetic co-timed interval, without site criteria."""
    row = {
        "start_utc": datetime(2000, 1, 1, tzinfo=timezone.utc) + timedelta(hours=offset),
        "duration_hours": 1.0,
        "hs_m": 1.0, "tp_s": 7.0, "wind_mps": 8.0, "current_mps": 0.5,
        "units": {"duration_hours": "h", "hs_m": "m", "tp_s": "s",
                  "wind_mps": "m/s", "current_mps": "m/s"},
        "provenance": {
            key: {"source_id": "synthetic", "record_id": f"sample-{offset}",
                  "quality": "synthetic"}
            for key in ("hs_m", "tp_s", "wind_mps", "current_mps")
        },
    }
    row.update(updates)
    return row


def six_samples():
    return [sample(), sample(1, hs_m=2.0, tp_s=8.0, wind_mps=10.0, current_mps=1.0),
            sample(2, wind_mps=10.1), sample(3, current_mps=1.1),
            sample(4, hs_m=2.1), sample(5, tp_s=8.1)]


def joint(rows=None, **updates):
    arguments = dict(hs_limit=2.0, tp_limit=8.0, wind_limit_mps=10.0,
                     current_limit_mps=1.0, samples=six_samples() if rows is None else rows)
    arguments.update(updates)
    return OperabilityInput(**arguments).calculate_operability()


class TestSynchronizedOperability:
    def test_joint_duration_and_equality(self):
        result = joint()
        assert result["sample_passes"] == [True, True, False, False, False, False]
        assert result["passing_duration_hours"] == 2.0
        assert result["observed_duration_hours"] == 6.0
        assert result["missing_coverage_hours"] == 0.0
        assert result["operability_pct"] == pytest.approx(100 * 2 / 6)
        assert result["window_durations_hours"] == [2.0]
        wave = joint(wind_limit_mps=None, current_limit_mps=None)
        assert wave["passing_duration_hours"] == 4.0
        assert wave["operability_pct"] == pytest.approx(100 * 4 / 6)

    @pytest.mark.parametrize("limit", ["wind_limit_mps", "current_limit_mps"])
    def test_scatter_cannot_supply_enabled_samples(self, limit):
        with pytest.raises(ValueError, match="samples"):
            OperabilityInput(**{limit: 1.0}).calculate_operability()

    @pytest.mark.parametrize("field", ["hs_m", "tp_s", "wind_mps", "current_mps"])
    def test_missing_enabled_value(self, field):
        with pytest.raises(ValueError):
            joint([sample(**{field: None})])

    def test_unset_speeds_allow_wave_only_samples(self):
        row = sample(wind_mps=None, current_mps=None)
        assert joint([row], wind_limit_mps=None, current_limit_mps=None)["operability_pct"] == 100.0

    @pytest.mark.parametrize("value", [0, -1, float("nan"), float("inf"), -float("inf"), 1e-15])
    def test_bad_duration_including_unrepresentable_interval(self, value):
        with pytest.raises(ValueError):
            joint([sample(duration_hours=value)])

    def test_empty_observation_denominator(self):
        with pytest.raises(ValueError):
            joint([])

    @pytest.mark.parametrize("field", ["hs_m", "tp_s", "wind_mps", "current_mps"])
    @pytest.mark.parametrize("value", [-1.0, float("nan"), float("inf"), True, "1.0"])
    def test_invalid_or_coerced_values(self, field, value):
        with pytest.raises(ValueError):
            joint([sample(**{field: value})])

    @pytest.mark.parametrize("field", ["hs_limit", "tp_limit", "wind_limit_mps", "current_limit_mps"])
    def test_infinite_limits(self, field):
        with pytest.raises(ValueError):
            joint(**{field: float("inf")})

    @pytest.mark.parametrize("rows", [
        [sample(), sample()], [sample(1), sample()],
        [sample(duration_hours=2), sample(1)],
    ])
    def test_duplicate_order_and_overlap(self, rows):
        with pytest.raises(ValueError):
            joint(rows)

    @pytest.mark.parametrize("start", ["2000-01-01T00:00:00", "2000-01-01T00:00:00+01:00", 0])
    def test_explicit_utc_only(self, start):
        with pytest.raises(ValueError):
            joint([sample(start_utc=start)])

    def test_gaps_break_windows_without_counting_as_exposure(self):
        result = joint([sample(), sample(2, duration_hours=2), sample(4, wind_mps=11)])
        assert result["observed_duration_hours"] == 4.0
        assert result["missing_coverage_hours"] == 1.0
        assert result["passing_duration_hours"] == 3.0
        assert result["window_durations_hours"] == [1.0, 2.0]
        assert result["operability_pct"] == 75.0

    def test_positive_unequal_durations_use_elapsed_time(self):
        result = joint([sample(duration_hours=0.25), sample(0.25, duration_hours=1.75, wind_mps=11)])
        assert result["operability_pct"] == 12.5
        assert result["non_operability_pct"] == 87.5

    def test_all_failed_and_failure_between_windows(self):
        assert joint([sample(wind_mps=11)])["window_durations_hours"] == []
        result = joint([sample(), sample(1, wind_mps=11), sample(2)])
        assert result["window_durations_hours"] == [1.0, 1.0]

    def test_unknown_fields_and_units(self):
        row = sample()
        row["units"]["wind_mps"] = "knots"
        for changes in ({"samples": [row]}, {"marginal_wind_probability": 0.8},
                        {"samples": [sample(unknown=3)]}):
            with pytest.raises(ValueError):
                joint(**changes)

    def test_metadata_required_and_missing_quality_never_passes(self):
        row = sample()
        del row["provenance"]["wind_mps"]
        with pytest.raises(ValueError):
            joint([row])
        row = sample()
        row["provenance"]["current_mps"]["quality"] = "missing"
        with pytest.raises(ValueError):
            joint([row])

    def test_alignment_is_one_timestamp_per_row(self):
        row = sample()
        row["wind_start_utc"] = row["start_utc"] + timedelta(hours=1)
        with pytest.raises(ValueError):
            joint([row])

    @pytest.mark.parametrize("field,tight", [("hs_limit", 1.0), ("tp_limit", 7.0),
                                            ("wind_limit_mps", 8.0), ("current_limit_mps", 0.5)])
    def test_tightening_cannot_increase_passing_duration(self, field, tight):
        assert joint(**{field: tight})["passing_duration_hours"] <= joint()["passing_duration_hours"]

    def test_marginals_cannot_determine_joint_duration(self):
        coincident = [sample(), sample(1, wind_mps=11, current_mps=2)]
        opposed = [sample(current_mps=2), sample(1, wind_mps=11)]
        assert joint(coincident)["passing_duration_hours"] == 1
        assert joint(opposed)["passing_duration_hours"] == 0

    def test_legacy_scatter_fields_and_rounding_unchanged(self):
        scatter = ScatterDiagram(hs_bins=[1, 2], tp_bins=[6, 8], occurrences=[[.1, .2], [.3, .4]])
        assert OperabilityInput(hs_limit=2, tp_limit=6, scatter=scatter).calculate_operability() == {
            "operability_pct": 40.0, "non_operability_pct": 60.0,
            "hs_limit_m": 2.0, "tp_limit_s": 6.0,
        }

    def test_revalidates_model_copy_and_mutation(self):
        operation = OperabilityInput(samples=[sample()], wind_limit_mps=10)
        operation.samples[0].wind_mps = float("nan")
        with pytest.raises(ValueError):
            operation.calculate_operability()
        copied = OperabilityInput().model_copy(update={"wind_limit_mps": 10})
        with pytest.raises(ValueError):
            copied.calculate_operability()


def weather_module():
    return importlib.import_module("digitalmodel.orcaflex.weather_window")


NDBC_HEADER = b"#YY MM DD hh mm WDIR WSPD GST WVHT DPD\n#yr mo dy hr mn degT m/s m/s m sec\n"
PROVENANCE = {"source_id": "noaa-ndbc", "record_id": "synthetic-parser-input", "quality": "synthetic"}


def current_payload(**updates):
    row = {"t": "2000-01-01 00:00", "s": "100.0", "d": "90.0", "b": "1"}
    row.update(updates)
    return json.dumps({"data": [row]}).encode()


def parse_current(payload=None, **updates):
    arguments = dict(provenance=dict(PROVENANCE, source_id="coops"), units="metric",
                     time_zone="gmt", expected_bin=1)
    arguments.update(updates)
    return weather_module().parse_coops_currents(current_payload() if payload is None else payload, **arguments)


class TestNoaaSuppliedBytes:
    def test_ndbc_field_meanings_and_no_peak_or_gust_substitution(self):
        row = weather_module().parse_ndbc_standard_met(
            NDBC_HEADER + b"2000 01 01 00 00 90 8.0 15.0 1.0 7.0\n", provenance=PROVENANCE)[0]
        assert row["wind_mps"] == 8.0
        assert row["wind_from_degrees_true"] == 90.0
        assert row["dominant_period_s"] == 7.0
        assert "tp_s" not in row
        assert row["start_utc"] == datetime(2000, 1, 1, tzinfo=timezone.utc)
        assert row["provenance"]["source_id"] == "noaa-ndbc"

    def test_ndbc_missing_stays_missing(self):
        row = weather_module().parse_ndbc_standard_met(
            NDBC_HEADER + b"2000 01 01 00 00 MM MM 15.0 MM MM\n", provenance=PROVENANCE)[0]
        assert row["wind_mps"] is None and row["hs_m"] is None
        assert row["dominant_period_s"] is None

    @pytest.mark.parametrize("body", [b"", NDBC_HEADER, NDBC_HEADER.replace(b"m/s", b"kts"),
        NDBC_HEADER.replace(b"WSPD", b"UNKNOWN"),
        NDBC_HEADER + b"2000 01 01 00 00 90 nan 15 1 7\n",
        NDBC_HEADER + b"2000 01 01 00 00 90 99.0 15 1 7\n"])
    def test_ndbc_unsupported_schema_units_numeric_missing_rejected(self, body):
        with pytest.raises(ValueError):
            weather_module().parse_ndbc_standard_met(body, provenance=PROVENANCE)

    def test_coops_conversion_and_bin_identity(self):
        row = parse_current()[0]
        assert row["current_mps"] == 1.0
        assert row["current_direction_degrees"] == 90.0
        assert row["bin_id"] == 1
        assert "depth_m" not in row

    @pytest.mark.parametrize("missing", ["", None])
    def test_coops_missing_is_not_zero(self, missing):
        assert parse_current(current_payload(s=missing))[0]["current_mps"] is None

    def test_coops_conversion_can_supply_synchronized_row(self):
        parsed = parse_current()[0]
        row = sample(current_mps=parsed["current_mps"])
        row["provenance"]["current_mps"] = parsed["provenance"]
        assert joint([row])["passing_duration_hours"] == 1.0

    @pytest.mark.parametrize("missing", ["", None])
    def test_coops_missing_rejected_by_joint_calculation(self, missing):
        parsed = parse_current(current_payload(s=missing))[0]
        row = sample(current_mps=parsed["current_mps"])
        row["provenance"]["current_mps"] = parsed["provenance"]
        with pytest.raises(ValueError, match="missing values"):
            joint([row])

    @pytest.mark.parametrize("field", ["s", "b"])
    def test_coops_duplicate_members_rejected(self, field):
        body = current_payload().decode().replace(f'"{field}":', f'"{field}": "0", "{field}":')
        with pytest.raises(ValueError, match="duplicate"):
            parse_current(body.encode())

    @pytest.mark.parametrize("changes", [{"units": "english"}, {"time_zone": "lst"},
        {"expected_bin": 0}, {"expected_bin": -1}, {"expected_bin": True}, {"expected_bin": 1.5}])
    def test_coops_unsupported_units_time_or_bins(self, changes):
        with pytest.raises(ValueError):
            parse_current(**changes)

    @pytest.mark.parametrize("changes", [{"s": "nan"}, {"s": "-1"}, {"s": True},
        {"b": "2"}, {"b": "1.5"}, {"d": "361"}, {"extra": "x"}, {"s": "MM"}])
    def test_coops_invalid_values_and_schema(self, changes):
        with pytest.raises(ValueError):
            parse_current(current_payload(**changes))

    @pytest.mark.parametrize("body", [b"{}", b'{"data": []}', b'{"error": {"message": "no data"}}',
        b'{"data": [{"t": "2000-01-01 00:00", "s": "100", "d": "90"}]}'])
    def test_coops_no_usable_data(self, body):
        with pytest.raises(ValueError):
            parse_current(body)


class TestSyntheticAudit:
    def test_report_reproduces_without_network_or_scratch_receipts(self, tmp_path, monkeypatch):
        import hashlib
        import socket

        def no_network(*args, **kwargs):
            raise AssertionError("audit regeneration must be offline")

        monkeypatch.setattr(socket, "socket", no_network)
        root = Path(__file__).resolve().parents[2]
        spec = importlib.util.spec_from_file_location(
            "operability_replacement", root / "scripts/reporting/operability_replacement.py")
        report = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(report)
        first, second = tmp_path / "first", tmp_path / "second"
        report.generate(first)
        report.generate(second)
        for suffix in ("json", "html"):
            name = f"operability-replacement.{suffix}"
            assert (first / name).read_bytes() == (second / name).read_bytes()
        result = json.loads((first / "operability-replacement.json").read_text())
        assert result["criteria_revision"] == "synthetic-v1"
        assert [row["case"] for row in result["inputs"]] == list("ABCDEF")
        assert result["joint"]["passing_duration_hours"] == 2.0
        assert result["wave_only"]["passing_duration_hours"] == 4.0
        assert result["joint"]["observed_duration_hours"] == 6.0
        assert result["joint"]["operability_pct_display"] == "33.3"
        assert result["wave_only"]["operability_pct_display"] == "66.7"
        assert len(result["gaps"]) == 4
        assert {gap["status"] for gap in result["gaps"].values()} == {"UNVERIFIED"}
        assert result["engineering_readiness"] == "UNVERIFIED"
        assert result["software_check"] == "PASS"
        assert result["parser_source_acceptance"]["coops_missing_values"]["status"] == "BLOCKED-IF-MISSING"
        assert len(result["source_receipts"]) == 5
        assert all(receipt["http_status"] == 200 for receipt in result["source_receipts"])
        for path, digest in result["code_sha256"].items():
            assert hashlib.sha256((root / path).read_bytes()).hexdigest() == digest
        page = (first / "operability-replacement.html").read_text()
        assert page.index("Summary") < page.index("Revision history") < page.index("Synthetic results")
        assert all(key in page for key in result["gaps"])
