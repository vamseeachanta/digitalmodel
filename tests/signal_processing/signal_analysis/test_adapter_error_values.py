# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic adapter boundaries and error recovery, without solver imports.

Counter accuracy is deliberately excluded for the default in-house path:
the linked counting-defect assessment already records its nonconformance.
An injected, hand-counted table tests adapter forwarding and conversion.
Expected values below are independent arithmetic or documented local API
contracts. Floating-point tolerances cover roundoff, not engineering limits.
"""

import numpy as np
import pandas as pd
import pytest

from digitalmodel.signal_processing.signal_analysis import adapters
from digitalmodel.signal_processing.signal_analysis.core.spectral import (
    SpectralAnalyzer,
)
from digitalmodel.signal_processing.signal_analysis.orcaflex.error_handling import (
    AnalysisError,
    ColumnMappingError,
    DataValidationError,
    ErrorHandler,
    RainflowError,
    SafeAnalyzer,
    SpectralAnalysisError,
    safe_analysis,
)


class HandCountedCounter:
    """Boundary double for four excursions 0->2->0, retaining end half cycles.

    Eight adjacent reversals have range 2, midpoint 1, weight 1/2 each;
    their weight is 4 cycles. This table tests integration only, and does
    not execute or certify the known defective default counting algorithm.
    """

    def __init__(self, method="astm"):
        self.method = method

    def count_cycles(self, signal, extract_info=True):
        assert list(signal) == [0, 2, 0, 2, 0, 2, 0, 2, 0]
        table = pd.DataFrame({"range": [2] * 8, "mean": [1] * 8, "count": [0.5] * 8})
        if extract_info:
            table["i_start"] = np.arange(8)
            table["i_end"] = np.arange(1, 9)
        return table


def test_fatigue_adapter_preserves_hand_counted_table():
    """The injected direct table and adapter output retain all eight half cycles.

    The independent check is eight weights of 0.5, total 4; range=2,
    mean=1. Dictionary conversion must preserve each numeric column.
    """
    signal = np.array([0, 2, 0, 2, 0, 2, 0, 2, 0])
    counter = HandCountedCounter()
    with pytest.warns(DeprecationWarning, match="FatigueAnalysis is deprecated"):
        adapter = adapters.FatigueAnalysisAdapter()
    adapter.rainflow_counter = counter
    direct = counter.count_cycles(signal)
    table, mapping = adapter.get_rainflow_from_timetrace(signal)
    pd.testing.assert_frame_equal(table, direct)
    pd.testing.assert_frame_equal(pd.DataFrame(mapping), direct)
    assert table["count"].sum() == 4
    assert table["range"].tolist() == [2] * 8
    assert table["mean"].tolist() == [1] * 8


def test_half_cycle_adapter_preserves_hand_ranges(monkeypatch):
    """Each of eight half cycles has range 2; period is compatibility-only.

    Replace only the counting boundary. The real adapter must request the
    half_cycle mode and return all eight ranges, without importing a solver.
    """
    created = []

    def counter_factory(method):
        counter = HandCountedCounter(method)
        created.append(counter)
        return counter

    with pytest.warns(UserWarning, match="open-source alternative"):
        adapter = adapters.OrcaFlexAdapter()
    monkeypatch.setattr(adapters, "RainflowCounter", counter_factory)
    signal = np.array([0, 2, 0, 2, 0, 2, 0, 2, 0])
    direct = HandCountedCounter("half_cycle").count_cycles(signal, extract_info=False)
    assert adapter.RainflowHalfCycles(signal, period=7) == direct["range"].tolist()
    assert adapter.RainflowHalfCycles(signal) == [2] * 8
    assert [counter.method for counter in created] == ["half_cycle", "half_cycle"]


def test_fatigue_adapter_damage_has_independent_hand_value():
    """Use the documented adapter law N=A*S**(-m), with chosen A=1000,m=2.

    Four cycles of range 2 allow N=1000/4=250 cycles, hence D=4/250
    =0.016. This artificial curve represents no material or standard.
    Absolute tolerance 1e-12 permits division roundoff.
    Source: adapters.FatigueAnalysisAdapter.damage_from_rainflow_cycles,
    the existing local compatibility equation, rather than a design curve.
    """
    cycles = pd.DataFrame({"range": [2.0], "mean": [1.0], "count": [4.0]})
    curve = {"A": 1000, "m": 2}
    with pytest.warns(DeprecationWarning):
        adapter = adapters.FatigueAnalysisAdapter()
    value = adapter.damage_from_rainflow_cycles(cycles, curve)
    assert value == pytest.approx(0.016, rel=0, abs=1e-12)


def test_migration_guide_names_reachable_replacements():
    """The returned document identifies the public replacements it recommends."""
    guide = adapters.create_migration_guide()
    assert isinstance(guide, str)
    assert "Migration Guide: Signal Analysis Module" in guide
    assert "get_rainflow_count_from_time_series | count_cycles" in guide
    assert "window_average_fft | window_averaged_fft" in guide
    assert callable(SpectralAnalyzer.window_averaged_fft)
    assert callable(
        adapters.TimeSeriesComponentsAdapter.get_rainflow_count_from_time_series
    )


@pytest.mark.parametrize(
    "error_type",
    [DataValidationError, ColumnMappingError, RainflowError, SpectralAnalysisError],
)
def test_error_types_and_handler_preserve_message(error_type):
    """Local error API: typed errors inherit AnalysisError and retain their message."""
    with pytest.raises(AnalysisError, match="synthetic failure") as raised:
        raise error_type("synthetic failure")
    assert type(raised.value) is error_type
    handler = ErrorHandler()
    report = handler.log_error("synthetic analysis", raised.value, {"sample_count": 16})
    assert report == {
        "context": "synthetic analysis",
        "error_type": error_type.__name__,
        "error_message": "synthetic failure",
        "data": {"sample_count": 16},
    }
    handler.log_warning("synthetic analysis", "check input")
    assert handler.get_summary()["errors"] == 1
    assert handler.get_summary()["warnings"] == 1
    handler.clear_logs()
    assert handler.get_summary()["error_log"] == []
    assert handler.get_summary()["warning_log"] == []


@pytest.mark.parametrize("fallback", [None, -1, {"value": -1}])
def test_safe_analysis_documented_fallback(fallback):
    """The local decorator returns a scalar or copies a dictionary plus error detail."""

    @safe_analysis(fallback_result=fallback, error_key="failure")
    def calculate():
        raise SpectralAnalysisError("synthetic failure")

    result = calculate()
    if isinstance(fallback, dict):
        assert result == {
            "value": -1,
            "failure": {
                "function": "calculate",
                "error": "synthetic failure",
                "type": "SpectralAnalysisError",
            },
        }
        assert fallback == {"value": -1}
        assert result is not fallback
    else:
        assert result == fallback


def test_safe_analysis_success_and_reraise():
    """A successful call is returned; continue_on_error=False preserves the exception."""
    assert safe_analysis(-1)(lambda: 8)() == 8
    error = DataValidationError("synthetic failure")

    @safe_analysis(continue_on_error=False)
    def calculate():
        raise error

    with pytest.raises(DataValidationError, match="synthetic failure") as raised:
        calculate()
    assert raised.value is error


@pytest.mark.parametrize(
    ("signal", "message", "span"),
    [([0, 2], "Signal too short", 2), ([4, 4, 4], "Signal is constant", 0)],
)
def test_safe_rainflow_validation_wraps_error_without_counting(signal, message, span):
    """Invalid signals return typed error reports and zero cycles before counting.

    Hand spans: max(0,2)-min(0,2)=2; a constant has span zero.
    No known defective counter path is evaluated.
    """
    result = SafeAnalyzer().safe_rainflow_analysis(np.array(signal), counter=None)
    assert result["success"] is False
    assert result["cycles"] is None
    assert result["error"]["error_type"] == "DataValidationError"
    assert message in result["error"]["error_message"]
    assert result["statistics"]["total_cycles"] == 0
    assert result["statistics"]["max_range"] == span


def test_safe_spectral_failure_report_and_retry():
    """Both FFT attempts raise the same typed error; the report retains type/message."""

    class BrokenAnalyzer:
        calls = 0

        def compute_spectrum(self, signal):
            self.calls += 1
            raise SpectralAnalysisError("synthetic spectrum failure")

    analyzer = BrokenAnalyzer()
    handler = ErrorHandler()
    result = SafeAnalyzer(handler).safe_spectral_analysis(np.arange(16), analyzer, {})
    assert analyzer.calls == 2
    assert result["success"] is False
    assert result["spectrum"] is None
    assert result["error"]["error_type"] == "SpectralAnalysisError"
    assert result["error"]["error_message"] == "synthetic spectrum failure"
    assert handler.get_summary()["warnings"] == 1


def test_safe_analyzer_characteristics_have_hand_values():
    """For [0,2,4,6], mean=3 and population variance=(9+1+1+9)/4=5.

    The API reports population standard deviation, so std=sqrt(5).
    Absolute tolerance 1e-12 allows square-root roundoff. No sample is NaN/Inf.
    """
    report = SafeAnalyzer().validate_signal(np.array([0.0, 2.0, 4.0, 6.0]))
    assert report["valid"] is True
    assert report["warnings"] == []
    values = report["characteristics"]
    assert values["mean"] == pytest.approx(3, rel=0, abs=1e-12)
    assert values["std"] == pytest.approx(np.sqrt(5), rel=0, abs=1e-12)
    assert (values["min"], values["max"], values["length"]) == (0, 6, 4)
    assert (values["nan_count"], values["inf_count"]) == (0, 0)
