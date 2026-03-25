"""
Unit tests for example model benchmark — model inventory and time domain statistics.

These are solver-agnostic unit tests (no OrcFxAPI required). All solver interactions
are replaced with mock data. Solver-integration and full-run tests are gated to licensed
machines only and are excluded from this unit test tier.

TDD approach: tests define the contracts for:
  - ModelInventory: classification of example models (Section 1)
  - TimeDomainStats: per-seed statistics extraction (Section 2)

See also:
  test_seed_equivalence_metrics.py   — Sections 3–4 (CoV, RAO comparison)
  test_benchmark_classification.py   — Sections 5–8 (pass/fail, summary, constants)
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.benchmarks.example_model_benchmark import (
    ModelCategory,
    ModelInventoryEntry,
    build_model_inventory,
    TimeDomainStats,
    compute_time_domain_stats,
)


# ===========================================================================
# Section 1: ModelInventory and Classification
# ===========================================================================

class TestModelCategory:
    """ModelCategory enum covers all required categories."""

    def test_all_categories_present(self):
        cats = {c.value for c in ModelCategory}
        assert "statics_only" in cats
        assert "time_domain" in cats
        assert "frequency_domain" in cats
        assert "both" in cats

    def test_category_values_are_strings(self):
        for cat in ModelCategory:
            assert isinstance(cat.value, str)


class TestModelInventoryEntry:
    """ModelInventoryEntry dataclass contract."""

    def test_required_fields(self):
        entry = ModelInventoryEntry(
            name="test_model",
            category=ModelCategory.TIME_DOMAIN,
            path=Path("examples/domains/mooring/mooring_example.py"),
            description="Test mooring model",
        )
        assert entry.name == "test_model"
        assert entry.category == ModelCategory.TIME_DOMAIN
        assert entry.has_time_domain is True
        assert entry.has_frequency_domain is False

    def test_both_category_flags(self):
        entry = ModelInventoryEntry(
            name="full_model",
            category=ModelCategory.BOTH,
            path=Path("examples/domains/fatigue/fatigue_analysis_examples.py"),
            description="Full fatigue model",
        )
        assert entry.has_time_domain is True
        assert entry.has_frequency_domain is True

    def test_statics_only_flags(self):
        entry = ModelInventoryEntry(
            name="static_model",
            category=ModelCategory.STATICS_ONLY,
            path=Path("examples/domains/mooring/lazy_wave_example.py"),
            description="Static catenary",
        )
        assert entry.has_time_domain is False
        assert entry.has_frequency_domain is False

    def test_frequency_domain_only_flags(self):
        entry = ModelInventoryEntry(
            name="freq_model",
            category=ModelCategory.FREQUENCY_DOMAIN,
            path=Path("examples/domains/hydrodynamics/generate_hydro_charts.py"),
            description="Frequency domain hydro",
        )
        assert entry.has_time_domain is False
        assert entry.has_frequency_domain is True


class TestBuildModelInventory:
    """build_model_inventory returns a non-empty list of classified models."""

    def test_returns_list(self):
        inventory = build_model_inventory()
        assert isinstance(inventory, list)

    def test_non_empty(self):
        inventory = build_model_inventory()
        assert len(inventory) > 0, "Inventory must contain at least one model"

    def test_all_entries_are_inventory_items(self):
        inventory = build_model_inventory()
        for entry in inventory:
            assert isinstance(entry, ModelInventoryEntry)

    def test_all_categories_represented(self):
        inventory = build_model_inventory()
        cats = {e.category for e in inventory}
        # At minimum, statics and time domain should be present
        assert ModelCategory.STATICS_ONLY in cats or ModelCategory.TIME_DOMAIN in cats

    def test_names_are_unique(self):
        inventory = build_model_inventory()
        names = [e.name for e in inventory]
        assert len(names) == len(set(names)), "Duplicate model names in inventory"

    def test_paths_are_path_objects(self):
        inventory = build_model_inventory()
        for entry in inventory:
            assert isinstance(entry.path, Path)

    def test_descriptions_are_non_empty(self):
        inventory = build_model_inventory()
        for entry in inventory:
            assert entry.description, f"Missing description for model {entry.name}"

    def test_td_models_have_correct_flags(self):
        inventory = build_model_inventory()
        for entry in inventory:
            if entry.category in (ModelCategory.TIME_DOMAIN, ModelCategory.BOTH):
                assert entry.has_time_domain is True

    def test_fd_models_have_correct_flags(self):
        inventory = build_model_inventory()
        for entry in inventory:
            if entry.category in (ModelCategory.FREQUENCY_DOMAIN, ModelCategory.BOTH):
                assert entry.has_frequency_domain is True


# ===========================================================================
# Section 2: Time Domain Statistics
# ===========================================================================

class TestTimeDomainStats:
    """TimeDomainStats dataclass contract."""

    def test_fields_present(self):
        stats = TimeDomainStats(
            seed=42,
            mean=1.5,
            std=0.3,
            maximum=3.2,
            minimum=0.1,
            rms=1.53,
        )
        assert stats.seed == 42
        assert stats.mean == pytest.approx(1.5)
        assert stats.std == pytest.approx(0.3)
        assert stats.maximum == pytest.approx(3.2)
        assert stats.minimum == pytest.approx(0.1)
        assert stats.rms == pytest.approx(1.53)

    def test_optional_extra_metrics(self):
        stats = TimeDomainStats(
            seed=1, mean=0.0, std=1.0, maximum=3.0, minimum=-3.0, rms=1.0,
            extra={"tension_max_kN": 500.0},
        )
        assert stats.extra["tension_max_kN"] == pytest.approx(500.0)


class TestComputeTimeDomainStats:
    """compute_time_domain_stats from a numpy time series."""

    def _sine_series(self, freq: float = 0.1, amplitude: float = 1.0,
                     duration: float = 100.0, fs: float = 10.0,
                     seed: int = 0) -> np.ndarray:
        rng = np.random.default_rng(seed)
        t = np.arange(0, duration, 1.0 / fs)
        signal = amplitude * np.sin(2 * np.pi * freq * t)
        signal += rng.normal(0, 0.01, size=len(t))  # tiny noise
        return signal

    def test_mean_near_zero_for_sine(self):
        sig = self._sine_series(freq=0.1, amplitude=1.0, duration=100.0)
        stats = compute_time_domain_stats(sig, seed=0)
        assert abs(stats.mean) < 0.1, "Mean of a sine wave should be near zero"

    def test_std_near_amplitude_over_sqrt2(self):
        amplitude = 2.0
        sig = self._sine_series(freq=0.1, amplitude=amplitude, duration=200.0,
                                seed=1)
        stats = compute_time_domain_stats(sig, seed=1)
        expected_std = amplitude / math.sqrt(2)
        assert abs(stats.std - expected_std) < 0.05 * expected_std

    def test_max_geq_mean(self):
        sig = self._sine_series(seed=2)
        stats = compute_time_domain_stats(sig, seed=2)
        assert stats.maximum >= stats.mean

    def test_min_leq_mean(self):
        sig = self._sine_series(seed=3)
        stats = compute_time_domain_stats(sig, seed=3)
        assert stats.minimum <= stats.mean

    def test_seed_is_recorded(self):
        sig = self._sine_series(seed=99)
        stats = compute_time_domain_stats(sig, seed=99)
        assert stats.seed == 99

    def test_rms_positive(self):
        sig = self._sine_series(seed=4)
        stats = compute_time_domain_stats(sig, seed=4)
        assert stats.rms > 0

    def test_rms_relation_to_mean_and_std(self):
        """rms^2 ≈ mean^2 + std^2 for ergodic signal."""
        sig = self._sine_series(amplitude=1.5, duration=200.0, seed=5)
        stats = compute_time_domain_stats(sig, seed=5)
        rms_from_moments = math.sqrt(stats.mean ** 2 + stats.std ** 2)
        assert abs(stats.rms - rms_from_moments) < 0.01 * stats.rms

    def test_empty_signal_raises(self):
        with pytest.raises(ValueError, match="empty"):
            compute_time_domain_stats(np.array([]), seed=0)
