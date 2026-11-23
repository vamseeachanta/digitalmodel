"""Tests for multi-vessel screening and comparison."""
from __future__ import annotations

import numpy as np
import pytest

from digitalmodel.marine_ops.installation.vessel_screening import (
    VesselScreeningResult,
    screen_vessels,
)
from digitalmodel.marine_ops.installation.models import (
    CraneTipConfig,
    InstallationCriteria,
    Structure,
    Vessel,
)
def _make_rao(frequencies, headings, peak_amp, peak_freq=0.8):
    """Helper: synthetic Gaussian RAO peaked at peak_freq."""
    nf, nh = len(frequencies), len(headings)
    amp = peak_amp * np.exp(-2 * (frequencies - peak_freq) ** 2)
    amp_2d = np.tile(amp[:, None], (1, nh))
    phase_2d = np.zeros((nf, nh))
    return {"amplitude": amp_2d, "phase": phase_2d}


class TestScreenVessels:
    """N vessels × M structures operability matrix."""

    def _make_vessel(self, name, rao_scale, frequencies, headings, crane_offset):
        """Helper to create a vessel with scaled RAOs."""
        rao_data = {
            "surge": _make_rao(frequencies, headings, 0.5 * rao_scale),
            "sway": _make_rao(frequencies, headings, 0.3 * rao_scale),
            "heave": _make_rao(frequencies, headings, 1.0 * rao_scale),
            "roll": _make_rao(frequencies, headings, 3.0 * rao_scale),
            "pitch": _make_rao(frequencies, headings, 2.0 * rao_scale),
            "yaw": _make_rao(frequencies, headings, 0.5 * rao_scale),
        }
        return Vessel(
            name=name,
            rao_frequencies=frequencies,
            rao_headings=headings,
            rao_data=rao_data,
            crane_tip=CraneTipConfig(x_m=crane_offset, y_m=10.0, z_m=20.0),
        )

    def test_single_vessel_single_structure(self, vessel, structure, criteria, tp_range):
        result = screen_vessels([vessel], [structure], criteria, tp_range)
        assert isinstance(result, VesselScreeningResult)
        assert result.hs_limit_matrix_m.shape == (1, 1)
        assert len(result.rankings) == 1

    def test_two_vessels_ranked(self, structure, criteria, tp_range, frequencies, headings):
        v_good = self._make_vessel("Good", 0.5, frequencies, headings, 20.0)
        v_bad = self._make_vessel("Bad", 2.0, frequencies, headings, 40.0)
        result = screen_vessels([v_good, v_bad], [structure], criteria, tp_range)
        # Good vessel (smaller RAOs, shorter crane arm) should rank higher
        assert result.rankings[0]["vessel"] == "Good"
        assert result.rankings[1]["vessel"] == "Bad"

    def test_matrix_dimensions(self, structure, criteria, tp_range, frequencies, headings):
        vessels = [
            self._make_vessel(f"V{i}", 1.0 + 0.2 * i, frequencies, headings, 25.0)
            for i in range(3)
        ]
        structures = [
            Structure(name=f"S{j}", length_m=4.0 + j, width_m=3.0, height_m=2.0, mass_air_kg=15000.0)
            for j in range(2)
        ]
        result = screen_vessels(vessels, structures, criteria, tp_range)
        assert result.hs_limit_matrix_m.shape == (3, 2)
        assert len(result.governing_criteria) == 3
        assert len(result.governing_criteria[0]) == 2

    def test_with_scatter_diagram(self, vessel, structure, criteria, tp_range, scatter_diagram):
        hs, tp, counts = scatter_diagram
        result = screen_vessels(
            [vessel], [structure], criteria, tp_range,
            scatter_hs=hs, scatter_tp=tp, scatter_counts=counts,
        )
        # With scatter data, operability should be computed
        assert result.operability_matrix_pct.shape == (1, 1)
        assert 0.0 <= result.operability_matrix_pct[0, 0] <= 100.0

    def test_rankings_sorted_descending(self, structure, criteria, tp_range, frequencies, headings):
        vessels = [
            self._make_vessel(f"V{i}", 0.5 + 0.5 * i, frequencies, headings, 20.0 + 5 * i)
            for i in range(4)
        ]
        result = screen_vessels(vessels, [structure], criteria, tp_range)
        # Rankings should be in descending order of metric
        metric_key = list(result.rankings[0].keys())[-1]  # Last key is the metric
        for i in range(len(result.rankings) - 1):
            assert result.rankings[i][metric_key] >= result.rankings[i + 1][metric_key]
