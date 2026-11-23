"""Tests for the off-repo real-RAO library adapter.

Parser logic is tested against a SYNTHETIC heading-block fixture (no off-repo or
confidential data), so it runs in CI. Resolution/degradation is tested with the
env var unset.
"""

from __future__ import annotations

import math
import os

import numpy as np
import pytest

from digitalmodel.marine_ops.vessel_db.hulls_adapter import (
    parse_heading_block_rao,
    real_rao_datasets,
    resolve_rao_library_dir,
)


def _synthetic_rows():
    """A 2-heading heading-block table: header (heading + DOF 1..6), then
    period, amp(1..6), phase(1..6) rows. Head-sea heave ~1, sway 0."""
    rows = []
    for heading, heave in ((180.0, 1.00), (90.0, 0.20)):
        rows.append([heading, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6])  # header
        for T, h in ((20.0, heave), (10.0, heave * 0.8), (6.0, heave * 0.3)):
            sway = 0.0 if heading == 180 else 0.9
            # period, amp surge,sway,heave,roll,pitch,yaw, then 6 phases
            rows.append([T, 0.5, sway, h, 0.0, 0.1, 0.0, 0, 0, 0, 0, 0, 0])
    return rows


def test_parse_synthetic_heading_block():
    rao = parse_heading_block_rao(_synthetic_rows(), vessel_name="synthetic")
    assert rao is not None
    assert rao.vessel_name == "synthetic"
    assert sorted(rao.directions) == [90.0, 180.0]
    assert rao.amplitudes.shape == (3, 2, 6)
    # head-sea (180) heave at longest period (lowest freq) ~ 1.0, sway ~ 0
    di = list(rao.directions).index(180.0)
    fi = int(np.argmin(rao.frequencies))
    assert rao.amplitudes[fi, di, 2] == pytest.approx(1.0, abs=1e-6)
    assert rao.amplitudes[fi, di, 1] == pytest.approx(0.0, abs=1e-6)


def test_frequencies_are_rad_per_s():
    rao = parse_heading_block_rao(_synthetic_rows(), vessel_name="x")
    # longest period 20 s -> omega = 2*pi/20
    assert rao.frequencies.min() == pytest.approx(2 * math.pi / 20.0, rel=1e-6)


def test_unrecognised_layout_returns_none():
    rows = [["junk", "data"], [1, 2, 3]]
    assert parse_heading_block_rao(rows, vessel_name="x") is None


def test_resolver_none_when_unset(monkeypatch):
    monkeypatch.delenv("VESSEL_RAO_LIBRARY_PATH", raising=False)
    assert resolve_rao_library_dir() is None
    with pytest.raises(FileNotFoundError):
        resolve_rao_library_dir(require=True)


def test_datasets_empty_when_unset(monkeypatch):
    monkeypatch.delenv("VESSEL_RAO_LIBRARY_PATH", raising=False)
    assert real_rao_datasets() == {}


@pytest.mark.skipif(resolve_rao_library_dir() is None,
                    reason="VESSEL_RAO_LIBRARY_PATH not configured")
def test_real_library_parses_when_configured():
    ds = real_rao_datasets()
    assert ds, "configured library yielded no datasets"
    for rao in ds.values():
        assert rao.amplitudes.shape == (len(rao.frequencies), len(rao.directions), 6)
