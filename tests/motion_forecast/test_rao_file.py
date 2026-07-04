"""Real-file GridRAO.from_file validation (digitalmodel #1358).

Exercises the AQWA/OrcaFlex file path (unified_rao_reader -> UnifiedRAOData ->
GridRAO), which the rest of the suite covers only with analytic RAOs. Uses a
tiny hand-authored OrcaFlex-schema fixture with known values.
"""

from pathlib import Path

import numpy as np
import pytest

from digitalmodel.motion_forecast import (
    AnalyticRAO,
    GridRAO,
    WaveComponent,
    WaveForecast,
    reconstruct_motion,
)
from digitalmodel.motion_forecast.conventions import RAOSource

FIXTURE = str(Path(__file__).parent / "fixtures" / "orcaflex_rao_min.yml")
W10 = 2.0 * np.pi / 10.0  # rad/s for the 10 s period node


def test_from_file_parses_known_node_values():
    g = GridRAO.from_file(FIXTURE, RAOSource.ORCAFLEX)
    assert np.allclose(np.sort(g.freqs), [2 * np.pi / 10.0, 2 * np.pi / 5.0])
    assert np.allclose(np.sort(g.headings), [0.0, 90.0])
    H = g.H("heave", W10, 0.0)               # node: amp 0.80, phase 30 deg
    assert abs(H) == pytest.approx(0.80, abs=1e-9)
    assert np.degrees(np.angle(H)) == pytest.approx(30.0, abs=1e-9)
    assert abs(g.H("pitch", W10, 0.0)) == pytest.approx(1.5, abs=1e-9)  # deg/m


def test_aqwa_source_normalises_phase_sign():
    orca = GridRAO.from_file(FIXTURE, RAOSource.ORCAFLEX).H("heave", W10, 0.0)
    aqwa = GridRAO.from_file(FIXTURE, RAOSource.AQWA).H("heave", W10, 0.0)
    assert np.degrees(np.angle(orca)) == pytest.approx(30.0, abs=1e-9)
    assert np.degrees(np.angle(aqwa)) == pytest.approx(-30.0, abs=1e-9)  # AQWA -> canonical


def test_complex_interpolation_between_heading_nodes():
    g = GridRAO.from_file(FIXTURE, RAOSource.ORCAFLEX)
    # heave amp is 0.80 at heading 0 and 0.85 at heading 90 -> 45 deg lands between
    mid = abs(g.H("heave", W10, 45.0))
    assert 0.80 < mid < 0.85


def test_file_rao_flows_through_reconstruct():
    g = GridRAO.from_file(FIXTURE, RAOSource.ORCAFLEX)
    fc = WaveForecast([WaveComponent(W10, 1.0, 0.0, heading=0.0)], horizon=40.0)
    mf = reconstruct_motion(fc, g, dt=0.05)
    # single component through a 0.80 m/m heave RAO -> heave amplitude ~ 0.80
    # (sampled peak of a cosine slightly underestimates the true amplitude)
    assert mf.max_abs("heave") == pytest.approx(0.80, rel=5e-3)
