"""Tests for orcawave.rao_processing module."""

import math
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.orcawave.rao_processing import (
    RAOTable,
    amplitude_phase_to_complex,
    combine_raos_multi_body,
    compare_raos,
    complex_to_amplitude_phase,
    interpolate_rao,
    rao_table_to_csv,
    read_rao_csv,
)


def _make_rao_table() -> RAOTable:
    """Helper: create a simple 2-heading, 3-period RAO table."""
    return RAOTable(
        dof="Heave",
        unit="m/m",
        periods=[5.0, 10.0, 15.0],
        headings_deg=[0.0, 90.0],
        amplitudes=[
            [0.2, 0.8, 0.5],  # heading 0
            [0.3, 1.0, 0.6],  # heading 90
        ],
        phases_deg=[
            [10.0, -30.0, -60.0],
            [15.0, -25.0, -55.0],
        ],
    )


class TestAmplitudePhaseConversion:
    """Test amplitude/phase <-> complex conversions."""

    def test_roundtrip(self):
        amp = np.array([1.0, 2.0, 0.5])
        phase = np.array([0.0, 90.0, -45.0])
        z = amplitude_phase_to_complex(amp, phase)
        amp_out, phase_out = complex_to_amplitude_phase(z)
        np.testing.assert_allclose(amp_out, amp, atol=1e-10)
        np.testing.assert_allclose(phase_out, phase, atol=1e-10)

    def test_zero_amplitude(self):
        amp = np.array([0.0])
        phase = np.array([45.0])
        z = amplitude_phase_to_complex(amp, phase)
        assert abs(z[0]) < 1e-15

    def test_complex_at_90_degrees(self):
        z = amplitude_phase_to_complex(np.array([1.0]), np.array([90.0]))
        np.testing.assert_allclose(z[0].real, 0.0, atol=1e-10)
        np.testing.assert_allclose(z[0].imag, 1.0, atol=1e-10)

    # Hand calculations and the floating-point budget are documented in
    # docs/examples/rao-complex-conventions.md, "Synthetic calculation basis".
    @pytest.mark.parametrize(
        "phase_deg, real, imag",
        [
            pytest.param(0.0, 2.0, 0.0, id="positive-real-axis"),
            pytest.param(90.0, 0.0, 2.0, id="positive-imaginary-axis"),
            pytest.param(180.0, -2.0, 0.0, id="negative-real-axis"),
            pytest.param(270.0, 0.0, -2.0, id="negative-imaginary-axis"),
            pytest.param(30.0, math.sqrt(3.0), 1.0, id="quadrant-one"),
            pytest.param(120.0, -1.0, math.sqrt(3.0), id="quadrant-two"),
            pytest.param(-135.0, -math.sqrt(2.0), -math.sqrt(2.0), id="quadrant-three"),
            pytest.param(390.0, math.sqrt(3.0), 1.0, id="positive-wrapped-phase"),
            pytest.param(-240.0, -1.0, math.sqrt(3.0), id="negative-wrapped-phase"),
        ],
    )
    def test_synthetic_conventions(self, phase_deg, real, imag):
        amplitude = np.array([2.0])
        z = amplitude_phase_to_complex(amplitude, np.array([phase_deg]))
        # Evaluate scalar trigonometry independently of the NumPy API under test.
        theta = math.radians(phase_deg)
        np.testing.assert_allclose(z.real, 2.0 * math.cos(theta), rtol=0, atol=1e-12)
        np.testing.assert_allclose(z.imag, 2.0 * math.sin(theta), rtol=0, atol=1e-12)
        # Exact axes and special-angle triangle values also anchor the sign.
        expected = np.array([complex(real, imag)])
        np.testing.assert_allclose(z, expected, rtol=0, atol=1e-12)

        # Start the inverse check from hand-calculated rectangular components.
        amp_out, phase_out = complex_to_amplitude_phase(expected)
        np.testing.assert_allclose(amp_out, math.hypot(real, imag), rtol=0, atol=1e-12)
        # Compare angles modulo a full turn: either branch-cut endpoint is valid.
        delta = np.deg2rad(phase_out - math.degrees(math.atan2(imag, real)))
        np.testing.assert_allclose(np.cos(delta), 1.0, rtol=0, atol=1e-12)
        np.testing.assert_allclose(np.sin(delta), 0.0, rtol=0, atol=1e-12)
        assert np.all((-180.0 <= phase_out) & (phase_out <= 180.0))

        # Preserve the response, rather than requiring the original wrapped angle.
        roundtrip = amplitude_phase_to_complex(*complex_to_amplitude_phase(z))
        np.testing.assert_allclose(roundtrip, expected, rtol=0, atol=1e-12)
        for turn in (-360.0, 360.0):
            wrapped = amplitude_phase_to_complex(amp_out, phase_out + turn)
            np.testing.assert_allclose(wrapped, expected, rtol=0, atol=1e-12)


def test_complex_convention_note_snippet():
    """Execute the published Python block so the runnable example stays checked."""
    note = Path(__file__).resolve().parents[2] / "docs/examples/rao-complex-conventions.md"
    blocks = note.read_text(encoding="utf-8").split("```python\n")[1:]
    assert len(blocks) == 1
    snippet = blocks[0].split("```", 1)[0]
    exec(compile(snippet, str(note), "exec"), {})


class TestReadWriteCSV:
    """Test CSV read/write roundtrip."""

    def test_read_csv(self):
        csv_text = (
            "DOF,Heave\n"
            "Unit,m/m\n"
            "Period\\Heading,0,90\n"
            "5.0,0.20/10.00,0.30/15.00\n"
            "10.0,0.80/-30.00,1.00/-25.00\n"
        )
        table = read_rao_csv(csv_text)
        assert table.dof == "Heave"
        assert len(table.periods) == 2
        assert len(table.headings_deg) == 2
        np.testing.assert_allclose(table.amplitudes[0][0], 0.20, atol=1e-6)
        np.testing.assert_allclose(table.amplitudes[1][1], 1.00, atol=1e-6)

    def test_write_csv(self):
        table = _make_rao_table()
        csv_out = rao_table_to_csv(table)
        assert "Heave" in csv_out
        assert "m/m" in csv_out
        lines = csv_out.strip().splitlines()
        assert len(lines) == 6  # header + 2 metadata + 3 data rows

    def test_csv_roundtrip(self):
        csv_text = (
            "DOF,Pitch\n"
            "Unit,deg/m\n"
            "Period\\Heading,0,180\n"
            "6.0,1.50/5.00,1.80/8.00\n"
            "12.0,2.30/-20.00,2.50/-18.00\n"
        )
        table = read_rao_csv(csv_text)
        csv_out = rao_table_to_csv(table)
        table2 = read_rao_csv(csv_out)
        np.testing.assert_allclose(table.amplitudes, table2.amplitudes, atol=1e-4)


class TestInterpolation:
    """Test bilinear RAO interpolation."""

    def test_interpolate_at_grid_point(self):
        table = _make_rao_table()
        amp, phase = interpolate_rao(table, 10.0, 0.0)
        np.testing.assert_allclose(amp, 0.8, atol=0.05)

    def test_interpolate_between_periods(self):
        table = _make_rao_table()
        amp, _ = interpolate_rao(table, 7.5, 0.0)
        # Should be between 0.2 and 0.8
        assert 0.1 < amp < 0.9

    def test_interpolate_between_headings(self):
        table = _make_rao_table()
        amp, _ = interpolate_rao(table, 10.0, 45.0)
        # Should be between 0.8 (heading 0) and 1.0 (heading 90)
        assert 0.7 < amp < 1.1


class TestCombineAndCompare:
    """Test multi-body combination and RAO comparison."""

    def test_combine_independent(self):
        t1 = _make_rao_table()
        t2 = _make_rao_table()
        combined = combine_raos_multi_body(t1, t2, coupling_factor=0.0)
        # Independent sum: amplitude ~ 2x (exactly 2x when phases are identical)
        orig_amp = np.array(t1.amplitudes)
        comb_amp = np.array(combined.amplitudes)
        np.testing.assert_allclose(comb_amp, 2.0 * orig_amp, atol=1e-10)

    def test_compare_identical(self):
        t1 = _make_rao_table()
        t2 = _make_rao_table()
        result = compare_raos(t1, t2)
        assert result.max_amplitude_diff < 1e-10
        assert result.rms_amplitude_diff < 1e-10

    def test_compare_different(self):
        t1 = _make_rao_table()
        t2 = _make_rao_table()
        t2.amplitudes[0][0] = 0.5  # Change one value
        result = compare_raos(t1, t2)
        assert result.max_amplitude_diff > 0.0
