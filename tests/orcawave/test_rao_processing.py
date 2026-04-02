"""Tests for orcawave.rao_processing module."""

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
