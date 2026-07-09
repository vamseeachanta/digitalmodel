from pathlib import Path

import numpy as np
import pytest

from digitalmodel.seismic.readers.ascii import read_ascii_accelerogram


FIXTURES = Path(__file__).parent / "fixtures"
G0_M_S2 = 9.80665


def test_reads_two_column_time_acceleration_and_converts_g_to_si():
    record = read_ascii_accelerogram(FIXTURES / "two_column_g.txt", units="g")

    assert record.npts == 4
    assert record.dt_s == pytest.approx(0.5)
    assert record.pga_g == pytest.approx(0.2)
    np.testing.assert_allclose(record.time_s, [0.0, 0.5, 1.0, 1.5])
    np.testing.assert_allclose(
        record.acceleration_m_s2,
        [0.0, 0.1 * G0_M_S2, -0.2 * G0_M_S2, 0.05 * G0_M_S2],
    )


def test_reads_single_column_dt_header_and_converts_cm_s2_to_si():
    record = read_ascii_accelerogram(
        FIXTURES / "single_column_dt_header.asc",
        units="cm_s2",
    )

    assert record.npts == 4
    assert record.dt_s == pytest.approx(0.25)
    np.testing.assert_allclose(record.time_s, [0.0, 0.25, 0.5, 0.75])
    np.testing.assert_allclose(record.acceleration_m_s2, [0.0, 0.1, -0.2, 0.05])


def test_rejects_nonuniform_two_column_time_step(tmp_path):
    path = tmp_path / "bad_dt.txt"
    path.write_text("0.0 0.0\n0.5 0.1\n1.2 0.2\n", encoding="utf-8")

    with pytest.raises(ValueError, match="uniform time step"):
        read_ascii_accelerogram(path, units="m_s2")


def test_requires_dt_for_single_column_without_header(tmp_path):
    path = tmp_path / "no_dt.asc"
    path.write_text("0.0\n1.0\n2.0\n", encoding="utf-8")

    with pytest.raises(ValueError, match="dt_s"):
        read_ascii_accelerogram(path, units="m_s2")
