"""Assert actual package deck text; no engine mock or solver execution."""
import pytest

from digitalmodel.hydrodynamics.aqwa.aqwa_dat_files import AqwaDATFiles


def test_deck6_written_frequency():
    # AQWA Reference Manual 2025 R1 10.3: HRTZ=1/T, FREQ=2*pi/T.
    parser = AqwaDATFiles()
    lines = parser.get_dc_06_body({}, {"data": {"element_type": "HRTZ", "raw": {"period": [20., 10.]}}})
    assert len(lines) == 2
    assert lines[0][6:10] == "HRTZ"
    assert float(lines[0][20:30]) == pytest.approx(0.05)
    assert float(lines[1][20:30]) == pytest.approx(0.1)
    assert parser._parse_deck6(lines).periods == pytest.approx([20., 10.])


def test_negative_damping_deck_rejected():
    with pytest.raises(ValueError, match="nonnegative"):
        AqwaDATFiles().get_dc_07_body({}, {"data": {"element_type": "ADMP", "raw": {
            "added_damping": {"rxx": -1., "ryy": 0.}}}})
