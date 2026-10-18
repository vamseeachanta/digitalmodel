"""DAT regression cases: AQWA Reference Manual 2025 R1 sections 5 and 10."""
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.aqwa.aqwa_dat_files import AqwaDATFiles
from digitalmodel.hydrodynamics.aqwa.viscous_damping_determination import ViscousDampingDetermination

ROOT = Path(__file__).resolve().parents[3]


def test_deck6_template():
    path = ROOT / "src/digitalmodel/base_configs/modules/aqwa/template_decks/D05266mm_06.deck"
    grid = AqwaDATFiles()._parse_deck6(path.read_text().splitlines())
    assert len(grid.frequencies) == 50
    assert grid.frequencies[0] == pytest.approx(0.1000, rel=0.0001)
    assert grid.periods[0] == pytest.approx(62.83, rel=0.0001)
    assert len(grid.directions_deg) == 9
    assert grid.directions_deg == list(range(-180, 181, 45))


@pytest.mark.parametrize("card,value", [("HRTZ", 0.5 / (2*np.pi)), ("FREQ", 0.5), ("PERD", 4*np.pi)])
def test_deck6_units(card, value):
    grid = AqwaDATFiles()._parse_deck6([f"      {card}    1    1{value:10.6f}"])
    assert grid.frequencies[0] == pytest.approx(0.5, rel=1e-5)


def test_unknown_deck6_card():
    with pytest.raises(ValueError, match="card"):
        AqwaDATFiles()._parse_deck6(["      JUNK    1    1       1.0"])


def test_absent_deck4_is_absent():
    assert AqwaDATFiles()._parse_deck4([]) == {}


def test_writer_cog_roundtrip_and_validators(tmp_path):
    parser = AqwaDATFiles()
    line = parser.get_dc_01_body({}, {"data": {"raw": [
        {"structure": 1, "node": 98000, "X": 1.25, "Y": -2., "Z": 3.}
    ]}})[0]
    parsed = parser._parse_deck1([line])
    assert parsed.cog_element == 98000
    assert parsed.cog_coordinates == (1.25, -2., 3.)
    dat = tmp_path / "input.dat"
    dat.write_text("DECK 0\nOPTIONS LDRG\nDECK 1\n" + line)
    parser.validate_ldrg_option(str(dat))
    parser.validate_cog_element(str(dat))
    with pytest.raises(ValueError, match="CoG"):
        parser.validate_cog_element(str(dat), expected_element=97000)
    dat.write_text("DECK 1\n" + line)
    with pytest.raises(ValueError, match="LDRG"):
        parser.validate_ldrg_option(str(dat))


def test_roll_and_pitch_arms_include_vertical_offset():
    from digitalmodel.hydrodynamics.aqwa.aqwa_dat_files import DATDeckOneMetadata
    from types import SimpleNamespace
    inspection = SimpleNamespace(deck_one=DATDeckOneMetadata(98000, (0, 0, 0), []))
    result = ViscousDampingDetermination()._build_drag_properties(
        [{"centroid": (3, 4, 12), "cd_normal": 1., "area": 10}], inspection)
    assert result["roll"][0]["lever_arm"] == pytest.approx(np.sqrt(160))
    assert result["pitch"][0]["lever_arm"] == pytest.approx(np.sqrt(153))
