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


def test_deck6_keeps_structure_grids_separate():
    parser = AqwaDATFiles()
    lines = ["FDR1", "      FREQ    1    2       0.2       0.4",
             "      DIRN    1    1     -90.0", "END", "FDR2",
             "      FREQ    1    1       0.7", "      DIRN    1    1      45.0", "END"]
    assert parser._parse_deck6(lines).frequencies == [0.2, 0.4]
    assert parser._parse_deck6(lines, structure=2).frequencies == [0.7]
    assert parser._parse_deck6(lines, structure=2).directions_deg == [45.0]
    with pytest.raises(ValueError, match="structure"):
        parser._parse_deck6(lines, structure=3)


def test_committed_deck6_examples_and_validators():
    import re
    import subprocess

    parser = AqwaDATFiles()
    paths = subprocess.check_output(["git", "ls-files"], cwd=ROOT, text=True).splitlines()
    count = 0
    for relative in paths:
        path = ROOT / relative
        if path.suffix.lower() != ".dat" or "aqwa" not in relative:
            continue
        if not re.search(r"DECK\s+6\b", path.read_text(errors="replace"), re.I):
            continue
        count += 1
        decks = parser._read_decks_from_file(str(path))
        grid = parser._parse_deck6(decks[6])
        assert all(value > 0 for value in grid.frequencies)
        # These validators must not fail on unrelated deck-6 syntax.
        for validate in (parser.validate_ldrg_option, parser.validate_cog_element):
            try:
                validate(str(path))
            except ValueError as exc:
                assert "LDRG" in str(exc) or "CoG" in str(exc)
    assert count == 50
