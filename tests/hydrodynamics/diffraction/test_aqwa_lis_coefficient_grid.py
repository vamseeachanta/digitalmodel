"""The coefficient tables must cover the same frequencies as the frequency list.

Only `extract_frequencies_and_periods` was de-truncated when the fixed-window
defect was first addressed. `parse_added_mass_table` and `parse_damping_table`
kept a 2,000-character window, which holds about twelve rows, so `parse_all`
returned a long frequency grid beside short coefficient grids and the
downstream converter padded the difference with zero matrices -- a hydrodynamic
database reporting zero added mass and zero radiation damping above the twelfth
frequency, returned as a complete result.

Measured on a listing shipped in this repository: 43 frequencies against 10
coefficient matrices.
"""

from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.diffraction.aqwa_lis_parser import (
    AQWALISParser,
)

REPO = Path(__file__).resolve().parents[3]
MULTIBODY = (REPO / "docs" / "domains" / "orcaflex" / "aqwa" / "to_orcaflex"
             / "multibody_test1" / "input"
             / "AL_FST2F_FST1F_HWL_ANS2019R1.LIS")
SHIP = (REPO / "docs" / "domains" / "aqwa" / "examples" / "03_dat"
        / "001_ship_raos" / "001_SHIP_RAOS.LIS")

multibody_only = pytest.mark.skipif(
    not MULTIBODY.exists(), reason="multibody listing absent")
ship_only = pytest.mark.skipif(not SHIP.exists(), reason="ship listing absent")


def _synthetic(tmp_path, rows: int):
    """A listing with `rows` frequencies in both coefficient tables."""

    def table(marker, scale):
        out = [f"1{marker}", "", "  PERIOD  FREQUENCY" + "  VALUE" * 12,
               "  " + "-" * 110]
        for i in range(rows):
            freq = 0.10 + 0.05 * i
            period = 2.0 * 3.141592653589793 / freq
            vals = "".join(f" {scale * (i + 1) * (j + 1):9.3E}"
                           for j in range(12))
            out.append(f"  {period:8.2f} {freq:8.4f}{vals}")
        out.append("")
        out.append("  some trailing prose that is not a data row")
        return "\n".join(out)

    text = "\n".join([
        " " * 10 + "A Q W A   L I S T I N G",
        "",
        table("ADDED MASS-VARIATION WITH WAVE PERIOD/FREQUENCY", 1.0e5),
        "",
        table("DAMPING-VARIATION WITH WAVE PERIOD/FREQUENCY", 1.0e3),
        "",
    ])
    p = tmp_path / "synthetic.LIS"
    p.write_text(text, encoding="ascii")
    return p


class TestLongGrids:
    @pytest.mark.parametrize("rows", [8, 13, 40, 60])
    def test_both_tables_return_every_row(self, tmp_path, rows):
        """Twelve rows is roughly what the old 2,000-character window held."""
        p = _synthetic(tmp_path, rows)
        parser = AQWALISParser(str(p))
        added = parser.parse_added_mass_table()
        damping = parser.parse_damping_table()
        assert len(added) == rows
        assert len(damping) == rows

    def test_the_last_row_is_present_and_not_zero(self, tmp_path):
        """A truncated table pads with zeros, which must not read as data."""
        p = _synthetic(tmp_path, 40)
        parser = AQWALISParser(str(p))
        added = parser.parse_added_mass_table()
        last = max(added)
        assert last == pytest.approx(0.10 + 0.05 * 39)
        assert added[last][0, 0] != 0.0

    def test_the_two_tables_are_not_read_into_one_another(self, tmp_path):
        """Added mass and damping are scaled differently in the fixture.

        A walk that runs past the end of its own table would pick up the
        neighbouring one, and the frequencies coincide so a key comparison
        would not notice. The values do.
        """
        p = _synthetic(tmp_path, 40)
        parser = AQWALISParser(str(p))
        added = parser.parse_added_mass_table()
        damping = parser.parse_damping_table()
        f = min(added)
        assert added[f][0, 0] == pytest.approx(1.0e5)
        assert damping[f][0, 0] == pytest.approx(1.0e3)


class TestAgainstShippedListings:
    @multibody_only
    def test_the_grids_agree_on_the_multibody_listing(self):
        """The case that exposed it: 43 frequencies against 10 matrices."""
        parser = AQWALISParser(str(MULTIBODY))
        freqs, _periods = parser.extract_frequencies_and_periods()
        added = parser.parse_added_mass_table()
        damping = parser.parse_damping_table()
        assert len(freqs) > 12, "fixture no longer exercises the defect"
        assert len(added) == len(freqs), (
            f"{len(freqs)} frequencies but {len(added)} added-mass matrices")
        assert len(damping) == len(freqs), (
            f"{len(freqs)} frequencies but {len(damping)} damping matrices")

    @ship_only
    def test_the_grids_agree_on_the_ship_listing(self):
        parser = AQWALISParser(str(SHIP))
        freqs, _periods = parser.extract_frequencies_and_periods()
        added = parser.parse_added_mass_table()
        assert len(added) == len(freqs)


class TestParseAllIsSelfConsistent:
    @multibody_only
    def test_parse_all_reports_a_grid_mismatch_rather_than_padding(self):
        """`parse_all` must not hand on grids that disagree without saying so."""
        parser = AQWALISParser(str(MULTIBODY))
        result = parser.parse_all()
        freqs = result.get("frequencies")
        added = result.get("added_mass")
        damping = result.get("damping")
        if freqs is None or added is None:
            pytest.skip("parse_all does not expose these keys")
        assert len(added) == len(freqs)
        if damping is not None:
            assert len(damping) == len(freqs)
