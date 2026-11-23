"""Frequency extraction from an AQWA .LIS listing.

The extractor filtered rows through hard-coded bounds chosen for a typical
seakeeping grid. Three consequences, all silent:

  ``0.1 < freq`` excludes exactly 0.100 rad/s, which is a common choice for the
  lowest frequency in a diffraction run precisely because it is a round number.
  The shipped ship-RAO example uses it, and loses it.

  ``period < 100`` excludes anything slower than 100 s, which a low-frequency
  or shallow-water study will reach.

  The table was read from a fixed 3000-character window, so a grid with enough
  frequencies is truncated rather than reported short.

A frequency list that silently disagrees with the coefficients parsed beside it
is worse than a parse failure, because the caller builds a grid from it.
"""

from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.diffraction.aqwa_lis_parser import (
    AQWALISParser,
    parse_aqwa_lis_file,
)

REPO = Path(__file__).resolve().parents[3]
SHIP_LIS = (REPO / "docs" / "domains" / "aqwa" / "examples" / "03_dat"
            / "001_ship_raos" / "001_SHIP_RAOS.LIS")

pytestmark = pytest.mark.skipif(
    not SHIP_LIS.exists(), reason="ship RAO example listing not present")


def test_lowest_frequency_is_not_dropped():
    """0.100 rad/s is in the listing and must survive extraction."""
    result = parse_aqwa_lis_file(str(SHIP_LIS))
    freqs = result["frequencies"]
    assert pytest.approx(0.1, abs=1e-9) in freqs, (
        f"0.100 rad/s missing; got {freqs}")


def test_frequency_list_matches_the_parsed_coefficients():
    """The grid the caller builds must match the data it indexes."""
    result = parse_aqwa_lis_file(str(SHIP_LIS))
    freqs = sorted(result["frequencies"])
    from_added_mass = sorted(result["added_mass"])
    assert freqs == pytest.approx(from_added_mass), (
        f"frequency list {freqs} disagrees with added-mass keys "
        f"{from_added_mass}")


def test_frequency_list_matches_the_rao_table():
    result = parse_aqwa_lis_file(str(SHIP_LIS))
    freqs = sorted(result["frequencies"])
    from_raos = sorted({f for f, _ in result["raos"]})
    assert freqs == pytest.approx(from_raos)


def test_expected_grid_for_the_shipped_example():
    """Pins the eight frequencies the deck actually requests."""
    result = parse_aqwa_lis_file(str(SHIP_LIS))
    assert sorted(result["frequencies"]) == pytest.approx(
        [0.1, 0.407, 0.715, 1.022, 1.33, 1.637, 1.945, 2.252], abs=1e-3)
    assert len(result["periods"]) == len(result["frequencies"])


def test_periods_and_frequencies_are_consistent():
    """Each period must be 2 pi over its frequency."""
    import math

    result = parse_aqwa_lis_file(str(SHIP_LIS))
    pairs = sorted(zip(result["frequencies"], result["periods"]))
    for freq, period in pairs:
        assert period == pytest.approx(2 * math.pi / freq, rel=2e-3), (
            f"period {period} does not match frequency {freq}")


def test_a_long_frequency_grid_is_not_truncated():
    """A grid too long for the old fixed window must still come back whole."""
    rows = []
    n = 40
    for i in range(n):
        freq = 0.1 + 0.05 * i
        period = 2 * 3.141592653589793 / freq
        vals = "  ".join(f"{1.0e6 * (j + 1):.4E}" for j in range(12))
        rows.append(f"  {period:7.2f} {freq:7.3f}  {vals}")
    text = ("ADDED MASS-VARIATION WITH WAVE PERIOD/FREQUENCY\n"
            "  PERIOD   FREQ\n" + "\n".join(rows) + "\n")
    parser = AQWALISParser.__new__(AQWALISParser)
    parser.content = text
    freqs, periods = parser.extract_frequencies_and_periods()
    assert len(freqs) == n, f"expected {n} frequencies, parsed {len(freqs)}"
    assert len(periods) == n


def test_a_very_long_period_is_kept():
    """A 150 s period is physical in shallow water and was being discarded."""
    vals = "  ".join(f"{1.0e6 * (j + 1):.4E}" for j in range(12))
    text = ("ADDED MASS-VARIATION WITH WAVE PERIOD/FREQUENCY\n"
            "  PERIOD   FREQ\n"
            f"   150.00   0.042  {vals}\n"
            f"    62.83   0.100  {vals}\n")
    parser = AQWALISParser.__new__(AQWALISParser)
    parser.content = text
    freqs, periods = parser.extract_frequencies_and_periods()
    assert pytest.approx(0.042, abs=1e-9) in freqs
    assert pytest.approx(150.0, abs=1e-9) in periods
