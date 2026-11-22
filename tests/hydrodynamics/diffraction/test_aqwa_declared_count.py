# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check listing counts against independent synthetic deck declarations."""

from decimal import Decimal
from math import pi

import pytest

from digitalmodel.hydrodynamics.diffraction.aqwa_lis_parser import AQWALISParser


def _listing(tmp_path, count):
    """Print consistent tables; the caller separately supplies a declaration."""
    rows = [(Decimal(100) + j) / Decimal(1000) for j in range(count)]
    sections = []
    coefficients = " ".join(f"{k + 1:.4E}" for k in range(12))
    for name in ("ADDED MASS", "DAMPING"):
        sections.append(f"{name}-VARIATION WITH WAVE PERIOD/FREQUENCY")
        sections.extend(
            f"  {2 * pi / float(frequency):.6f} {frequency:.3f} {coefficients}"
            for frequency in rows
        )
    sections.append("R.A.O.S-VARIATION WITH WAVE PERIOD/FREQUENCY")
    for heading in (-90, 0, 90):
        for j, frequency in enumerate(rows):
            pairs = " ".join(f"{10 * j + k + 1:.4f} 0.0000" for k in range(6))
            sections.append(
                f"  {2 * pi / float(frequency):.6f} {frequency:.3f} {heading} {pairs}"
            )
    path = tmp_path / "ordinal.LIS"
    path.write_text("\n".join(sections), encoding="utf-8")
    return AQWALISParser(path), [float(frequency) for frequency in rows]


@pytest.mark.parametrize("declared", [1, 256])
def test_independent_count_accepts_every_frequency_and_coefficient(tmp_path, declared):
    parser, frequencies = _listing(tmp_path, declared)
    result = parser.parse_all(expected_frequency_count=declared)
    assert result["frequencies"] == frequencies
    assert len(result["periods"]) == declared
    assert len(result["added_mass"]) == len(result["damping"]) == declared
    assert set(result["added_mass"]) == set(result["damping"]) == set(frequencies)
    assert set(result["raos"]) == {
        (frequency, heading) for frequency in frequencies for heading in (-90, 0, 90)
    }


@pytest.mark.parametrize("observed, declared", [(255, 256), (256, 255)])
def test_consistently_short_or_surplus_listing_raises(tmp_path, observed, declared):
    parser, _ = _listing(tmp_path, observed)
    with pytest.raises(
        ValueError,
        match=rf"declared frequency count.*expected {declared}.*observed {observed}",
    ):
        parser.parse_all(expected_frequency_count=declared)


@pytest.mark.parametrize("declaration", [0, -1, True, False, 256.0, 1.5, "256", [], {}])
def test_invalid_declaration_is_rejected_before_extracting_tables(tmp_path, declaration):
    path = tmp_path / "empty.LIS"
    path.write_text("", encoding="utf-8")
    parser = AQWALISParser(path)
    with pytest.raises(ValueError, match="expected_frequency_count.*positive integer"):
        parser.parse_all(expected_frequency_count=declaration)


def test_no_declaration_keeps_observation_only_return_contract(tmp_path):
    parser, frequencies = _listing(tmp_path, 255)
    for result in (parser.parse_all(), parser.parse_all(expected_frequency_count=None)):
        assert set(result) == {
            "frequencies", "periods", "headings", "added_mass", "damping", "raos",
        }
        assert result["frequencies"] == frequencies
        assert len(result["added_mass"]) == len(result["damping"]) == 255
        assert result["headings"] == [-90, 0, 90]
        assert len(result["raos"]) == 255 * 3


def test_declaration_is_keyword_only(tmp_path):
    parser, _ = _listing(tmp_path, 1)
    with pytest.raises(TypeError):
        parser.parse_all(1)
