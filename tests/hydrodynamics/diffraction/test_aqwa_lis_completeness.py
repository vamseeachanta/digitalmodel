# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent synthetic completeness oracles for issue #2107.

Input definition is in generate_aqwa_lis.py. Hand count: indices 1..33
contain 33 rows; deleting/adding one gives 32/34. The final added-mass
M11 is 33*1=33, damping C11 is -33*1=-33. No solver accuracy is asserted.
"""

from pathlib import Path
import runpy

import pytest

from digitalmodel.hydrodynamics.diffraction.aqwa_lis_parser import AQWALISParser

GENERATOR = runpy.run_path(str(Path(__file__).resolve().parents[3]
                               / "scripts/testing/generate_aqwa_lis.py"))


def test_complete_grid_survives_page_headers(tmp_path):
    """A 33-row table with 20 page-header lines after row 10 stays complete."""
    path = GENERATOR["write_pair"](tmp_path, gap=20)
    parser = AQWALISParser(path)
    frequencies, periods = parser.extract_frequencies_and_periods()
    assert len(frequencies) == len(periods) == 33
    assert frequencies == [i / 10 for i in range(1, 34)]
    assert len(parser.parse_added_mass_table()) == 33
    assert len(parser.parse_damping_table()) == 33
    assert parser.parse_added_mass_table()[3.3][0, 0] == 33
    assert parser.parse_damping_table()[3.3][0, 0] == -33


@pytest.mark.parametrize("method,field", [
    ("extract_frequencies_and_periods", "added_rows"),
    ("parse_added_mass_table", "added_rows"),
    ("parse_damping_table", "damping_rows"),
])
@pytest.mark.parametrize("rows", [32, 34])
def test_declared_count_mismatch_refuses(tmp_path, method, field, rows):
    path = GENERATOR["write_pair"](tmp_path, **{field: rows})
    with pytest.raises(ValueError, match="count_mismatch") as caught:
        getattr(AQWALISParser(path), method)()
    assert caught.value.expected == 33
    assert caught.value.actual == rows


def test_missing_terminator_refuses(tmp_path):
    text, deck = GENERATOR["listing"]()
    path = tmp_path / "synthetic.LIS"
    # Keep only the added-mass section, deliberately without its next header.
    path.write_text(text.split(GENERATOR["DAMPING"])[0])
    path.with_suffix(".dat").write_text(deck)
    with pytest.raises(ValueError, match="missing_terminator"):
        AQWALISParser(path).parse_added_mass_table()


def test_headings_beyond_old_window(tmp_path):
    path = GENERATOR["write_pair"](tmp_path, row_padding=2000)
    assert AQWALISParser(path).extract_headings() == [0, 90]


def test_last_rao_section_beyond_old_window(tmp_path):
    path = GENERATOR["write_pair"](tmp_path, row_padding=2000)
    raos = AQWALISParser(path).parse_rao_table()
    assert len(raos) == 33 * 2
    assert (3.3, 90) in raos


@pytest.mark.parametrize("table", ["added_mass", "damping"])
def test_converter_refuses_missing_matrix(tmp_path, monkeypatch, table):
    """Hand deletion leaves 32 matrices for an explicitly declared 33 rows."""
    from digitalmodel.hydrodynamics.diffraction import aqwa_converter

    path = GENERATOR["write_pair"](tmp_path)
    data = AQWALISParser(path).parse_all()
    data[table].pop(3.3)
    monkeypatch.setattr(aqwa_converter, "parse_aqwa_lis_file", lambda _: data)
    converter = aqwa_converter.AQWAConverter(tmp_path, "synthetic")
    with pytest.raises(ValueError, match="count_mismatch"):
        getattr(converter, f"_extract_{table}_data")()


@pytest.mark.parametrize("comparison", ["pair", "multiple"])
def test_comparison_refuses_declared_short_grid(mock_diffraction_results, comparison):
    """A declared N+1 cannot be reconciled with the N stored rows."""
    from copy import deepcopy
    from digitalmodel.hydrodynamics.diffraction.comparison_framework import DiffractionComparator
    from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import MultiSolverComparator

    result = deepcopy(mock_diffraction_results)
    result.declared_frequency_count = len(result.added_mass.frequencies.values) + 1
    with pytest.raises(ValueError, match="count_mismatch"):
        if comparison == "pair":
            DiffractionComparator(result, mock_diffraction_results)
        else:
            MultiSolverComparator({"first": result, "second": mock_diffraction_results})


def test_declared_count_survives_result_serialization(mock_diffraction_results):
    from digitalmodel.hydrodynamics.diffraction.output_schemas import DiffractionResults

    result = mock_diffraction_results
    result.declared_frequency_count = len(result.added_mass.frequencies.values)
    rebuilt = DiffractionResults.from_dict(result.to_dict())
    assert rebuilt.declared_frequency_count == result.declared_frequency_count


@pytest.mark.parametrize("method,next_marker", [
    ("extract_frequencies_and_periods", "DAMPING"),
    ("parse_damping_table", "RAO"),
    ("parse_rao_table", "END"),
])
def test_each_reader_requires_terminator(tmp_path, method, next_marker):
    path = GENERATOR["write_pair"](tmp_path)
    path.write_text(path.read_text().split(GENERATOR[next_marker])[0])
    with pytest.raises(ValueError, match="missing_terminator"):
        getattr(AQWALISParser(path), method)()


def test_repeated_title_is_a_page_header(tmp_path):
    path = GENERATOR["write_pair"](tmp_path, gap=20)
    text = path.read_text().replace(" PAGE HEADER\n" * 20,
                                   GENERATOR["ADDED"] + "\n", 1)
    path.write_text(text)
    assert len(AQWALISParser(path).parse_added_mass_table()) == 33


def test_different_title_ends_frequency_table(tmp_path):
    path = GENERATOR["write_pair"](tmp_path)
    added = AQWALISParser(path).parse_added_mass_table()
    assert all(matrix[0, 0] > 0 for matrix in added.values())


def test_duplicate_frequency_is_not_deduplicated(tmp_path):
    path = GENERATOR["write_pair"](tmp_path)
    lines = path.read_text().splitlines()
    lines.insert(3, lines[2])
    path.write_text("\n".join(lines) + "\n")
    with pytest.raises(ValueError, match="duplicate_frequency"):
        AQWALISParser(path).extract_frequencies_and_periods()


def test_invalid_coefficient_row_refuses(tmp_path):
    path = GENERATOR["write_pair"](tmp_path)
    lines = path.read_text().splitlines()
    row = lines[2].split()
    row[2] = "invalid"
    lines[2] = " " + " ".join(row)
    path.write_text("\n".join(lines) + "\n")
    with pytest.raises(ValueError, match="malformed_row"):
        AQWALISParser(path).parse_added_mass_table()


def test_equal_counts_with_different_keys_refuse(tmp_path):
    path = GENERATOR["write_pair"](tmp_path)
    first, damping = path.read_text().split(GENERATOR["DAMPING"], 1)
    path.write_text(first + GENERATOR["DAMPING"] + damping.replace("0.100", "9.900", 1))
    with pytest.raises(ValueError, match="frequency_keys_mismatch"):
        AQWALISParser(path).parse_all()


def test_standalone_listing_has_no_independent_declaration(tmp_path):
    path = tmp_path / "standalone.LIS"
    path.write_text(GENERATOR["listing"]()[0])
    result = AQWALISParser(path).parse_all()
    assert result["declared_frequency_count"] is None
    assert result["declaration_status"] == "NOT_EVALUATED"
    assert AQWALISParser(path, expected_frequency_count=33).parse_all()[
        "declaration_status"] == "CHECKED"
    with pytest.raises(ValueError, match="count_mismatch"):
        AQWALISParser(path, expected_frequency_count=34).parse_all()


def test_echoed_deck_counts_are_used(tmp_path):
    text, deck = GENERATOR["listing"]()
    path = tmp_path / "echo.LIS"
    path.write_text(deck + text)
    assert AQWALISParser(path).parse_all()["declared_frequency_count"] == 33


def test_unsupported_deck_range_refuses(tmp_path):
    path = GENERATOR["write_pair"](tmp_path)
    deck = path.with_suffix(".dat")
    lines = deck.read_text().splitlines()
    # Change the last index field on the first card from 1 to 33. Range
    # interpolation is not implemented by this listing reader.
    lines[1] = lines[1][:15] + f'{33:5d}' + lines[1][20:]
    deck.write_text("\n".join(lines) + "\n")
    with pytest.raises(ValueError, match="unsupported_frequency_range"):
        AQWALISParser(path)


def test_converter_retains_declared_count(tmp_path):
    from digitalmodel.hydrodynamics.diffraction.aqwa_converter import AQWAConverter

    GENERATOR["write_pair"](tmp_path)
    # Water depth is a stipulated metadata input; no wave-physics claim uses it.
    result = AQWAConverter(tmp_path, "synthetic").convert_to_unified_schema(water_depth=1)
    assert result.declared_frequency_count == 33
    assert len(result.added_mass.matrices) == len(result.damping.matrices) == 33
