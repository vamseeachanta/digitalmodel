# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Ordinal parser probes for issue #2107; no physical accuracy is implied.

Independent hand calculation specified in #2107: omega_j=(100+j)/1000,
for j=0..255, headings=(-90, 0, 90), amplitude=10*j+k+1 for the six
zero-based DOFs, phase=0, period=2*pi/omega. Thus 256*3=768 keys and
the final yaw amplitude is 10*255+5+1=2556. Decimal arithmetic fixes the
printed frequency keys before any parser call. The old extent limits were
10000 characters for headings and 50000 characters for the final RAO table.
"""

from decimal import Decimal
from math import pi
from pathlib import PureWindowsPath

import pytest

from digitalmodel.hydrodynamics.diffraction.aqwa_lis_parser import AQWALISParser

FREQUENCIES = tuple((Decimal(100) + j) / Decimal(1000) for j in range(256))
HEADINGS = (-90, 0, 90)
DOFS = ("surge", "sway", "heave", "roll", "pitch", "yaw")
RAO_MARKER = "R.A.O.S-VARIATION WITH WAVE PERIOD/FREQUENCY"
MASS_MARKER = "ADDED MASS-VARIATION WITH WAVE PERIOD/FREQUENCY"
DAMPING_MARKER = "DAMPING-VARIATION WITH WAVE PERIOD/FREQUENCY"


def _rao_row(j, heading=None, *, other=False, frequency=None):
    """The offset for non-displacement rows makes leakage observable."""
    frequency = FREQUENCIES[j] if frequency is None else frequency
    direction = "" if heading is None else str(heading)
    # Negate the ordinal phase and offset amplitude by the grid length for
    # neighbouring tables, independently of the displacement values above.
    pairs = " ".join(
        f"{10 * j + k + 1 + (len(FREQUENCIES) if other else 0):12.4f} "
        f"{-j if other else 0:10.4f}" for k in range(len(DOFS))
    )
    return f"  {2 * pi / float(frequency):12.6f} {frequency:.3f} {direction:>8} {pairs}"


def _displacement(headings=HEADINGS, *, pages=False, omit=None):
    lines = [RAO_MARKER, "  PERIOD FREQUENCY DIRECTION AMPLITUDE PHASE"]
    for heading in headings:
        for j in range(len(FREQUENCIES)):
            if (j, heading) == omit:
                continue
            # Mid-group page break: repeated title and column headers, with
            # more blank lines than the former frequency-table blank heuristic.
            if pages and j == len(FREQUENCIES) // 2:
                lines.extend(["\n" * len(DOFS), "1", "H Y D R O D Y N A M I C", RAO_MARKER,
                              "  PERIOD FREQUENCY DIRECTION AMPLITUDE PHASE"])
            lines.append(_rao_row(j, heading if j == 0 else None))
    return "\n".join(lines)


def _coefficients(marker, *, omit=None, extra=False, pages=False,
                  page_banner=False, print_control=False, scale=1):
    lines = [marker, "  PERIOD FREQUENCY COEFFICIENTS"]
    # Twelve entries correspond to the parser's documented matrix layout.
    values = " ".join(f"{scale * (k + 1):.4E}" for k in range(2 * len(DOFS)))
    frequencies = list(FREQUENCIES)
    if extra:
        frequencies.append(FREQUENCIES[-1] + Decimal("0.001"))
    for j, frequency in enumerate(frequencies):
        if j == omit:
            continue
        if pages and j == len(FREQUENCIES) // 2:
            lines.extend(["\n" * (2 * len(DOFS) + 1), "1"])
            if page_banner:
                lines.append("H Y D R O D Y N A M I C")
            lines.extend([("1" if print_control else "") + marker,
                          "  PERIOD FREQUENCY COEFFICIENTS"])
        lines.append(f"  {2 * pi / float(frequency):12.6f} {frequency:.3f} {values}")
    return "\n".join(lines)


def _parser(tmp_path, text):
    path = tmp_path / "ordinal.LIS"
    path.write_text(text, encoding="utf-8", newline="\n")
    return AQWALISParser(path)


def _expected():
    """Snapshot the independently declared Cartesian grid before parsing."""
    return frozenset((float(frequency), heading)
                     for frequency in FREQUENCIES for heading in HEADINGS)


def _assert_all_records(actual, expected):
    assert set(actual) == expected
    for j, frequency in enumerate(FREQUENCIES):
        for heading in HEADINGS:
            row = actual[float(frequency), heading]
            assert set(row) == set(DOFS)
            for k, dof in enumerate(DOFS):
                assert row[dof] == (10 * j + k + 1, 0)
    assert actual[float(FREQUENCIES[-1]), HEADINGS[-1]]["yaw"] == (2556, 0)


def test_final_displacement_table_exceeds_old_cap_without_truncation(tmp_path):
    expected = _expected()
    assert len(expected) == 768
    text = _displacement()
    assert len(text) > 50000
    parser = _parser(tmp_path, text)  # Single table, end of input.
    _assert_all_records(parser.parse_rao_table(), expected)


def test_late_heading_groups_survive_extraction(tmp_path):
    expected = _expected()
    text = _displacement()
    for heading in HEADINGS[1:]:
        assert text.index(_rao_row(0, heading)) > 10000
    parser = _parser(tmp_path, text)
    assert parser.extract_headings() == list(HEADINGS)
    _assert_all_records(parser.parse_rao_table(), expected)


@pytest.mark.parametrize("pages", [False, True])
@pytest.mark.parametrize("multiple", [False, True])
def test_structural_boundaries_exclude_velocity_and_acceleration(tmp_path, pages, multiple):
    expected = _expected()
    groups = [(heading,) for heading in HEADINGS] if multiple else [HEADINGS]
    parts = []
    for group in groups:
        parts.append(_displacement(group, pages=pages))
        for prefix in ("VEL ", "ACC "):
            parts.append(prefix + RAO_MARKER)
            for heading in group:
                parts.extend(_rao_row(j, heading if j == 0 else None, other=True)
                             for j in range(len(FREQUENCIES)))
            # An additional non-displacement group at the midpoint between two
            # declared headings must not become an observed displacement heading.
            parts.append(_rao_row(0, sum(HEADINGS[:2]) / 2, other=True))
    parser = _parser(tmp_path, "\n".join(parts))
    assert parser.extract_headings() == list(HEADINGS)
    _assert_all_records(parser.parse_rao_table(), expected)


@pytest.mark.parametrize("boundary", [DAMPING_MARKER,
                                      "R.A.O.S-VARIATION WITH WAVE DIRECTION",
                                      "H Y D R O D Y N A M I C",
                                      "S T A B I L I T Y", "M O O R I N G"])
def test_explicit_next_section_ends_displacement_table(tmp_path, boundary):
    expected = _expected()
    text = _displacement() + "\n" + boundary + "\n" + _rao_row(255, 90, other=True)
    parser = _parser(tmp_path, text)
    _assert_all_records(parser.parse_rao_table(), expected)


@pytest.mark.parametrize("defect, diagnostic", [
    (None, None),
    ("missing-rao", "displacement RAO grid.*missing"),
    ("surplus-rao", "displacement RAO grid.*surplus"),
    ("missing-damping", "damping table.*missing"),
    ("surplus-damping", "damping table.*surplus"),
    ("added-mass-grid", "added mass table.*missing"),
    ("surplus-added-mass", "added mass table.*surplus"),
    ("substituted-damping", "damping table.*missing.*surplus"),
])
def test_parse_all_refuses_incomplete_or_surplus_grid_membership(
    tmp_path, monkeypatch, defect, diagnostic,
):
    expected = _expected()
    damping_text = _coefficients(
        DAMPING_MARKER, omit=255 if defect == "missing-damping" else None,
        extra=defect == "surplus-damping",
    )
    if defect == "substituted-damping":
        # Same cardinality, different membership: the next ordinal frequency
        # is 356/1000 by the independent frequency rule in the module docstring.
        damping_text = damping_text.replace(" 0.355 ", " 0.356 ")
    text = "\n".join([
        _coefficients(MASS_MARKER),
        damping_text,
        _displacement(omit=(255, 90) if defect == "missing-rao" else None),
    ])
    if defect == "surplus-rao":
        text += "\n" + _rao_row(255, 90, frequency=Decimal("0.356"))
    parser = _parser(tmp_path, text)
    if defect in {"added-mass-grid", "surplus-added-mass"}:
        # Exercise the membership contract separately: frequency extraction and
        # added mass otherwise consume the very same source rows.
        original = parser.parse_added_mass_table

        def incomplete_added_mass():
            result = original()
            if defect == "added-mass-grid":
                result.pop(float(FREQUENCIES[-1]))
            else:
                result[float(Decimal("0.356"))] = result[float(FREQUENCIES[-1])]
            return result

        monkeypatch.setattr(parser, "parse_added_mass_table", incomplete_added_mass)
    if diagnostic:
        with pytest.raises(ValueError, match=diagnostic) as error:
            parser.parse_all()
        assert "0.355" in str(error.value) or "0.356" in str(error.value)
    else:
        result = parser.parse_all()
        assert result["frequencies"] == [float(f) for f in FREQUENCIES]
        assert result["headings"] == list(HEADINGS)
        _assert_all_records(result["raos"], expected)


def test_parse_all_retains_blank_and_page_headers_in_frequency_table(tmp_path):
    expected = _expected()
    parser = _parser(tmp_path, "\n".join([
        _coefficients(MASS_MARKER, pages=True),
        _coefficients(DAMPING_MARKER, pages=True),
        _displacement(pages=True),
    ]))
    result = parser.parse_all()
    assert result["frequencies"] == [float(f) for f in FREQUENCIES]
    _assert_all_records(result["raos"], expected)


@pytest.mark.parametrize("page_banner", [False, True])
@pytest.mark.parametrize("print_control", [False, True])
def test_coefficient_title_continuations_retain_full_grid(
    tmp_path, page_banner, print_control,
):
    expected = _expected()
    frequencies = [float(f) for f in FREQUENCIES]
    parser = _parser(tmp_path, "\n".join([
        _coefficients(MASS_MARKER, pages=True, page_banner=page_banner,
                      print_control=print_control),
        _coefficients(DAMPING_MARKER, pages=True, page_banner=page_banner,
                      print_control=print_control, scale=100),
        _displacement(),
    ]))
    actual_frequencies, periods = parser.extract_frequencies_and_periods()
    assert actual_frequencies == frequencies
    assert len(actual_frequencies) == len(periods) == 256
    added = parser.parse_added_mass_table()
    damping = parser.parse_damping_table()
    assert set(added) == set(damping) == set(frequencies)
    assert len(added) == len(damping) == 256
    # Hand calculation: coefficient k is scale*(k+1); the six diagonal
    # entries are 1..6 for added mass and 100*(1..6) for damping. Reading
    # through a different table would overwrite the shared frequency keys.
    for frequency in frequencies:
        assert added[frequency].diagonal() == pytest.approx((1, 2, 3, 4, 5, 6))
        assert damping[frequency].diagonal() == pytest.approx(
            (100, 200, 300, 400, 500, 600),
        )
    result = parser.parse_all()
    assert result["frequencies"] == frequencies
    _assert_all_records(result["raos"], expected)


@pytest.mark.parametrize("marker", [MASS_MARKER, DAMPING_MARKER])
def test_coefficient_page_banner_requires_matching_continuation(tmp_path, marker):
    # A banner followed by a data-like row, rather than this table's title,
    # ends the table. The different scale exposes any overwritten row.
    text = _coefficients(marker) + "\nH Y D R O D Y N A M I C\n\n1\n"
    text += _coefficients(marker, scale=100).splitlines()[-1]
    parser = _parser(tmp_path, text)
    actual = (parser.parse_added_mass_table() if marker == MASS_MARKER
              else parser.parse_damping_table())
    assert len(actual) == 256
    assert actual[0.355][0, 0] == 1


def test_missing_listing_diagnostic_uses_forward_slashes(monkeypatch):
    class MissingWindowsPath(PureWindowsPath):
        def exists(self):
            return False

    monkeypatch.setattr(
        "digitalmodel.hydrodynamics.diffraction.aqwa_lis_parser.Path",
        MissingWindowsPath,
    )
    with pytest.raises(FileNotFoundError, match="missing/ordinal[.]LIS"):
        AQWALISParser(MissingWindowsPath("missing") / "ordinal.LIS")


def test_print_control_and_preceding_acceleration_text_do_not_hide_displacement(tmp_path):
    expected = _expected()
    # The listing print-control character is 1; ACC belongs to the preceding
    # line here. Classification must use the title line, not nearby characters.
    parser = _parser(tmp_path, "ACC\n1" + _displacement())
    _assert_all_records(parser.parse_rao_table(), expected)


def test_unobserved_entire_heading_is_an_explicit_contract_limit(tmp_path):
    # No independent deck declaration enters this API: removing a whole group
    # leaves a complete Cartesian grid over the remaining observed headings.
    expected = frozenset((float(f), h) for f in FREQUENCIES for h in HEADINGS[:-1])
    parser = _parser(tmp_path, "\n".join([
        _coefficients(MASS_MARKER), _coefficients(DAMPING_MARKER),
        _displacement(HEADINGS[:-1]),
    ]))
    result = parser.parse_all()
    assert result["headings"] == list(HEADINGS[:-1])
    assert set(result["raos"]) == expected
