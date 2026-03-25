"""Tests for AQWA backend Deck 7 FIDP/FISK card generation.

Validates that the AQWABackend.build_deck7() method correctly emits
Frequency Independent DamPing (FIDP) and Frequency Independent
StiFFness/sKiffness (FISK) cards from DiffractionSpec external_damping
and external_stiffness matrices.

Each FIDP/FISK card row follows the AQWA fixed-column format:
    <6 spaces><keyword><5 spaces><row_idx (5-wide)><6 x 10-char values>
"""

from __future__ import annotations

import re

import pytest

from digitalmodel.hydrodynamics.diffraction.aqwa_backend import AQWABackend
from digitalmodel.hydrodynamics.diffraction.input_schemas import (
    BodySpec,
    DiffractionSpec,
    EnvironmentSpec,
    FrequencySpec,
    HeadingRangeSpec,
    MetadataSpec,
    VesselGeometry,
    VesselInertia,
    VesselSpec,
    WaveHeadingSpec,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _zero_matrix() -> list[list[float]]:
    """Return a 6x6 matrix of all zeros."""
    return [[0.0] * 6 for _ in range(6)]


def _roll_damping_matrix(m44: float = 36010.0) -> list[list[float]]:
    """Return a 6x6 matrix with only M44 (roll-roll) non-zero."""
    matrix = _zero_matrix()
    matrix[3][3] = m44
    return matrix


def _full_damping_matrix() -> list[list[float]]:
    """Return a 6x6 matrix with distinct values for every element."""
    return [
        [100.0, 0.0, 0.0, 0.0, 0.0, 0.0],
        [0.0, 200.0, 0.0, 0.0, 0.0, 0.0],
        [0.0, 0.0, 300.0, 0.0, 0.0, 0.0],
        [0.0, 0.0, 0.0, 36010.0, 0.0, 0.0],
        [0.0, 0.0, 0.0, 0.0, 50000.0, 0.0],
        [0.0, 0.0, 0.0, 0.0, 0.0, 60000.0],
    ]


def _stiffness_matrix() -> list[list[float]]:
    """Return a 6x6 external stiffness matrix with non-zero diagonals."""
    return [
        [1e5, 0.0, 0.0, 0.0, 0.0, 0.0],
        [0.0, 1e5, 0.0, 0.0, 0.0, 0.0],
        [0.0, 0.0, 2e6, 0.0, 0.0, 0.0],
        [0.0, 0.0, 0.0, 5e7, 0.0, 0.0],
        [0.0, 0.0, 0.0, 0.0, 5e7, 0.0],
        [0.0, 0.0, 0.0, 0.0, 0.0, 1e7],
    ]


def _make_spec(
    external_damping: list[list[float]] | None = None,
    external_stiffness: list[list[float]] | None = None,
) -> DiffractionSpec:
    """Build a minimal DiffractionSpec with optional damping/stiffness."""
    vessel = VesselSpec(
        name="test_vessel",
        type="barge",
        geometry=VesselGeometry(
            mesh_file="test.gdf",
            length_units="m",
        ),
        inertia=VesselInertia(
            mass=10_000_000.0,
            centre_of_gravity=[0.0, 0.0, -2.5],
            radii_of_gyration=[10.0, 15.0, 15.0],
        ),
        external_damping=external_damping,
        external_stiffness=external_stiffness,
    )
    return DiffractionSpec(
        metadata=MetadataSpec(project="test_damping"),
        vessel=vessel,
        environment=EnvironmentSpec(water_depth=200.0),
        frequencies=FrequencySpec(
            values=[0.3, 0.6, 0.9],
        ),
        wave_headings=WaveHeadingSpec(
            range=HeadingRangeSpec(start=0.0, end=180.0, increment=45.0),
        ),
    )


def _extract_keyword_lines(
    cards: list[str], keyword: str
) -> list[str]:
    """Return lines from the card list that contain the given keyword."""
    return [line for line in cards if keyword in line]


# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------


class TestDeck7NoDamping:
    """Deck 7 behaviour when no external damping is defined."""

    def test_deck7_no_damping_is_empty_wfs1(self):
        """No external_damping produces WFS1 banner + END with no FIDP."""
        spec = _make_spec(external_damping=None)
        backend = AQWABackend()
        cards = backend.build_deck7(spec)

        card_text = "\n".join(cards)
        assert "WFS1" in card_text
        assert "END" in card_text
        assert "FIDP" not in card_text
        assert "FISK" not in card_text

    def test_deck7_zero_damping_no_fidp(self):
        """All-zero external_damping matrix produces no FIDP cards."""
        spec = _make_spec(external_damping=_zero_matrix())
        backend = AQWABackend()
        cards = backend.build_deck7(spec)

        fidp_lines = _extract_keyword_lines(cards, "FIDP")
        assert len(fidp_lines) == 0


class TestDeck7RollDamping:
    """Deck 7 FIDP cards for roll-only damping."""

    def test_deck7_roll_damping_emits_fidp(self):
        """M44=36010 produces exactly 6 FIDP card rows."""
        spec = _make_spec(external_damping=_roll_damping_matrix(36010.0))
        backend = AQWABackend()
        cards = backend.build_deck7(spec)

        fidp_lines = _extract_keyword_lines(cards, "FIDP")
        assert len(fidp_lines) == 6, (
            f"Expected 6 FIDP rows, got {len(fidp_lines)}"
        )

    def test_deck7_roll_damping_row4_value(self):
        """Row 4 of FIDP cards contains the M44=36010 value."""
        spec = _make_spec(external_damping=_roll_damping_matrix(36010.0))
        backend = AQWABackend()
        cards = backend.build_deck7(spec)

        fidp_lines = _extract_keyword_lines(cards, "FIDP")
        # Row 4 is index 3 (0-based) in the FIDP lines
        row4 = fidp_lines[3]
        assert "3.601e+04" in row4, (
            f"Expected 3.601e+04 in row 4, got: {row4}"
        )

    def test_deck7_roll_damping_other_rows_zero(self):
        """Rows 1-3, 5-6 of FIDP cards contain all zeros for roll-only."""
        spec = _make_spec(external_damping=_roll_damping_matrix(36010.0))
        backend = AQWABackend()
        cards = backend.build_deck7(spec)

        fidp_lines = _extract_keyword_lines(cards, "FIDP")
        for idx in [0, 1, 2, 4, 5]:
            row = fidp_lines[idx]
            # Extract the 6 value fields (everything after the row index)
            values_part = row.split(str(idx + 1))[-1]
            # All values should be 0.000e+00
            non_zero = [
                v for v in values_part.split()
                if float(v) != 0.0
            ]
            assert len(non_zero) == 0, (
                f"Row {idx + 1} has non-zero values: {non_zero}"
            )


class TestDeck7FullDampingMatrix:
    """Deck 7 FIDP cards for a full 6x6 damping matrix."""

    def test_deck7_full_damping_matrix_row_count(self):
        """Full 6x6 matrix emits exactly 6 FIDP rows."""
        spec = _make_spec(external_damping=_full_damping_matrix())
        backend = AQWABackend()
        cards = backend.build_deck7(spec)

        fidp_lines = _extract_keyword_lines(cards, "FIDP")
        assert len(fidp_lines) == 6

    def test_deck7_full_damping_matrix_diagonal_values(self):
        """Diagonal values are correctly placed in each FIDP row."""
        matrix = _full_damping_matrix()
        spec = _make_spec(external_damping=matrix)
        backend = AQWABackend()
        cards = backend.build_deck7(spec)

        fidp_lines = _extract_keyword_lines(cards, "FIDP")
        expected_diag = [100.0, 200.0, 300.0, 36010.0, 50000.0, 60000.0]
        for row_idx, (line, expected_val) in enumerate(
            zip(fidp_lines, expected_diag)
        ):
            # Parse the numeric values from the line
            # Values start after the keyword prefix and row number
            # Find all floating-point numbers (scientific notation)
            values = re.findall(r'[+-]?\d+\.\d+e[+-]\d+', line)
            assert len(values) == 6, (
                f"Row {row_idx + 1}: expected 6 values, found {len(values)}"
            )
            parsed_val = float(values[row_idx])
            assert abs(parsed_val - expected_val) / max(expected_val, 1e-10) < 0.01, (
                f"Row {row_idx + 1}: diagonal expected {expected_val}, "
                f"got {parsed_val}"
            )


class TestDeck7Stiffness:
    """Deck 7 FISK card generation from external_stiffness."""

    def test_deck7_no_stiffness_no_fisk(self):
        """No external_stiffness produces no FISK cards."""
        spec = _make_spec(external_stiffness=None)
        backend = AQWABackend()
        cards = backend.build_deck7(spec)

        fisk_lines = _extract_keyword_lines(cards, "FISK")
        assert len(fisk_lines) == 0

    def test_deck7_zero_stiffness_no_fisk(self):
        """All-zero external_stiffness produces no FISK cards."""
        spec = _make_spec(external_stiffness=_zero_matrix())
        backend = AQWABackend()
        cards = backend.build_deck7(spec)

        fisk_lines = _extract_keyword_lines(cards, "FISK")
        assert len(fisk_lines) == 0

    def test_deck7_stiffness_emits_fisk(self):
        """Non-zero external_stiffness produces exactly 6 FISK rows."""
        spec = _make_spec(external_stiffness=_stiffness_matrix())
        backend = AQWABackend()
        cards = backend.build_deck7(spec)

        fisk_lines = _extract_keyword_lines(cards, "FISK")
        assert len(fisk_lines) == 6

    def test_deck7_stiffness_values_correct(self):
        """FISK row 3 contains the heave stiffness 2e6."""
        matrix = _stiffness_matrix()
        spec = _make_spec(external_stiffness=matrix)
        backend = AQWABackend()
        cards = backend.build_deck7(spec)

        fisk_lines = _extract_keyword_lines(cards, "FISK")
        row3 = fisk_lines[2]
        # Heave stiffness = 2e6 at position [2][2]
        values = re.findall(r'[+-]?\d+\.\d+e[+-]\d+', row3)
        parsed_heave = float(values[2])
        assert abs(parsed_heave - 2e6) / 2e6 < 0.01, (
            f"Row 3 heave stiffness expected 2e6, got {parsed_heave}"
        )


class TestDeck7DampingAndStiffness:
    """Deck 7 with both external_damping and external_stiffness."""

    def test_deck7_both_present(self):
        """Both FIDP and FISK cards emitted when both are defined."""
        spec = _make_spec(
            external_damping=_roll_damping_matrix(36010.0),
            external_stiffness=_stiffness_matrix(),
        )
        backend = AQWABackend()
        cards = backend.build_deck7(spec)

        fidp_lines = _extract_keyword_lines(cards, "FIDP")
        fisk_lines = _extract_keyword_lines(cards, "FISK")
        assert len(fidp_lines) == 6
        assert len(fisk_lines) == 6

    def test_deck7_fidp_before_fisk(self):
        """FIDP cards appear before FISK cards in the card list."""
        spec = _make_spec(
            external_damping=_roll_damping_matrix(36010.0),
            external_stiffness=_stiffness_matrix(),
        )
        backend = AQWABackend()
        cards = backend.build_deck7(spec)

        card_text = "\n".join(cards)
        first_fidp = card_text.index("FIDP")
        first_fisk = card_text.index("FISK")
        assert first_fidp < first_fisk, (
            "FIDP cards must appear before FISK cards"
        )

    def test_deck7_wfs1_and_end_present(self):
        """WFS1 banner and END card bracket the FIDP/FISK content."""
        spec = _make_spec(
            external_damping=_roll_damping_matrix(),
            external_stiffness=_stiffness_matrix(),
        )
        backend = AQWABackend()
        cards = backend.build_deck7(spec)

        assert any("WFS1" in c for c in cards)
        assert cards[-1].strip() == "END"


class TestFIDPCardFormat:
    """Verify exact column-position formatting of FIDP card lines."""

    def test_fidp_card_keyword_position(self):
        """FIDP keyword starts at column 7 (1-indexed)."""
        spec = _make_spec(external_damping=_roll_damping_matrix())
        backend = AQWABackend()
        cards = backend.build_deck7(spec)

        fidp_lines = _extract_keyword_lines(cards, "FIDP")
        for line in fidp_lines:
            # The keyword "FIDP" should be preceded by 6 spaces
            # (from f"{' ':>1s}{' ':>3s}{' ':>2s}" = 1+3+2 = 6 chars)
            assert line[:6] == "      ", (
                f"Expected 6 leading spaces, got: '{line[:6]}'"
            )
            assert line[6:10] == "FIDP", (
                f"Expected 'FIDP' at columns 7-10, got: '{line[6:10]}'"
            )

    def test_fidp_card_row_index_position(self):
        """Row index occupies columns 15-19 (5 chars wide, right-aligned)."""
        spec = _make_spec(external_damping=_roll_damping_matrix())
        backend = AQWABackend()
        cards = backend.build_deck7(spec)

        fidp_lines = _extract_keyword_lines(cards, "FIDP")
        for expected_row, line in enumerate(fidp_lines, start=1):
            # 5 spaces after keyword: columns 11-15
            assert line[10:15] == "     ", (
                f"Expected 5 spaces at cols 11-15, got: '{line[10:15]}'"
            )
            # Row index at columns 16-20 (5 chars, right-aligned)
            row_field = line[15:20]
            assert int(row_field) == expected_row, (
                f"Expected row {expected_row} at cols 16-20, "
                f"got: '{row_field}'"
            )

    def test_fidp_card_value_fields_width(self):
        """Each of the 6 value fields is exactly 10 characters wide."""
        spec = _make_spec(external_damping=_full_damping_matrix())
        backend = AQWABackend()
        cards = backend.build_deck7(spec)

        fidp_lines = _extract_keyword_lines(cards, "FIDP")
        for line in fidp_lines:
            # Values start at column 21 (after 6+4+5+5 = 20 prefix chars)
            values_section = line[20:]
            # Total width should be 6 * 10 = 60 chars
            assert len(values_section) == 60, (
                f"Expected 60-char value section, got {len(values_section)}: "
                f"'{values_section}'"
            )

    def test_fidp_card_scientific_notation(self):
        """Values use scientific notation with 3 decimal places."""
        spec = _make_spec(external_damping=_roll_damping_matrix(36010.0))
        backend = AQWABackend()
        cards = backend.build_deck7(spec)

        fidp_lines = _extract_keyword_lines(cards, "FIDP")
        # Row 4 has the non-zero value
        row4 = fidp_lines[3]
        values = re.findall(r'[+-]?\d+\.\d+e[+-]\d+', row4)
        assert len(values) == 6, (
            f"Expected 6 scientific-notation values, found {len(values)}"
        )
        # Each value should have exactly 3 decimal places
        for val_str in values:
            # Match pattern: digits.3digits e +/- digits
            assert re.match(r'[+-]?\d+\.\d{3}e[+-]\d+', val_str), (
                f"Value '{val_str}' does not have 3 decimal places"
            )

    def test_fidp_card_total_line_length(self):
        """Each FIDP line is exactly 80 characters (20 prefix + 60 values)."""
        spec = _make_spec(external_damping=_full_damping_matrix())
        backend = AQWABackend()
        cards = backend.build_deck7(spec)

        fidp_lines = _extract_keyword_lines(cards, "FIDP")
        for line in fidp_lines:
            assert len(line) == 80, (
                f"Expected 80-char line, got {len(line)}: '{line}'"
            )


class TestMatrixHasNonzero:
    """Unit tests for the _matrix_has_nonzero static method."""

    def test_zero_matrix_returns_false(self):
        assert not AQWABackend._matrix_has_nonzero(_zero_matrix())

    def test_nonzero_matrix_returns_true(self):
        assert AQWABackend._matrix_has_nonzero(_roll_damping_matrix())

    def test_single_nonzero_element(self):
        matrix = _zero_matrix()
        matrix[5][0] = 1e-10
        assert AQWABackend._matrix_has_nonzero(matrix)

    def test_negative_value_is_nonzero(self):
        matrix = _zero_matrix()
        matrix[0][0] = -100.0
        assert AQWABackend._matrix_has_nonzero(matrix)
