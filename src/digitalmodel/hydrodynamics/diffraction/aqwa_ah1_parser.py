"""Parser for AQWA .AH1 ASCII hydrodynamic database files.

The .AH1 file is produced by AQWA when the ``OPTIONS AHD1`` card is
included in the input deck.  It contains the same data as the binary
.HYD file but in a human-readable ASCII format:

- Added mass matrices (6x6, frequency-dependent)
- Radiation damping matrices (6x6, frequency-dependent)
- Excitation force RAOs (magnitude + phase, per DOF/heading/frequency)
- Hydrostatic stiffness, mass, draft, COG

File layout (based on bemio reference implementation and AQWA documentation):

    * <comment lines starting with asterisk>
    <num_bodies>  <num_headings>  <num_frequencies>
    <heading values, up to 6 per line>
    <frequency values in rad/s, up to 6 per line>
    GENERAL
    <water_depth>  <density>  <gravity>
    DRAFT
    <draft per body>
    COG
    <x y z per body>
    MASS
    <6x6 mass matrix per body, 6 rows of 6 values>
    HYDSTIFFNESS
    <6x6 stiffness matrix per body>
    ADDEDMASS
    <for each body-pair, for each frequency: 6 rows of 6 values>
    DAMPING
    <same layout as ADDEDMASS>
    FORCERAO
    <for each body, heading, frequency: magnitude line + phase line>

References:
    - bemio (WEC-Sim): https://github.com/WEC-Sim/bemio/blob/master/bemio/io/aqwa.py
    - AQWA User Manual, Section on OPTIONS AHD1
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

import numpy as np


# ---------------------------------------------------------------------------
# Result container
# ---------------------------------------------------------------------------


@dataclass
class AH1ParseResult:
    """Structured data extracted from an AQWA .AH1 file.

    Attributes:
        num_bodies: Number of structures in the model.
        num_frequencies: Number of wave frequencies.
        num_headings: Number of wave headings.
        frequencies: Wave frequencies in rad/s, shape (num_frequencies,).
        headings: Wave headings in degrees, shape (num_headings,).
        water_depth: Water depth in metres (inf for deep water).
        water_density: Water density in kg/m^3.
        gravity: Gravitational acceleration in m/s^2.
        drafts: Draft per body, shape (num_bodies,).
        cog: Centre of gravity per body, shape (num_bodies, 3).
        mass: Mass matrix per body, shape (num_bodies, 6, 6).
        hydrostatic_stiffness: Hydrostatic stiffness per body,
            shape (num_bodies, 6, 6).
        added_mass: Added mass matrices,
            shape (num_bodies, num_bodies, num_frequencies, 6, 6).
        damping: Radiation damping matrices,
            shape (num_bodies, num_bodies, num_frequencies, 6, 6).
        excitation_magnitude: Excitation force RAO magnitude per body,
            shape (num_bodies, num_headings, num_frequencies, 6).
        excitation_phase: Excitation force RAO phase (degrees) per body,
            shape (num_bodies, num_headings, num_frequencies, 6).
    """

    num_bodies: int = 0
    num_frequencies: int = 0
    num_headings: int = 0
    frequencies: np.ndarray = field(default_factory=lambda: np.array([]))
    headings: np.ndarray = field(default_factory=lambda: np.array([]))
    water_depth: float = 0.0
    water_density: float = 1025.0
    gravity: float = 9.80665
    drafts: np.ndarray = field(default_factory=lambda: np.array([]))
    cog: np.ndarray = field(default_factory=lambda: np.array([]))
    mass: np.ndarray = field(default_factory=lambda: np.array([]))
    hydrostatic_stiffness: np.ndarray = field(
        default_factory=lambda: np.array([])
    )
    added_mass: np.ndarray = field(default_factory=lambda: np.array([]))
    damping: np.ndarray = field(default_factory=lambda: np.array([]))
    excitation_magnitude: np.ndarray = field(
        default_factory=lambda: np.array([])
    )
    excitation_phase: np.ndarray = field(
        default_factory=lambda: np.array([])
    )


# ---------------------------------------------------------------------------
# Parser
# ---------------------------------------------------------------------------


def parse_ah1(file_path: str | Path) -> AH1ParseResult:
    """Parse an AQWA .AH1 ASCII hydrodynamic database file.

    Args:
        file_path: Path to the .AH1 file.

    Returns:
        AH1ParseResult with all extracted data arrays.

    Raises:
        FileNotFoundError: If the file does not exist.
        ValueError: If the file cannot be parsed.
    """
    file_path = Path(file_path)
    if not file_path.exists():
        raise FileNotFoundError(f"AH1 file not found: {file_path}")

    with open(file_path, "r", encoding="utf-8", errors="replace") as f:
        raw_lines = f.readlines()

    # Strip comment lines (start with '*')
    lines = [line.rstrip() for line in raw_lines]

    result = AH1ParseResult()
    idx = _skip_comments(lines, 0)

    if idx >= len(lines):
        raise ValueError(
            f"No data found in AH1 file (only comments): {file_path}"
        )

    # --- Header: num_bodies, num_headings, num_frequencies ---
    idx = _parse_header(lines, idx, result)

    # --- Section-based parsing ---
    while idx < len(lines):
        line = lines[idx].strip().upper()

        if line == "GENERAL":
            idx = _parse_general(lines, idx + 1, result)
        elif line == "DRAFT":
            idx = _parse_draft(lines, idx + 1, result)
        elif line == "COG":
            idx = _parse_cog(lines, idx + 1, result)
        elif line == "MASS":
            idx = _parse_mass(lines, idx + 1, result)
        elif line == "HYDSTIFFNESS":
            idx = _parse_hydrostatic_stiffness(lines, idx + 1, result)
        elif line == "ADDEDMASS":
            idx = _parse_added_mass(lines, idx + 1, result)
        elif line == "DAMPING":
            idx = _parse_damping(lines, idx + 1, result)
        elif line == "FORCERAO":
            idx = _parse_force_rao(lines, idx + 1, result)
        else:
            idx += 1

    return result


# ---------------------------------------------------------------------------
# Internal parsing helpers
# ---------------------------------------------------------------------------


def _skip_comments(lines: list[str], idx: int) -> int:
    """Advance past lines starting with '*'."""
    while idx < len(lines) and lines[idx].strip().startswith("*"):
        idx += 1
    return idx


def _read_floats(line: str) -> list[float]:
    """Parse a line into a list of floats."""
    return [float(tok) for tok in line.split()]


def _read_n_floats(
    lines: list[str], idx: int, count: int, per_line: int = 6
) -> tuple[list[float], int]:
    """Read *count* float values spread across multiple lines.

    Values are packed up to *per_line* per line.
    Returns the values and the next line index.
    """
    values: list[float] = []
    num_lines = math.ceil(count / per_line)
    for _ in range(num_lines):
        if idx < len(lines):
            values.extend(_read_floats(lines[idx]))
            idx += 1
    return values[:count], idx


def _read_6x6(lines: list[str], idx: int) -> tuple[np.ndarray, int]:
    """Read a 6x6 matrix (6 rows, 6 values per row).

    Returns the matrix and the next line index.
    """
    matrix = np.zeros((6, 6))
    for row in range(6):
        if idx < len(lines):
            vals = _read_floats(lines[idx])
            for col, v in enumerate(vals[:6]):
                matrix[row, col] = v
            idx += 1
    return matrix, idx


def _read_6x6_prefixed(
    lines: list[str], idx: int, first_row_prefix_count: int = 0,
) -> tuple[np.ndarray, int]:
    """Read a 6x6 matrix where the first row may have prefix tokens.

    Many AH1 sections prefix the first row of each 6x6 block with body
    numbers or indices (e.g. ``1  5.5E+07 0 0 0 0 0`` for MASS, or
    ``1 1 1  5.3E+07 ...`` for ADDEDMASS).  This function skips the
    leading prefix tokens on the first line when the line contains more
    than 6 values.  Subsequent rows are read as plain 6-value lines.

    Auto-detection: if the first row has exactly 6 values, no prefix is
    assumed (compatible with simplified test fixtures).  If it has more
    than 6 values, the first *first_row_prefix_count* tokens are skipped.

    If *first_row_prefix_count* is 0, behaviour is identical to
    :func:`_read_6x6` (taking the first 6 values from each line).
    """
    matrix = np.zeros((6, 6))
    for row in range(6):
        if idx < len(lines):
            vals = _read_floats(lines[idx])
            if row == 0 and first_row_prefix_count > 0 and len(vals) > 6:
                vals = vals[first_row_prefix_count:]
            for col, v in enumerate(vals[:6]):
                matrix[row, col] = v
            idx += 1
    return matrix, idx


# ---------------------------------------------------------------------------
# Section parsers
# ---------------------------------------------------------------------------


def _parse_header(
    lines: list[str], idx: int, result: AH1ParseResult
) -> int:
    """Parse the header: body count, heading count, frequency count,
    then heading values and frequency values.

    Some AH1 files place the first batch of heading values on the same
    line as the three header integers (e.g. ``1  9  3  -180.0 ...``).
    We consume any trailing floats on the header line before reading
    continuation lines.
    """
    if idx >= len(lines):
        return idx

    tokens = lines[idx].split()
    if len(tokens) < 3:
        raise ValueError(
            f"Expected header with 3 integers on line {idx + 1}, "
            f"got: {lines[idx]!r}"
        )

    result.num_bodies = int(tokens[0])
    result.num_headings = int(tokens[1])
    result.num_frequencies = int(tokens[2])

    # Trailing tokens on the header line may be the first heading values
    inline_floats = [float(t) for t in tokens[3:]]
    idx += 1

    # Heading values — start with any inline floats, then read remaining
    remaining_headings = result.num_headings - len(inline_floats)
    if remaining_headings > 0:
        extra, idx = _read_n_floats(lines, idx, remaining_headings)
        headings = inline_floats + extra
    else:
        headings = inline_floats[: result.num_headings]
    result.headings = np.array(headings)

    # Frequency values (up to 6 per line)
    freqs, idx = _read_n_floats(lines, idx, result.num_frequencies)
    result.frequencies = np.array(freqs)

    return idx


def _parse_general(
    lines: list[str], idx: int, result: AH1ParseResult
) -> int:
    """Parse GENERAL section: body_number, water_depth, density, gravity, ...

    The GENERAL line in AH1 files is prefixed with a body number, e.g.:
        ``1  200.000  1025.000  9.807  0  0``
    We skip the leading body number.
    """
    if idx >= len(lines):
        return idx
    vals = _read_floats(lines[idx])
    # vals[0] is body number; actual data starts at index 1
    if len(vals) >= 4:
        result.water_depth = vals[1]
        result.water_density = vals[2]
        result.gravity = vals[3]
    elif len(vals) >= 3:
        # Fallback for files without body number prefix
        result.water_depth = vals[0]
        result.water_density = vals[1]
        result.gravity = vals[2]
    return idx + 1


def _parse_draft(
    lines: list[str], idx: int, result: AH1ParseResult
) -> int:
    """Parse DRAFT section: body_number, draft per body.

    Format: ``1  110.000`` (body number prefix + draft value).
    """
    drafts: list[float] = []
    for _ in range(result.num_bodies):
        if idx < len(lines):
            vals = _read_floats(lines[idx])
            # vals[0] = body number, vals[1] = draft
            if len(vals) >= 2:
                drafts.append(vals[1])
            elif vals:
                drafts.append(vals[0])
            else:
                drafts.append(0.0)
            idx += 1
    result.drafts = np.array(drafts)
    return idx


def _parse_cog(
    lines: list[str], idx: int, result: AH1ParseResult
) -> int:
    """Parse COG section: body_number, x, y, z per body.

    Format: ``1  0.0  0.0  -61.630`` (body number prefix + 3 coords).
    """
    cog = np.zeros((result.num_bodies, 3))
    for b in range(result.num_bodies):
        if idx < len(lines):
            vals = _read_floats(lines[idx])
            # vals[0] = body number; vals[1:4] = x, y, z
            if len(vals) >= 4:
                cog[b, :3] = vals[1:4]
            elif len(vals) >= 3:
                cog[b, :len(vals[:3])] = vals[:3]
            idx += 1
    result.cog = cog
    return idx


def _parse_mass(
    lines: list[str], idx: int, result: AH1ParseResult
) -> int:
    """Parse MASS section: 6x6 mass matrix per body.

    First row of each block is prefixed with body number, e.g.:
    ``1  5.5E+07  0  0  0  0  0``
    """
    mass = np.zeros((result.num_bodies, 6, 6))
    for b in range(result.num_bodies):
        matrix, idx = _read_6x6_prefixed(lines, idx, first_row_prefix_count=1)
        mass[b] = matrix
    result.mass = mass
    return idx


def _parse_hydrostatic_stiffness(
    lines: list[str], idx: int, result: AH1ParseResult
) -> int:
    """Parse HYDSTIFFNESS section: 6x6 stiffness matrix per body.

    First row of each block is prefixed with body number.
    """
    stiffness = np.zeros((result.num_bodies, 6, 6))
    for b in range(result.num_bodies):
        matrix, idx = _read_6x6_prefixed(lines, idx, first_row_prefix_count=1)
        stiffness[b] = matrix
    result.hydrostatic_stiffness = stiffness
    return idx


def _parse_added_mass(
    lines: list[str], idx: int, result: AH1ParseResult
) -> int:
    """Parse ADDEDMASS section.

    Layout: for each body-pair (body_i, body_j), for each frequency,
    read a 6x6 matrix. First row of each block has 3 prefix tokens
    (body_i, body_j, freq_index), e.g.:
    ``1  1  1  5.3237E+07 -8.1136E+00 ...``
    Total matrices = num_bodies^2 * num_frequencies.
    """
    nb = result.num_bodies
    nf = result.num_frequencies
    am = np.zeros((nb, nb, nf, 6, 6))

    for bi in range(nb):
        for bj in range(nb):
            for fi in range(nf):
                matrix, idx = _read_6x6_prefixed(
                    lines, idx, first_row_prefix_count=3
                )
                am[bi, bj, fi] = matrix

    result.added_mass = am
    return idx


def _parse_damping(
    lines: list[str], idx: int, result: AH1ParseResult
) -> int:
    """Parse DAMPING section (same layout as ADDEDMASS).

    First row of each 6x6 block has 3 prefix tokens (body_i, body_j,
    freq_index).
    """
    nb = result.num_bodies
    nf = result.num_frequencies
    damp = np.zeros((nb, nb, nf, 6, 6))

    for bi in range(nb):
        for bj in range(nb):
            for fi in range(nf):
                matrix, idx = _read_6x6_prefixed(
                    lines, idx, first_row_prefix_count=3
                )
                damp[bi, bj, fi] = matrix

    result.damping = damp
    return idx


def _parse_force_rao(
    lines: list[str], idx: int, result: AH1ParseResult
) -> int:
    """Parse FORCERAO section.

    Layout: for each body, for each heading, for each frequency,
    two lines:
      - magnitude line: ``body heading freq  m1 m2 m3 m4 m5 m6``
        (3 prefix tokens + 6 values)
      - phase line: ``p1 p2 p3 p4 p5 p6`` (6 values, no prefix)
    """
    nb = result.num_bodies
    nh = result.num_headings
    nf = result.num_frequencies
    mag = np.zeros((nb, nh, nf, 6))
    phase = np.zeros((nb, nh, nf, 6))

    for bi in range(nb):
        for hi in range(nh):
            for fi in range(nf):
                if idx < len(lines):
                    vals = _read_floats(lines[idx])
                    # Skip 3 prefix tokens (body, heading, freq index)
                    data = vals[3:] if len(vals) > 6 else vals
                    mag[bi, hi, fi, :len(data[:6])] = data[:6]
                    idx += 1
                if idx < len(lines):
                    vals = _read_floats(lines[idx])
                    phase[bi, hi, fi, :len(vals[:6])] = vals[:6]
                    idx += 1

    result.excitation_magnitude = mag
    result.excitation_phase = phase
    return idx
