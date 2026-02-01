"""AQWA input backend: convert DiffractionSpec to AQWA .dat files (WRK-058).

Supports two output modes:
- Single: all decks concatenated into one .dat file
- Modular: each deck written to a separate file in an output directory

The backend maps the solver-agnostic DiffractionSpec fields to AQWA-specific
deck/card format, using fixed-column FORTRAN-style formatting consistent with
the conventions in aqwa_dat_files.py.

Mesh data (Deck 1 node coordinates, Deck 2 element connectivity) are emitted
as placeholder comments. Actual mesh integration is deferred to WRK-060.
"""

from __future__ import annotations

import math
from pathlib import Path
from typing import Optional

import numpy as np

from digitalmodel.modules.diffraction.input_schemas import (
    DiffractionSpec,
    VesselInertia,
)


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

# AQWA uses a large depth to represent "infinite" / deep water
_DEEP_WATER_DEPTH = 10000.0

# Column-formatting helpers (mirrors aqwa_dat_files.py white_space convention)
_WS = " "


# ---------------------------------------------------------------------------
# Formatting helpers
# ---------------------------------------------------------------------------


def _deck_banner(deck_num: int) -> list[str]:
    """Return the 3-line banner for a deck separator."""
    stars = "*" * 80
    label = f" DECK{deck_num:>3d} "
    pad_total = 80 - len(label)
    left = pad_total // 2
    right = pad_total - left
    mid = "*" * left + label + "*" * right
    return [stars, mid, stars]


def _fmt_float(value: float, width: int = 10) -> str:
    """Format a float for AQWA fixed-width fields.

    Uses scientific notation for large/small values, otherwise fixed-point.
    """
    abs_val = abs(value)
    if abs_val == 0.0:
        return f"{0.0:>{width}.1f}"
    if abs_val >= 1e7 or abs_val < 1e-2:
        return f"{value:>{width}.3e}"
    if abs_val >= 1000:
        return f"{value:>{width}.0f}"
    return f"{value:>{width}g}"


# ---------------------------------------------------------------------------
# AQWABackend
# ---------------------------------------------------------------------------


class AQWABackend:
    """Generate AQWA .dat input files from a DiffractionSpec.

    Public API
    ----------
    generate_single(spec, output_dir) -> Path
        Write all decks into a single .dat file.
    generate_modular(spec, output_dir) -> Path
        Write each deck into a separate file inside *output_dir*.

    The ``build_deck*`` methods are also public so individual card
    blocks can be inspected or tested in isolation.
    """

    # -----------------------------------------------------------------
    # Public entry points
    # -----------------------------------------------------------------

    def generate_single(
        self, spec: DiffractionSpec, output_dir: Path
    ) -> Path:
        """Generate a single .dat file containing all decks.

        Parameters
        ----------
        spec : DiffractionSpec
            Canonical analysis specification.
        output_dir : Path
            Directory where the .dat file will be written.

        Returns
        -------
        Path
            Path to the generated .dat file.
        """
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        all_cards = self._assemble_all_decks(spec)
        file_name = self._make_filename(spec)
        output_path = output_dir / file_name

        output_path.write_text("\n".join(all_cards) + "\n")
        return output_path

    def generate_modular(
        self, spec: DiffractionSpec, output_dir: Path
    ) -> Path:
        """Generate modular deck files, one per deck.

        Parameters
        ----------
        spec : DiffractionSpec
            Canonical analysis specification.
        output_dir : Path
            Directory where deck files will be written.

        Returns
        -------
        Path
            Path to the output directory containing the deck files.
        """
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        deck_builders = self._ordered_deck_builders(spec)
        for idx, (deck_num, cards) in enumerate(deck_builders):
            file_name = f"deck_{idx:02d}_{deck_num:02d}.dat"
            file_path = output_dir / file_name
            file_path.write_text("\n".join(cards) + "\n")

        return output_dir

    # -----------------------------------------------------------------
    # Internal assembly
    # -----------------------------------------------------------------

    def _assemble_all_decks(self, spec: DiffractionSpec) -> list[str]:
        """Concatenate all deck card blocks in order."""
        all_cards: list[str] = []
        for _deck_num, cards in self._ordered_deck_builders(spec):
            all_cards.extend(cards)
        return all_cards

    def _ordered_deck_builders(
        self, spec: DiffractionSpec
    ) -> list[tuple[int, list[str]]]:
        """Return (deck_number, cards) pairs in canonical order."""
        return [
            (0, self.build_deck0(spec)),
            (1, self.build_deck1(spec)),
            (2, self.build_deck2(spec)),
            (3, self.build_deck3(spec)),
            (4, self.build_deck4(spec)),
            (5, self.build_deck5(spec)),
            (6, self.build_deck6(spec)),
        ]

    def _make_filename(self, spec: DiffractionSpec) -> str:
        """Derive a .dat filename from the spec metadata."""
        project = spec.metadata.project or "aqwa_analysis"
        safe_name = (
            project.replace(" ", "_").replace("/", "_").replace("\\", "_")
        )
        return f"{safe_name}.dat"

    # -----------------------------------------------------------------
    # Deck 0 — JOB control
    # -----------------------------------------------------------------

    def build_deck0(self, spec: DiffractionSpec) -> list[str]:
        """Build Deck 0: JOB control cards."""
        cards: list[str] = []
        cards.extend(_deck_banner(0))

        # JOB card
        cards.append("JOB AQWA  LINE")

        # TITLE card
        title = spec.metadata.project or spec.metadata.description or ""
        cards.append(f"TITLE               {title}")

        # OPTIONS
        options = self._build_options(spec)
        cards.append(f"OPTIONS {options}")

        # RESTART
        cards.append("RESTART  1  5")

        return cards

    def _build_options(self, spec: DiffractionSpec) -> str:
        """Map solver_options to AQWA OPTIONS string."""
        opts: list[str] = ["GOON"]
        if spec.solver_options.remove_irregular_frequencies:
            opts.append("LHFR")
        if spec.solver_options.qtf_calculation:
            opts.append("MQTF")
        opts.append("REST")
        opts.append("END")
        return " ".join(opts)

    # -----------------------------------------------------------------
    # Deck 1 — Node coordinates (placeholder for WRK-060)
    # -----------------------------------------------------------------

    def build_deck1(self, spec: DiffractionSpec) -> list[str]:
        """Build Deck 1: node coordinate cards.

        Mesh data is not yet integrated (WRK-060). This emits the deck
        header, STRC markers for each body, and a placeholder comment.
        """
        cards: list[str] = []
        cards.extend(_deck_banner(1))
        cards.append(f"{_WS:>10s}COOR")
        cards.append(f"{_WS:>6s}NOD5")

        bodies = spec.get_bodies()
        for idx, body in enumerate(bodies, start=1):
            cards.append(
                f"{_WS:>6s}STRC{_WS:>4s}{idx:>5d}"
            )
            cards.append(
                f"* Mesh nodes for {body.vessel.name} "
                f"(mesh file: {body.vessel.geometry.mesh_file}) "
                f"— placeholder, see WRK-060"
            )

        cards.append(f"{_WS:>10s}FINI")
        return cards

    # -----------------------------------------------------------------
    # Deck 2 — Element connectivity (placeholder for WRK-060)
    # -----------------------------------------------------------------

    def build_deck2(self, spec: DiffractionSpec) -> list[str]:
        """Build Deck 2: element connectivity cards.

        Mesh data is not yet integrated (WRK-060). This emits only the
        deck header and a placeholder comment.
        """
        cards: list[str] = []
        cards.extend(_deck_banner(2))
        cards.append(f"{_WS:>10s}ELM1{_WS:>6s}FST")

        bodies = spec.get_bodies()
        for idx, body in enumerate(bodies, start=1):
            cards.append(
                f"* Elements for structure {idx}: {body.vessel.name} "
                f"— placeholder, see WRK-060"
            )

        cards.append(" END")
        return cards

    # -----------------------------------------------------------------
    # Deck 3 — Mass properties (MATE)
    # -----------------------------------------------------------------

    def build_deck3(self, spec: DiffractionSpec) -> list[str]:
        """Build Deck 3: mass property cards."""
        cards: list[str] = []
        cards.extend(_deck_banner(3))
        cards.append(f"{_WS:>10s}MATE")

        bodies = spec.get_bodies()
        for idx, body in enumerate(bodies, start=1):
            mass = body.vessel.inertia.mass
            # Reference node 98000 is AQWA convention for CoG
            mass_str = _fmt_float(mass)
            cards.append(
                f"{_WS:>5s}{idx:>1d}{_WS:>9s}98000{mass_str}"
            )

        cards.append(" END")
        return cards

    # -----------------------------------------------------------------
    # Deck 4 — Inertia properties (GEOM / PMAS)
    # -----------------------------------------------------------------

    def build_deck4(self, spec: DiffractionSpec) -> list[str]:
        """Build Deck 4: inertia property cards."""
        cards: list[str] = []
        cards.extend(_deck_banner(4))
        cards.append(f"{_WS:>10s}GEOM")

        bodies = spec.get_bodies()
        for idx, body in enumerate(bodies, start=1):
            ixx, iyy, izz, ixy, ixz, iyz = self._compute_inertia(
                body.vessel.inertia
            )
            cards.append(
                f"{_WS:>5s}{idx:>1d}PMAS{_WS:>5s}98000"
                f"{_fmt_float(ixx)}{_fmt_float(ixy)}{_fmt_float(ixz)}"
                f"{_fmt_float(iyy)}{_fmt_float(iyz)}{_fmt_float(izz)}"
            )

        cards.append(" END")
        return cards

    @staticmethod
    def _compute_inertia(
        inertia: VesselInertia,
    ) -> tuple[float, float, float, float, float, float]:
        """Return (Ixx, Iyy, Izz, Ixy, Ixz, Iyz) from spec inertia.

        If an explicit inertia_tensor is provided it takes precedence;
        otherwise the moments are computed from mass and radii of gyration.
        """
        if inertia.inertia_tensor is not None:
            t = inertia.inertia_tensor
            return (
                t.get("Ixx", 0.0),
                t.get("Iyy", 0.0),
                t.get("Izz", 0.0),
                t.get("Ixy", 0.0),
                t.get("Ixz", 0.0),
                t.get("Iyz", 0.0),
            )
        # Compute from radii of gyration: I = m * k^2
        mass = inertia.mass
        kxx, kyy, kzz = inertia.radii_of_gyration  # type: ignore[misc]
        return (
            mass * kxx**2,
            mass * kyy**2,
            mass * kzz**2,
            0.0,
            0.0,
            0.0,
        )

    # -----------------------------------------------------------------
    # Deck 5 — Global environment (GLOB)
    # -----------------------------------------------------------------

    def build_deck5(self, spec: DiffractionSpec) -> list[str]:
        """Build Deck 5: environment (GLOB) cards."""
        cards: list[str] = []
        cards.extend(_deck_banner(5))
        cards.append(f"{_WS:>10s}GLOB")

        depth = self._resolve_water_depth(spec.environment.water_depth)
        depth_str = _fmt_float(depth)
        cards.append(f"{_WS:>6s}DPTH{depth_str}")

        density_str = _fmt_float(spec.environment.water_density)
        cards.append(f"{_WS:>6s}DENS{density_str}")

        gravity_str = f"{spec.environment.gravity:>10g}"
        cards.append(f"{_WS:>6s}ACCG{gravity_str}")

        cards.append(" END")
        return cards

    @staticmethod
    def _resolve_water_depth(depth: float | str) -> float:
        """Convert spec water depth to a numeric AQWA value."""
        if isinstance(depth, str) and depth.lower() in (
            "infinite",
            "inf",
            "deep",
        ):
            return _DEEP_WATER_DEPTH
        return float(depth)

    # -----------------------------------------------------------------
    # Deck 6 — Frequencies and directions (FDR1, HRTZ, DIRN)
    # -----------------------------------------------------------------

    def build_deck6(self, spec: DiffractionSpec) -> list[str]:
        """Build Deck 6: frequency/direction cards."""
        cards: list[str] = []
        cards.extend(_deck_banner(6))
        cards.append(f"{_WS:>10s}FDR1")

        # Frequencies: convert rad/s -> Hz for AQWA HRTZ cards
        freqs_rad = spec.frequencies.to_frequencies_rad_s()
        freqs_hz = [w / (2.0 * math.pi) for w in freqs_rad]

        for i, freq_hz in enumerate(freqs_hz, start=1):
            freq_str = f"{freq_hz:>10g}"
            cards.append(
                f"{_WS:>5s}1HRTZ{i:>5d}{i:>5d}{freq_str}"
            )

        # Headings
        headings = spec.wave_headings.to_heading_list()
        for i, heading_deg in enumerate(headings, start=1):
            heading_str = f"{heading_deg:>10g}"
            cards.append(
                f"{_WS:>5s}1DIRN{i:>5d}{i:>5d}{heading_str}"
            )

        cards.append(" END")
        return cards
