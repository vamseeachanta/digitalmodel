"""AQWA input backend: convert DiffractionSpec to AQWA .dat files (WRK-058).

Supports two output modes:
- Single: all decks concatenated into one .dat file
- Modular: each deck written to a separate file in an output directory

The backend maps the solver-agnostic DiffractionSpec fields to AQWA-specific
deck/card format, using fixed-column FORTRAN-style formatting consistent with
the conventions in aqwa_dat_files.py.

Mesh data (Deck 1 node coordinates, Deck 2 element connectivity) is parsed
from GDF mesh files and emitted in AQWA's fixed-column format.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

import numpy as np

from digitalmodel.hydrodynamics.diffraction.input_schemas import (
    DiffractionSpec,
    VesselInertia,
)


# ---------------------------------------------------------------------------
# Mesh data container
# ---------------------------------------------------------------------------


@dataclass
class ParsedMesh:
    """Container for parsed mesh data from GDF files."""

    vertices: np.ndarray  # (N, 3) array of node coordinates
    panels: np.ndarray  # (M, 4) array of panel vertex indices
    name: str = ""

    @property
    def n_vertices(self) -> int:
        return len(self.vertices)

    @property
    def n_panels(self) -> int:
        return len(self.panels)


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
    generate_single(spec, output_dir, spec_dir=None) -> Path
        Write all decks into a single .dat file.
    generate_modular(spec, output_dir, spec_dir=None) -> Path
        Write each deck into a separate file inside *output_dir*.

    The ``build_deck*`` methods are also public so individual card
    blocks can be inspected or tested in isolation.

    Parameters
    ----------
    spec_dir : Path, optional
        Directory containing the spec YAML file. Used to resolve relative
        mesh file paths. If not provided, mesh paths must be absolute.
    """

    def __init__(self) -> None:
        self._spec_dir: Optional[Path] = None
        self._mesh_cache: dict[str, ParsedMesh] = {}

    # -----------------------------------------------------------------
    # Public entry points
    # -----------------------------------------------------------------

    def generate_single(
        self,
        spec: DiffractionSpec,
        output_dir: Path,
        spec_dir: Optional[Path] = None,
    ) -> Path:
        """Generate a single .dat file containing all decks.

        Parameters
        ----------
        spec : DiffractionSpec
            Canonical analysis specification.
        output_dir : Path
            Directory where the .dat file will be written.
        spec_dir : Path, optional
            Directory containing the spec YAML file, for resolving
            relative mesh paths.

        Returns
        -------
        Path
            Path to the generated .dat file.
        """
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        self._spec_dir = Path(spec_dir) if spec_dir else None
        self._mesh_cache.clear()

        all_cards = self._assemble_all_decks(spec)
        file_name = self._make_filename(spec)
        output_path = output_dir / file_name

        # AQWA on Windows expects CRLF line endings
        # Use newline='' to prevent Python from adding extra line conversion
        content = "\r\n".join(all_cards) + "\r\n"
        with open(output_path, "w", newline='') as f:
            f.write(content)
        return output_path

    def generate_modular(
        self,
        spec: DiffractionSpec,
        output_dir: Path,
        spec_dir: Optional[Path] = None,
    ) -> Path:
        """Generate modular deck files, one per deck.

        Parameters
        ----------
        spec : DiffractionSpec
            Canonical analysis specification.
        output_dir : Path
            Directory where deck files will be written.
        spec_dir : Path, optional
            Directory containing the spec YAML file, for resolving
            relative mesh paths.

        Returns
        -------
        Path
            Path to the output directory containing the deck files.
        """
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        self._spec_dir = Path(spec_dir) if spec_dir else None
        self._mesh_cache.clear()

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

    def _resolve_mesh_path(self, mesh_file: str) -> Path:
        """Resolve a mesh file path, relative to spec_dir if needed."""
        mesh_path = Path(mesh_file)
        if mesh_path.is_absolute():
            return mesh_path
        if self._spec_dir is not None:
            return (self._spec_dir / mesh_file).resolve()
        return mesh_path

    def _load_mesh(self, mesh_file: str) -> Optional[ParsedMesh]:
        """Load and parse a GDF mesh file.

        Returns None if the mesh file cannot be loaded.
        Results are cached by mesh_file path.
        """
        if mesh_file in self._mesh_cache:
            return self._mesh_cache[mesh_file]

        mesh_path = self._resolve_mesh_path(mesh_file)
        if not mesh_path.exists():
            return None

        try:
            mesh = self._parse_gdf(mesh_path)
            self._mesh_cache[mesh_file] = mesh
            return mesh
        except Exception:
            return None

    def _parse_gdf(self, file_path: Path) -> ParsedMesh:
        """Parse a WAMIT GDF mesh file.

        GDF format:
        - Line 1: Header comment (title)
        - Line 2: ULEN GRAV (unit length and gravity)
        - Line 3: ISX ISY (symmetry flags)
        - Line 4: NPAN (number of panels)
        - Lines 5+: 4 vertices per panel (X Y Z for each vertex)
        """
        with open(file_path, "r") as f:
            lines = f.readlines()

        line_idx = 0

        # Skip header comments starting with '#'
        while line_idx < len(lines) and lines[line_idx].strip().startswith("#"):
            line_idx += 1

        # Line 1 may be a title (not starting with #), skip it
        # Then read ULEN GRAV line
        if line_idx < len(lines):
            line_idx += 1  # Skip title or ULEN/GRAV

        # Read ULEN, GRAV (might be line 1 if no title)
        if line_idx < len(lines):
            parts = lines[line_idx].split()
            # Check if this looks like numbers (ULEN GRAV) or text (title)
            try:
                float(parts[0])
                # This is ULEN/GRAV line, continue
            except (ValueError, IndexError):
                pass
            line_idx += 1

        # Read symmetry flags (ISX ISY)
        if line_idx < len(lines):
            line_idx += 1

        # Read number of panels (NPAN)
        npan = 0
        if line_idx < len(lines):
            try:
                npan = int(lines[line_idx].split()[0])
            except (ValueError, IndexError):
                pass
            line_idx += 1

        # Read panel vertices (4 vertices per panel)
        vertices: list[tuple[float, float, float]] = []
        panels: list[list[int]] = []
        vertex_map: dict[tuple[float, float, float], int] = {}

        for _ in range(npan):
            panel_vertices: list[int] = []
            for _ in range(4):
                if line_idx >= len(lines):
                    break
                line = lines[line_idx].strip()
                if not line:
                    line_idx += 1
                    continue
                parts = line.split()
                if len(parts) >= 3:
                    try:
                        vertex = (
                            float(parts[0]),
                            float(parts[1]),
                            float(parts[2]),
                        )
                        if vertex not in vertex_map:
                            vertex_map[vertex] = len(vertices)
                            vertices.append(vertex)
                        panel_vertices.append(vertex_map[vertex])
                    except ValueError:
                        pass
                line_idx += 1

            if len(panel_vertices) == 4:
                panels.append(panel_vertices)

        return ParsedMesh(
            vertices=np.array(vertices, dtype=np.float64),
            panels=np.array(panels, dtype=np.int32),
            name=file_path.stem,
        )

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
    # Deck 1 — Node coordinates
    # -----------------------------------------------------------------

    def build_deck1(self, spec: DiffractionSpec) -> list[str]:
        """Build Deck 1: node coordinate cards.

        Parses mesh files referenced in the spec and emits node
        coordinates in AQWA's fixed-column format.
        """
        cards: list[str] = []
        cards.extend(_deck_banner(1))
        cards.append(f"{_WS:>10s}COOR")
        cards.append(f"{_WS:>6s}NOD5")

        bodies = spec.get_bodies()
        for struct_idx, body in enumerate(bodies, start=1):
            mesh_file = body.vessel.geometry.mesh_file
            mesh = self._load_mesh(mesh_file)

            cards.append(f"{_WS:>6s}STRC{_WS:>4s}{struct_idx:>5d}")

            if mesh is not None:
                # Emit node coordinates in AQWA format
                # Based on working files: "     1   10          149.10987-18.016252        0."
                # Format: 6-char struct, 5-char node, 10 spaces, then coordinates
                for node_idx, vertex in enumerate(mesh.vertices, start=1):
                    x_val = vertex[0]
                    y_val = vertex[1]
                    z_val = vertex[2]
                    # Format coordinates: 10 spaces, then space-separated values
                    cards.append(
                        f"{struct_idx:>6d}{node_idx:>5d}          "
                        f"{x_val:10.5g}{y_val:10.5g}{z_val:10.5g}"
                    )

                # Add CoG node (element 98000) for mass reference
                cog = body.vessel.inertia.centre_of_gravity
                cards.append(
                    f"{struct_idx:>6d}{98000:>5d}          "
                    f"{cog[0]:10.5g}{cog[1]:10.5g}{cog[2]:10.5g}"
                )
            else:
                # Fallback: emit warning comment if mesh cannot be loaded
                cards.append(
                    f"* WARNING: Mesh file '{mesh_file}' could not be loaded"
                )

        cards.append(f"{_WS:>10s}FINI")
        return cards

    # -----------------------------------------------------------------
    # Deck 2 — Element connectivity
    # -----------------------------------------------------------------

    def build_deck2(self, spec: DiffractionSpec) -> list[str]:
        """Build Deck 2: element connectivity cards.

        Parses mesh files referenced in the spec and emits panel
        (element) connectivity in AQWA QPPL format (Workbench-compatible).
        """
        cards: list[str] = []
        cards.extend(_deck_banner(2))
        cards.append("          ELM1")

        bodies = spec.get_bodies()
        for struct_idx, body in enumerate(bodies, start=1):
            mesh_file = body.vessel.geometry.mesh_file
            mesh = self._load_mesh(mesh_file)
            vessel_name = body.vessel.name or "body"

            # SEAG card with bounding box
            cards.append(
                f"      SEAG          ( 81, 51,      -50.,       50.,"
                f"      -50.,       50.)"
            )
            # ZLWL (waterline) card
            cards.append("      ZLWL          (        0.)")
            # Group ID comment
            group_id = 15
            cards.append(f"* Group ID    {group_id} is body named {vessel_name}")

            if mesh is not None:
                # Emit element connectivity in QPPL format
                # Format: "     1QPPL        15(1)(    1)(    2)(    3)(    4)"
                for elem_idx, panel in enumerate(mesh.panels, start=1):
                    # Panel contains 0-based indices, convert to 1-based
                    n1 = panel[0] + 1
                    n2 = panel[1] + 1
                    n3 = panel[2] + 1
                    n4 = panel[3] + 1
                    cards.append(
                        f"     {struct_idx}QPPL        {group_id}({struct_idx})"
                        f"({n1:>5d})({n2:>5d})({n3:>5d})({n4:>5d})"
                        f"  Aqwa Elem No: {elem_idx:>4d}"
                    )
            else:
                cards.append(
                    f"* WARNING: Mesh file '{mesh_file}' could not be loaded"
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
