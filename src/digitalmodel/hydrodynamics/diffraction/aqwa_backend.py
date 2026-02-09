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


def _fmt_coord(value: float) -> str:
    """Format a coordinate value in 10-char AQWA field.

    Always includes a decimal point (required by AQWA v252+).
    Matches Workbench output format: e.g. ``149.10987``, ``-18.016252``, ``0.``
    """
    s = f"{value:.5g}"
    if "." not in s and "e" not in s.lower():
        s += "."
    return f"{s:>10s}"


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
        all_cards: list[str] = self._workbench_header()
        for _deck_num, cards in self._ordered_deck_builders(spec):
            all_cards.extend(cards)
        return all_cards

    @staticmethod
    def _workbench_header() -> list[str]:
        """Workbench-compatible file header (required by AQWA v252+)."""
        ruler1 = "*********1*********2*********3*********4*********5*********6*********7*********8"
        ruler2 = "*2345678901234567890123456789012345678901234567890123456789012345678901234567890"
        return [
            ruler1,
            ruler2,
            "*" * 80,
            "***************** File generated by digitalmodel AQWA backend *****************",
            "*" * 80,
        ]

    def _ordered_deck_builders(
        self, spec: DiffractionSpec
    ) -> list[tuple[int, list[str]]]:
        """Return (deck_number, cards) pairs in canonical order."""
        decks = [
            (0, self.build_deck0(spec)),
            (1, self.build_deck1(spec)),
            (2, self.build_deck2(spec)),
            (3, self.build_deck3(spec)),
            (4, self.build_deck4(spec)),
            (5, self.build_deck5(spec)),
            (6, self.build_deck6(spec)),
            (7, self.build_deck7(spec)),
            (8, self.build_deck8(spec)),
        ]
        # AQWA LINE reads decks 0-20; fill remaining with NONE
        for d in range(9, 21):
            decks.append((d, self._build_none_deck(d)))
        return decks

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

        # NUM_CORES (required by AQWA v252+)
        cards.append("NUM_CORES         4")

        # OPTIONS GOON — continue past non-fatal mesh errors
        cards.append("OPTIONS GOON ")
        # Feature options
        options = self._build_options(spec)
        cards.append(f"OPTIONS {options}")

        cards.extend(self._build_ah1_option(spec))

        # RESTART
        cards.append("RESTART  1  5")

        return cards

    def _build_options(self, spec: DiffractionSpec) -> str:
        """Map solver_options to AQWA OPTIONS string."""
        opts: list[str] = []
        if spec.solver_options.remove_irregular_frequencies:
            opts.append("LHFR")
        if spec.solver_options.qtf_calculation:
            opts.append("MQTF")
        opts.append("REST")
        opts.append("END")
        return " ".join(opts)

    def _build_ah1_option(self, spec: DiffractionSpec) -> list[str]:
        """Return OPTIONS AHD1 card when output_ah1 is enabled."""
        if spec.solver_options.output_ah1:
            return ["OPTIONS AHD1"]
        return []

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
                # Emit node coordinates in AQWA Workbench format
                # Format: I6 struct, I5 node, 9-space pad, 3×F10 coordinates
                # Coordinates must always include decimal point (AQWA v252+)
                for node_idx, vertex in enumerate(mesh.vertices, start=1):
                    cards.append(
                        f"{struct_idx:>6d}{node_idx:>5d}         "
                        f"{_fmt_coord(vertex[0])}"
                        f"{_fmt_coord(vertex[1])}"
                        f"{_fmt_coord(vertex[2])}"
                    )

                # Add CoG node (element 98000) for mass reference
                cog = body.vessel.inertia.centre_of_gravity
                cards.append(
                    f"{struct_idx:>6d}{98000:>5d}         "
                    f"{_fmt_coord(cog[0])}"
                    f"{_fmt_coord(cog[1])}"
                    f"{_fmt_coord(cog[2])}"
                )
            else:
                # Fallback: emit warning comment if mesh cannot be loaded
                cards.append(
                    f"* WARNING: Mesh file '{mesh_file}' could not be loaded"
                )

        cards.append(" END")
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

            # Compute bounding box from mesh vertices
            if mesh is not None and len(mesh.vertices) > 0:
                xmin = float(mesh.vertices[:, 0].min()) - 1.0
                xmax = float(mesh.vertices[:, 0].max()) + 1.0
                ymin = float(mesh.vertices[:, 1].min()) - 1.0
                ymax = float(mesh.vertices[:, 1].max()) + 1.0
            else:
                xmin, xmax, ymin, ymax = -100.0, 100.0, -100.0, 100.0

            # SEAG card — resolution only (non-Workbench mode accepts 2 params)
            cards.append(
                f"      SEAG          ( 81, 51)"
            )
            # ZLWL (waterline) card
            wl_z = body.vessel.geometry.waterline_z or 0.0
            cards.append(f"      ZLWL          ({wl_z:>9.1f})")
            # ILID AUTO — internal lid for irregular frequency removal
            # Only emit when remove_irregular_frequencies is enabled,
            # matching the LHFR option set in Deck 0.
            if spec.solver_options.remove_irregular_frequencies:
                lid_group = 21
                cards.append(
                    f"     {struct_idx}ILID AUTO   {lid_group}"
                )
            # Group ID comment
            group_id = 15
            cards.append(f"* Group ID    {group_id} is body named {vessel_name}")

            if mesh is not None:
                # Emit element connectivity in QPPL DIFF format
                # DIFF keyword marks elements as diffracting (required for panel method)
                # Format: "     1QPPL DIFF   15(1)(    1)(    2)(    3)(    4)"
                for elem_idx, panel in enumerate(mesh.panels, start=1):
                    # Panel contains 0-based indices, convert to 1-based
                    n1 = panel[0] + 1
                    n2 = panel[1] + 1
                    n3 = panel[2] + 1
                    n4 = panel[3] + 1
                    cards.append(
                        f"     {struct_idx}QPPL DIFF   {group_id}({struct_idx})"
                        f"({n1:>5d})({n2:>5d})({n3:>5d})({n4:>5d})"
                        f"  Aqwa Elem No: {elem_idx:>4d}"
                    )
            else:
                cards.append(
                    f"* WARNING: Mesh file '{mesh_file}' could not be loaded"
                )

            # PMAS element — point mass at CoG node (98000)
            pmas_group = 18
            cards.append(
                f"     {struct_idx}PMAS        {pmas_group}({struct_idx})"
                f"({98000:>5d})({98000:>5d})({98000:>5d})"
            )

        cards.append(" END")
        cards.append(f"{_WS:>10s}FINI")
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

    # -----------------------------------------------------------------
    # Deck 7 — Wave force spectrum (WFS1) — required but empty for LINE
    # -----------------------------------------------------------------

    def build_deck7(self, spec: DiffractionSpec) -> list[str]:
        """Build Deck 7: wave force spectrum (empty for diffraction)."""
        cards: list[str] = []
        cards.extend(_deck_banner(7))
        cards.append(f"{_WS:>10s}WFS1")
        cards.append(" END")
        return cards

    # -----------------------------------------------------------------
    # Deck 8 — Drift coefficient (DRC1) — required but empty for LINE
    # -----------------------------------------------------------------

    def build_deck8(self, spec: DiffractionSpec) -> list[str]:
        """Build Deck 8: drift coefficients (empty for diffraction)."""
        cards: list[str] = []
        cards.extend(_deck_banner(8))
        cards.append(f"{_WS:>10s}DRC1")
        cards.append("* No data defined for this structure")
        cards.append(" END")
        return cards

    @staticmethod
    def _build_none_deck(deck_num: int) -> list[str]:
        """Build an empty NONE deck placeholder."""
        return [f"{_WS:>10s}NONE"]

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

    @staticmethod
    def _expand_headings_for_aqwa(headings: list[float]) -> list[float]:
        """Expand heading range to -180..+180 for AQWA (no-symmetry bodies).

        AQWA requires the first heading to be -180 and the last to be +180.
        If the spec only covers 0..180, mirror the intermediate headings to
        the negative side to build a full -180..+180 range.
        """
        h_set = set(headings)
        # Mirror positive headings to negative side
        for h in headings:
            if 0 < h < 180:
                h_set.add(-h)
        # Ensure -180 and +180 are present
        h_set.discard(0)  # will re-add at correct position
        h_set.add(-180)
        h_set.add(0)
        h_set.add(180)
        return sorted(h_set)

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

        # Headings — AQWA requires -180 to +180 range for no-symmetry bodies
        headings = spec.wave_headings.to_heading_list()
        aqwa_headings = self._expand_headings_for_aqwa(headings)
        for i, heading_deg in enumerate(aqwa_headings, start=1):
            heading_str = f"{heading_deg:>10g}"
            cards.append(
                f"{_WS:>5s}1DIRN{i:>5d}{i:>5d}{heading_str}"
            )

        cards.append(" END")
        return cards
