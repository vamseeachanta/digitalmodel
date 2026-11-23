#!/usr/bin/env python3
"""
ABOUTME: OpenFOAM post-processing pipeline for extracting force time series,
free surface elevation, and probe point data from case output directories.
"""

from __future__ import annotations

import re
from pathlib import Path
from typing import List, Optional, Tuple

import numpy as np

from loguru import logger

from .results_models import ForceTimeSeries, FreeSurfaceElevation, ProbeTimeSeries


# ---------------------------------------------------------------------------
# OpenFOAMPostProcessor
# ---------------------------------------------------------------------------


class OpenFOAMPostProcessor:
    """Extract and aggregate post-processing data from an OpenFOAM case.

    All methods operate on plain text files written by OpenFOAM function
    objects. No OpenFOAM installation is required to use this class.

    Example::

        pp = OpenFOAMPostProcessor(case_dir=Path("my_case"))
        fts = pp.parse_force_file(Path("my_case/postProcessing/forces/0/force.dat"))
    """

    def __init__(self, case_dir: Path) -> None:
        self._case_dir = Path(case_dir)

    # ------------------------------------------------------------------ #
    #  Force file parsing                                                  #
    # ------------------------------------------------------------------ #

    def parse_force_file(self, force_file: Path) -> ForceTimeSeries:
        """Parse an OpenFOAM forces function object output file.

        Supports both the legacy (pre-v2306) and modern formats. Lines
        starting with '#' are treated as comments and skipped.

        Expected column order:
            time  Fp_x Fp_y Fp_z  Fv_x Fv_y Fv_z

        Args:
            force_file: Path to the force.dat / force(0).dat file.

        Returns:
            ForceTimeSeries with pressure and viscous components.

        Raises:
            FileNotFoundError: If force_file does not exist.
            ValueError: If the file cannot be parsed.
        """
        force_file = Path(force_file)
        if not force_file.exists():
            raise FileNotFoundError(f"Force file not found: {force_file}")

        times: List[float] = []
        fp_x: List[float] = []
        fp_y: List[float] = []
        fp_z: List[float] = []
        fv_x: List[float] = []
        fv_y: List[float] = []
        fv_z: List[float] = []

        for raw_line in force_file.read_text().splitlines():
            line = raw_line.strip()
            if not line or line.startswith("#"):
                continue

            # Strip OpenFOAM vector parentheses e.g. "(1.0 2.0 3.0)"
            line_clean = re.sub(r"[()]", " ", line)
            tokens = line_clean.split()

            if len(tokens) < 7:
                logger.debug(f"Skipping short line: {line!r}")
                continue

            try:
                times.append(float(tokens[0]))
                fp_x.append(float(tokens[1]))
                fp_y.append(float(tokens[2]))
                fp_z.append(float(tokens[3]))
                fv_x.append(float(tokens[4]))
                fv_y.append(float(tokens[5]))
                fv_z.append(float(tokens[6]))
            except ValueError:
                logger.debug(f"Skipping unparseable line: {line!r}")
                continue

        if not times:
            raise ValueError(
                f"No data rows found in force file: {force_file}"
            )

        return ForceTimeSeries(
            times=np.array(times, dtype=np.float64),
            fx=np.array(fp_x, dtype=np.float64),
            fy=np.array(fp_y, dtype=np.float64),
            fz=np.array(fp_z, dtype=np.float64),
            fx_viscous=np.array(fv_x, dtype=np.float64),
            fy_viscous=np.array(fv_y, dtype=np.float64),
            fz_viscous=np.array(fv_z, dtype=np.float64),
            patch_name=force_file.stem,
        )

    # ------------------------------------------------------------------ #
    #  Probe file parsing                                                  #
    # ------------------------------------------------------------------ #

    def parse_probe_file(
        self, probe_file: Path, field_name: str = ""
    ) -> ProbeTimeSeries:
        """Parse an OpenFOAM probes function object output file.

        Args:
            probe_file: Path to the probe output file.
            field_name: Field name for labelling (e.g. 'p', 'U').

        Returns:
            ProbeTimeSeries with values at each probe and timestep.
        """
        probe_file = Path(probe_file)
        if not probe_file.exists():
            raise FileNotFoundError(f"Probe file not found: {probe_file}")

        probe_coords: List[List[float]] = []
        times: List[float] = []
        value_rows: List[List[float]] = []

        for raw_line in probe_file.read_text().splitlines():
            line = raw_line.strip()

            # Probe coordinate header lines: "# Probe N (x y z)"
            probe_match = re.match(
                r"#\s*Probe\s+\d+\s+\(([^)]+)\)", line
            )
            if probe_match:
                coords = [float(v) for v in probe_match.group(1).split()]
                probe_coords.append(coords)
                continue

            if line.startswith("#") or not line:
                continue

            tokens = line.split()
            if len(tokens) < 2:
                continue

            try:
                times.append(float(tokens[0]))
                value_rows.append([float(t) for t in tokens[1:]])
            except ValueError:
                continue

        if not times:
            raise ValueError(
                f"No data rows found in probe file: {probe_file}"
            )

        values_array = np.array(value_rows, dtype=np.float64)

        # Ensure consistent probe coordinate list
        if not probe_coords:
            n_probes = values_array.shape[1] if values_array.ndim > 1 else 1
            probe_coords = [[0.0, 0.0, 0.0]] * n_probes

        return ProbeTimeSeries(
            times=np.array(times, dtype=np.float64),
            probe_coords=probe_coords,
            values=values_array,
            field_name=field_name or probe_file.stem,
        )

    # ------------------------------------------------------------------ #
    #  Free surface parsing                                                #
    # ------------------------------------------------------------------ #

    def extract_free_surface_from_probe(
        self,
        alpha_probe_file: Path,
        x_positions: Optional[np.ndarray] = None,
    ) -> FreeSurfaceElevation:
        """Extract free surface elevation from alpha.water probe data.

        Reads a probe file for the alpha.water field and identifies the
        interface (alpha = 0.5) to determine surface elevation at each
        probe column.

        Args:
            alpha_probe_file: Path to alpha.water probe output.
            x_positions: Optional explicit x-coordinates for each probe.

        Returns:
            FreeSurfaceElevation with time-varying surface elevation.
        """
        pts = self.parse_probe_file(alpha_probe_file, "alpha.water")

        if x_positions is None:
            x_positions = np.array(
                [c[0] for c in pts.probe_coords], dtype=np.float64
            )

        # For a simple scalar probe of alpha, values directly track
        # the interface (0=air, 1=water). Return values as proxy for eta.
        # A more rigorous extraction would locate the z-position where
        # alpha crosses 0.5 in a column of probes.
        return FreeSurfaceElevation(
            times=pts.times,
            x_positions=x_positions,
            elevations=pts.values,
        )
