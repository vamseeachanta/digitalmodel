"""Polars-primary DataFrame exporter for diffraction results (WRK-030).

Exports DiffractionResults to long-format DataFrames and CSV files.
Polars is the primary engine; Pandas is available as a fallback.
"""
from __future__ import annotations

from pathlib import Path
from typing import Any

import numpy as np

from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    AddedMassSet,
    DampingSet,
    DiffractionResults,
    DOF,
    RAOSet,
)

try:
    import polars as pl

    POLARS_AVAILABLE = True
except ImportError:  # pragma: no cover
    POLARS_AVAILABLE = False

try:
    import pandas as pd

    PANDAS_AVAILABLE = True
except ImportError:  # pragma: no cover
    PANDAS_AVAILABLE = False

DOF_ORDER = [DOF.SURGE, DOF.SWAY, DOF.HEAVE, DOF.ROLL, DOF.PITCH, DOF.YAW]


class PolarsExporter:
    """Export DiffractionResults to Polars/Pandas DataFrames and CSV."""

    def __init__(self, results: DiffractionResults) -> None:
        self._results = results

    # ------------------------------------------------------------------
    # RAO export
    # ------------------------------------------------------------------

    def raos_to_polars(self) -> "pl.DataFrame":
        if not POLARS_AVAILABLE:
            msg = "polars is not installed"
            raise ImportError(msg)
        records = self._build_rao_records()
        return pl.DataFrame(records)

    def raos_to_pandas(self) -> "pd.DataFrame":
        if not PANDAS_AVAILABLE:
            msg = "pandas is not installed"
            raise ImportError(msg)
        records = self._build_rao_records()
        return pd.DataFrame(records)

    # ------------------------------------------------------------------
    # Matrix export (added mass / damping)
    # ------------------------------------------------------------------

    def added_mass_to_polars(self) -> "pl.DataFrame":
        if not POLARS_AVAILABLE:
            msg = "polars is not installed"
            raise ImportError(msg)
        return pl.DataFrame(self._build_matrix_records("added_mass"))

    def damping_to_polars(self) -> "pl.DataFrame":
        if not POLARS_AVAILABLE:
            msg = "polars is not installed"
            raise ImportError(msg)
        return pl.DataFrame(self._build_matrix_records("damping"))

    # ------------------------------------------------------------------
    # CSV export
    # ------------------------------------------------------------------

    def export_raos_csv(self, output_dir: Path) -> Path:
        output_dir.mkdir(parents=True, exist_ok=True)
        path = output_dir / f"{self._results.vessel_name}_raos.csv"
        if POLARS_AVAILABLE:
            self.raos_to_polars().write_csv(path)
        else:
            self.raos_to_pandas().to_csv(path, index=False)
        return path

    def export_added_mass_csv(self, output_dir: Path) -> Path:
        output_dir.mkdir(parents=True, exist_ok=True)
        path = output_dir / f"{self._results.vessel_name}_added_mass.csv"
        if POLARS_AVAILABLE:
            self.added_mass_to_polars().write_csv(path)
        else:
            records = self._build_matrix_records("added_mass")
            pd.DataFrame(records).to_csv(path, index=False)
        return path

    def export_damping_csv(self, output_dir: Path) -> Path:
        output_dir.mkdir(parents=True, exist_ok=True)
        path = output_dir / f"{self._results.vessel_name}_damping.csv"
        if POLARS_AVAILABLE:
            self.damping_to_polars().write_csv(path)
        else:
            records = self._build_matrix_records("damping")
            pd.DataFrame(records).to_csv(path, index=False)
        return path

    def export_all_csv(self, output_dir: Path) -> dict[str, Path]:
        return {
            "raos": self.export_raos_csv(output_dir),
            "added_mass": self.export_added_mass_csv(output_dir),
            "damping": self.export_damping_csv(output_dir),
        }

    # ------------------------------------------------------------------
    # Record builders
    # ------------------------------------------------------------------

    def _build_rao_records(self) -> list[dict[str, Any]]:
        records: list[dict[str, Any]] = []
        raos = self._results.raos
        for dof in DOF_ORDER:
            comp = raos.get_component(dof)
            freqs = comp.frequencies.values
            periods = comp.frequencies.periods
            headings = comp.headings.values
            for fi in range(len(freqs)):
                for hi in range(len(headings)):
                    records.append(
                        {
                            "frequency": float(freqs[fi]),
                            "period": float(periods[fi]),
                            "heading": float(headings[hi]),
                            "dof": dof.name,
                            "amplitude": float(comp.magnitude[fi, hi]),
                            "phase": float(comp.phase[fi, hi]),
                            "unit": comp.unit,
                        }
                    )
        return records

    def _build_matrix_records(self, matrix_type: str) -> list[dict[str, Any]]:
        matrix_set: AddedMassSet | DampingSet = (
            self._results.added_mass
            if matrix_type == "added_mass"
            else self._results.damping
        )
        records: list[dict[str, Any]] = []
        freqs = matrix_set.frequencies.values
        periods = matrix_set.frequencies.periods
        for idx, matrix in enumerate(matrix_set.matrices):
            freq = float(freqs[idx])
            period = float(periods[idx])
            for i in range(6):
                for j in range(6):
                    dof_i = DOF_ORDER[i]
                    dof_j = DOF_ORDER[j]
                    unit_key = (
                        "linear"
                        if i < 3 and j < 3
                        else "angular"
                        if i >= 3 and j >= 3
                        else "coupling"
                    )
                    unit = matrix.units.get(unit_key, "")
                    records.append(
                        {
                            "frequency": freq,
                            "period": period,
                            "dof_i": dof_i.name,
                            "dof_j": dof_j.name,
                            "value": float(matrix.matrix[i, j]),
                            "unit": unit,
                        }
                    )
        return records
