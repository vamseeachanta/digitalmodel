"""Parametric study framework for OrcaFlex batch analysis.

Defines parameter sweeps (vary current speed, wave height, heading),
generates case matrices, organizes output directory structure, and
post-processes results into summary tables.

Does NOT require OrcFxAPI — generates configuration dicts and case matrices.

References:
    - API RP 2SK: Design and Analysis of Stationkeeping Systems (load cases)
    - DNV-OS-E301: Position Mooring (environmental combinations)
"""

import itertools
import json
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple, Union

import numpy as np
import pandas as pd
from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# Parameter definitions
# ---------------------------------------------------------------------------

class ParameterSweep(BaseModel):
    """Definition of a single parameter sweep.

    Specify either values (explicit list) or range (start, stop, step).
    """
    name: str = Field(..., description="Parameter name (e.g., 'wave_hs')")
    values: Optional[List[float]] = Field(None, description="Explicit list of values")
    start: Optional[float] = Field(None, description="Range start")
    stop: Optional[float] = Field(None, description="Range stop (inclusive)")
    step: Optional[float] = Field(None, description="Range step")
    unit: str = Field("", description="Unit label (e.g., 'm', 'deg')")

    def get_values(self) -> List[float]:
        """Return the list of parameter values.

        Returns:
            List of float values for the sweep.
        """
        if self.values is not None:
            return self.values
        if self.start is not None and self.stop is not None and self.step is not None:
            vals = np.arange(self.start, self.stop + self.step * 0.5, self.step)
            return [round(float(v), 6) for v in vals]
        raise ValueError(f"Parameter '{self.name}': provide either 'values' or 'start/stop/step'")


class ParametricStudy(BaseModel):
    """Parametric study definition.

    Generates a full-factorial or custom case matrix from parameter sweeps.
    """
    name: str = Field("parametric_study", description="Study name")
    parameters: List[ParameterSweep] = Field(
        default_factory=list,
        description="List of parameter sweeps",
    )
    base_config: Dict[str, Any] = Field(
        default_factory=dict,
        description="Base model configuration dict (template)",
    )
    output_dir: str = Field("./parametric_output", description="Root output directory")

    @property
    def num_cases(self) -> int:
        """Total number of cases in full-factorial matrix."""
        n = 1
        for p in self.parameters:
            n *= len(p.get_values())
        return n

    def generate_case_matrix(self) -> pd.DataFrame:
        """Generate full-factorial case matrix.

        Returns:
            DataFrame with one row per case and columns for each parameter.
        """
        param_names = [p.name for p in self.parameters]
        param_values = [p.get_values() for p in self.parameters]

        combinations = list(itertools.product(*param_values))

        df = pd.DataFrame(combinations, columns=param_names)
        df.insert(0, "case_id", [f"case_{i:04d}" for i in range(len(df))])
        return df

    def generate_case_configs(self) -> List[Dict[str, Any]]:
        """Generate individual case configurations.

        Each case is the base_config with parameter values injected.

        Returns:
            List of case configuration dicts.
        """
        matrix = self.generate_case_matrix()
        configs: List[Dict[str, Any]] = []

        for _, row in matrix.iterrows():
            config = dict(self.base_config)
            config["case_id"] = row["case_id"]
            for p in self.parameters:
                config[p.name] = row[p.name]
            configs.append(config)

        return configs

    def create_directory_structure(self) -> List[str]:
        """Create output directory structure.

        Returns:
            List of created directory paths.

        Note: Creates directories only if output_dir is writable.
        """
        matrix = self.generate_case_matrix()
        dirs: List[str] = []
        root = Path(self.output_dir)

        for _, row in matrix.iterrows():
            case_dir = root / row["case_id"]
            try:
                case_dir.mkdir(parents=True, exist_ok=True)
                dirs.append(str(case_dir))
            except OSError:
                dirs.append(str(case_dir))

        return dirs

    def export_case_matrix_csv(self, filepath: Optional[str] = None) -> str:
        """Export case matrix to CSV.

        Args:
            filepath: Output path. If None, uses output_dir/case_matrix.csv.

        Returns:
            Path to written CSV file.
        """
        if filepath is None:
            filepath = str(Path(self.output_dir) / "case_matrix.csv")

        df = self.generate_case_matrix()
        Path(filepath).parent.mkdir(parents=True, exist_ok=True)
        df.to_csv(filepath, index=False)
        return filepath


# ---------------------------------------------------------------------------
# Post-processing helpers
# ---------------------------------------------------------------------------

class CaseResult(BaseModel):
    """Results from a single parametric case."""
    case_id: str = Field(..., description="Case identifier")
    parameters: Dict[str, float] = Field(default_factory=dict, description="Parameter values")
    max_tension_kN: Optional[float] = Field(None, description="Max tension (kN)")
    max_bending_moment_kNm: Optional[float] = Field(None, description="Max bending moment (kN.m)")
    max_offset_m: Optional[float] = Field(None, description="Max vessel offset (m)")
    min_clearance_m: Optional[float] = Field(None, description="Min seabed clearance (m)")
    max_utilisation: Optional[float] = Field(None, description="Max code check utilisation")
    status: str = Field("pending", description="Case status: pending, completed, failed")
    notes: str = Field("", description="Additional notes")


class ParametricResultsSummary(BaseModel):
    """Summary of parametric study results.

    Provides methods to aggregate results and identify critical cases.
    """
    study_name: str = Field("", description="Study name")
    results: List[CaseResult] = Field(default_factory=list)

    def to_dataframe(self) -> pd.DataFrame:
        """Convert results to DataFrame.

        Returns:
            DataFrame with one row per case.
        """
        rows = []
        for r in self.results:
            row = {"case_id": r.case_id, "status": r.status}
            row.update(r.parameters)
            if r.max_tension_kN is not None:
                row["max_tension_kN"] = r.max_tension_kN
            if r.max_bending_moment_kNm is not None:
                row["max_bending_moment_kNm"] = r.max_bending_moment_kNm
            if r.max_offset_m is not None:
                row["max_offset_m"] = r.max_offset_m
            if r.min_clearance_m is not None:
                row["min_clearance_m"] = r.min_clearance_m
            if r.max_utilisation is not None:
                row["max_utilisation"] = r.max_utilisation
            rows.append(row)
        return pd.DataFrame(rows)

    def get_critical_case(self, metric: str = "max_tension_kN") -> Optional[CaseResult]:
        """Find the critical (worst) case for a given metric.

        Args:
            metric: Metric name to maximise.

        Returns:
            CaseResult with the highest value for the given metric.
        """
        completed = [r for r in self.results if r.status == "completed"]
        if not completed:
            return None

        def _get_val(r: CaseResult) -> float:
            v = getattr(r, metric, None)
            return v if v is not None else float("-inf")

        return max(completed, key=_get_val)

    def summary_statistics(self) -> Dict[str, Dict[str, float]]:
        """Calculate summary statistics across all completed cases.

        Returns:
            Dict of metric -> {min, max, mean, std}.
        """
        df = self.to_dataframe()
        df = df[df["status"] == "completed"]

        metrics = ["max_tension_kN", "max_bending_moment_kNm", "max_offset_m",
                    "min_clearance_m", "max_utilisation"]
        stats = {}

        for m in metrics:
            if m in df.columns and not df[m].isna().all():
                col = df[m].dropna()
                stats[m] = {
                    "min": round(float(col.min()), 3),
                    "max": round(float(col.max()), 3),
                    "mean": round(float(col.mean()), 3),
                    "std": round(float(col.std()), 3),
                }

        return stats
