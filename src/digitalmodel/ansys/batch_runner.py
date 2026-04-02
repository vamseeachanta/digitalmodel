# ABOUTME: Batch run configuration — generate APDL scripts with parameter variations
# ABOUTME: Creates batch launch scripts and output directory structures; no ANSYS needed

"""
Batch Runner
============

Generates multiple APDL scripts with systematic parameter variations for
parametric studies (e.g. wall thickness, pressure, temperature sweeps).

Features:

    - Parameter sweep definitions (linspace, list, combinatorial)
    - Batch script generation (.bat for Windows, .sh for Linux)
    - Output directory organization per run
    - Run matrix summary as DataFrame

This module generates text files only — it does NOT launch ANSYS.
"""

from __future__ import annotations

import itertools
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional

import numpy as np


# ---------------------------------------------------------------------------
# Configuration dataclasses
# ---------------------------------------------------------------------------

@dataclass
class ParameterSweep:
    """Definition of a single parameter sweep."""
    name: str
    unit: str = ""
    values: list[float] = field(default_factory=list)

    @classmethod
    def from_range(
        cls, name: str, start: float, stop: float, num: int = 5, unit: str = ""
    ) -> ParameterSweep:
        """Create a sweep from a linear range."""
        values = list(np.linspace(start, stop, num))
        return cls(name=name, unit=unit, values=values)

    @classmethod
    def from_list(cls, name: str, values: list[float], unit: str = "") -> ParameterSweep:
        """Create a sweep from explicit values."""
        return cls(name=name, unit=unit, values=values)


@dataclass
class BatchConfig:
    """Configuration for batch parametric run."""
    project_name: str = "PV_Parametric"
    base_template: str = ""  # APDL template with {param_name} placeholders
    sweeps: list[ParameterSweep] = field(default_factory=list)
    combinatorial: bool = True  # True=full factorial, False=zip
    ansys_executable: str = "mapdl"
    ansys_args: str = "-b -np 4"
    output_root: str = "batch_output"
    platform: str = "linux"  # linux or windows


@dataclass
class BatchRun:
    """Single run within a batch study."""
    run_id: int
    run_name: str
    parameters: dict[str, float]
    script_path: str
    output_dir: str
    apdl_content: str


# ---------------------------------------------------------------------------
# Batch runner
# ---------------------------------------------------------------------------

class BatchRunner:
    """Generate batch APDL scripts and launcher files.

    This class produces text content for scripts and batch files.
    It does NOT execute ANSYS or write to disk — callers decide where
    to save the generated content.
    """

    def generate_run_matrix(self, config: BatchConfig) -> list[dict[str, float]]:
        """Generate the parameter combination matrix.

        Returns
        -------
        list[dict[str, float]]
            Each dict maps parameter name to value for one run.
        """
        if not config.sweeps:
            return [{}]

        if config.combinatorial:
            all_values = [sweep.values for sweep in config.sweeps]
            names = [sweep.name for sweep in config.sweeps]
            combos = list(itertools.product(*all_values))
            return [dict(zip(names, combo)) for combo in combos]
        else:
            # Zip mode: all sweeps must have same length
            min_len = min(len(s.values) for s in config.sweeps)
            matrix = []
            for i in range(min_len):
                row = {s.name: s.values[i] for s in config.sweeps}
                matrix.append(row)
            return matrix

    def generate_runs(self, config: BatchConfig) -> list[BatchRun]:
        """Generate all batch runs with APDL scripts.

        Parameters
        ----------
        config : BatchConfig
            Batch run configuration including template and sweeps.

        Returns
        -------
        list[BatchRun]
            List of configured runs with APDL content.
        """
        matrix = self.generate_run_matrix(config)
        runs: list[BatchRun] = []

        for idx, params in enumerate(matrix):
            run_name = f"run_{idx:04d}"
            output_dir = f"{config.output_root}/{run_name}"
            script_path = f"{output_dir}/{config.project_name}.inp"

            # Substitute parameters into template
            apdl_content = config.base_template
            for pname, pval in params.items():
                apdl_content = apdl_content.replace(f"{{{pname}}}", str(pval))

            # Add run header
            param_str = ", ".join(f"{k}={v}" for k, v in params.items())
            header = (
                f"! Batch Run: {run_name}\n"
                f"! Parameters: {param_str}\n"
                f"! Project: {config.project_name}\n"
            )
            apdl_content = header + apdl_content

            runs.append(BatchRun(
                run_id=idx,
                run_name=run_name,
                parameters=params,
                script_path=script_path,
                output_dir=output_dir,
                apdl_content=apdl_content,
            ))

        return runs

    def generate_batch_script(
        self, config: BatchConfig, runs: list[BatchRun]
    ) -> str:
        """Generate a batch launcher script (.bat or .sh).

        Parameters
        ----------
        config : BatchConfig
            Batch configuration for platform and executable.
        runs : list[BatchRun]
            List of runs to include.

        Returns
        -------
        str
            Batch script content.
        """
        if config.platform.lower() == "windows":
            return self._generate_bat(config, runs)
        else:
            return self._generate_sh(config, runs)

    def generate_run_summary_csv(self, runs: list[BatchRun]) -> str:
        """Generate CSV summary of all runs and their parameters.

        Returns
        -------
        str
            CSV text content with run_id, run_name, parameters, paths.
        """
        if not runs:
            return "run_id,run_name\n"

        all_param_names = sorted(
            {k for run in runs for k in run.parameters.keys()}
        )
        header = ["run_id", "run_name"] + all_param_names + ["script_path", "output_dir"]
        lines = [",".join(header)]

        for run in runs:
            row = [str(run.run_id), run.run_name]
            for pname in all_param_names:
                row.append(str(run.parameters.get(pname, "")))
            row.extend([run.script_path, run.output_dir])
            lines.append(",".join(row))

        return "\n".join(lines) + "\n"

    # ------------------------------------------------------------------
    # Private helpers
    # ------------------------------------------------------------------

    def _generate_sh(self, config: BatchConfig, runs: list[BatchRun]) -> str:
        """Generate Linux bash launcher script."""
        lines = [
            "#!/bin/bash",
            f"# Batch launcher for {config.project_name}",
            f"# Total runs: {len(runs)}",
            "",
            f'MAPDL="{config.ansys_executable}"',
            f'ARGS="{config.ansys_args}"',
            "",
        ]
        for run in runs:
            lines.append(f"echo 'Starting {run.run_name}...'")
            lines.append(f"mkdir -p {run.output_dir}")
            lines.append(
                f'$MAPDL $ARGS -dir {run.output_dir} -i {run.script_path} '
                f'-o {run.output_dir}/solve.out'
            )
            lines.append("")
        lines.append("echo 'All runs complete.'")
        return "\n".join(lines) + "\n"

    def _generate_bat(self, config: BatchConfig, runs: list[BatchRun]) -> str:
        """Generate Windows batch launcher script."""
        lines = [
            "@echo off",
            f"REM Batch launcher for {config.project_name}",
            f"REM Total runs: {len(runs)}",
            "",
            f'SET MAPDL="{config.ansys_executable}"',
            f'SET ARGS={config.ansys_args}',
            "",
        ]
        for run in runs:
            out_dir = run.output_dir.replace("/", "\\")
            script = run.script_path.replace("/", "\\")
            lines.append(f"echo Starting {run.run_name}...")
            lines.append(f"if not exist {out_dir} mkdir {out_dir}")
            lines.append(
                f'%MAPDL% %ARGS% -dir {out_dir} -i {script} '
                f'-o {out_dir}\\solve.out'
            )
            lines.append("")
        lines.append("echo All runs complete.")
        return "\n".join(lines) + "\n"
