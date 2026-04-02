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

import json
import csv
import io
from enum import Enum


class RunStatus(Enum):
    """Status of a batch run."""
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"


@dataclass
class LoadCase:
    """Single load case for batch FEA analysis."""
    name: str
    loads: dict[str, float] = field(default_factory=dict)
    boundary_conditions: dict[str, float] = field(default_factory=dict)
    description: str = ""


@dataclass
class BatchLoadConfig:
    """Batch configuration using load cases instead of parameter sweeps."""
    project_name: str = "FEA_Batch"
    base_model: str = ""  # APDL template with {load_name} placeholders
    load_cases: list[LoadCase] = field(default_factory=list)
    output_dir: str = "batch_output"
    ansys_executable: str = "mapdl"
    ansys_args: str = "-b -np 4"
    platform: str = "linux"


@dataclass
class BatchResultEntry:
    """Result for a single batch run."""
    load_case_name: str
    status: RunStatus = RunStatus.PENDING
    max_stress_mpa: Optional[float] = None
    max_displacement_mm: Optional[float] = None
    output_dir: str = ""
    error_message: str = ""
    converged: bool = False


@dataclass
class BatchResults:
    """Aggregated results from all batch runs."""
    project_name: str = ""
    entries: list[BatchResultEntry] = field(default_factory=list)
    total_runs: int = 0
    completed_runs: int = 0
    failed_runs: int = 0



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


    @staticmethod
    def parse_load_cases_yaml(yaml_text: str) -> list[LoadCase]:
        """Parse load case definitions from YAML text.

        Expected format::

            load_cases:
              - name: "Operating"
                loads:
                  pressure_mpa: 10.0
                  temperature_c: 200.0
                boundary_conditions:
                  fixed_end: 0.0

        Parameters
        ----------
        yaml_text : str
            YAML content defining load cases.

        Returns
        -------
        list[LoadCase]
            Parsed load cases.
        """
        # Simple YAML-like parser (no PyYAML dependency needed)
        # For robustness, we support a dict-based input too
        load_cases: list[LoadCase] = []
        current_case: dict[str, Any] = {}
        current_section: str = ""
        indent_base = 0

        for line in yaml_text.splitlines():
            stripped = line.strip()
            if not stripped or stripped.startswith("#"):
                continue

            # Detect load_cases list items
            if stripped.startswith("- name:"):
                if current_case.get("name"):
                    load_cases.append(LoadCase(
                        name=current_case["name"],
                        loads=current_case.get("loads", {}),
                        boundary_conditions=current_case.get("boundary_conditions", {}),
                        description=current_case.get("description", ""),
                    ))
                current_case = {"name": stripped.split(":", 1)[1].strip().strip('"').strip("'")}
                current_section = ""
            elif stripped.startswith("description:"):
                current_case["description"] = stripped.split(":", 1)[1].strip().strip('"').strip("'")
            elif stripped == "loads:":
                current_section = "loads"
                current_case.setdefault("loads", {})
            elif stripped == "boundary_conditions:":
                current_section = "boundary_conditions"
                current_case.setdefault("boundary_conditions", {})
            elif ":" in stripped and current_section:
                key, val = stripped.split(":", 1)
                key = key.strip()
                val = val.strip()
                try:
                    current_case.setdefault(current_section, {})[key] = float(val)
                except ValueError:
                    current_case.setdefault(current_section, {})[key] = val

        # Don't forget the last case
        if current_case.get("name"):
            load_cases.append(LoadCase(
                name=current_case["name"],
                loads=current_case.get("loads", {}),
                boundary_conditions=current_case.get("boundary_conditions", {}),
                description=current_case.get("description", ""),
            ))

        return load_cases

    def generate_batch_scripts_from_load_cases(
        self, config: BatchLoadConfig
    ) -> list[tuple[str, str]]:
        """Generate APDL scripts for each load case.

        Parameters
        ----------
        config : BatchLoadConfig
            Batch load case configuration.

        Returns
        -------
        list[tuple[str, str]]
            List of (script_path, apdl_content) tuples.
        """
        scripts: list[tuple[str, str]] = []

        for idx, lc in enumerate(config.load_cases):
            run_dir = f"{config.output_dir}/{lc.name.replace(' ', '_')}"
            script_path = f"{run_dir}/{config.project_name}.inp"

            # Start with header
            header = (
                f"! Load Case: {lc.name}\n"
                f"! Description: {lc.description}\n"
                f"! Project: {config.project_name}\n"
                f"! Run {idx + 1} of {len(config.load_cases)}\n"
                f"!\n"
            )

            # Substitute load values into template
            apdl_content = config.base_model
            for key, val in lc.loads.items():
                apdl_content = apdl_content.replace(f"{{{key}}}", str(val))
            for key, val in lc.boundary_conditions.items():
                apdl_content = apdl_content.replace(f"{{{key}}}", str(val))

            scripts.append((script_path, header + apdl_content))

        return scripts

    def collect_results_from_outputs(
        self, config: BatchLoadConfig, output_texts: dict[str, str]
    ) -> BatchResults:
        """Collect results from mock output text for each load case.

        Parameters
        ----------
        config : BatchLoadConfig
            Batch configuration.
        output_texts : dict[str, str]
            Mapping of load_case_name -> output text content.

        Returns
        -------
        BatchResults
            Aggregated results.
        """
        import re

        results = BatchResults(
            project_name=config.project_name,
            total_runs=len(config.load_cases),
        )

        max_seqv_re = re.compile(
            r"MAXIMUM\s+(?:VALUE|SEQV)\s*=?\s*([-\d.E+]+)", re.IGNORECASE
        )
        max_usum_re = re.compile(
            r"MAXIMUM\s+(?:USUM|DISPLACEMENT)\s*=?\s*([-\d.E+]+)", re.IGNORECASE
        )
        converged_re = re.compile(r"SOLUTION\s+(?:IS\s+)?CONVERGED", re.IGNORECASE)

        for lc in config.load_cases:
            entry = BatchResultEntry(
                load_case_name=lc.name,
                output_dir=f"{config.output_dir}/{lc.name.replace(' ', '_')}",
            )

            text = output_texts.get(lc.name, "")
            if not text:
                entry.status = RunStatus.FAILED
                entry.error_message = "No output found"
                results.failed_runs += 1
            else:
                m = max_seqv_re.search(text)
                if m:
                    entry.max_stress_mpa = float(m.group(1))
                m = max_usum_re.search(text)
                if m:
                    entry.max_displacement_mm = float(m.group(1))
                if converged_re.search(text):
                    entry.converged = True
                    entry.status = RunStatus.COMPLETED
                    results.completed_runs += 1
                else:
                    entry.status = RunStatus.FAILED
                    entry.error_message = "Solution did not converge"
                    results.failed_runs += 1

            results.entries.append(entry)

        return results

    def generate_summary_report(
        self, results: BatchResults, format: str = "csv"
    ) -> str:
        """Generate a summary report from batch results.

        Parameters
        ----------
        results : BatchResults
            Aggregated batch results.
        format : str
            Output format: 'csv' or 'json'.

        Returns
        -------
        str
            Report content as string.
        """
        if format == "json":
            data = {
                "project": results.project_name,
                "total_runs": results.total_runs,
                "completed": results.completed_runs,
                "failed": results.failed_runs,
                "cases": [],
            }
            for entry in results.entries:
                data["cases"].append({
                    "name": entry.load_case_name,
                    "status": entry.status.value,
                    "max_stress_mpa": entry.max_stress_mpa,
                    "max_displacement_mm": entry.max_displacement_mm,
                    "converged": entry.converged,
                    "error": entry.error_message,
                })
            return json.dumps(data, indent=2)
        else:
            # CSV format
            output = io.StringIO()
            writer = csv.writer(output)
            writer.writerow([
                "load_case", "status", "max_stress_mpa",
                "max_displacement_mm", "converged", "error",
            ])
            for entry in results.entries:
                writer.writerow([
                    entry.load_case_name,
                    entry.status.value,
                    entry.max_stress_mpa if entry.max_stress_mpa is not None else "",
                    entry.max_displacement_mm if entry.max_displacement_mm is not None else "",
                    entry.converged,
                    entry.error_message,
                ])
            return output.getvalue()

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
