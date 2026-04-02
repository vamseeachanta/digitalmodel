# ABOUTME: Extract results from ANSYS output text files (.rst summaries, element tables)
# ABOUTME: Pure text parsing into DataFrames; no ANSYS software required

"""
Results Extractor
=================

Parses ANSYS output text files to extract structural analysis results:

    - Result summary files (max stress, displacement, reaction forces)
    - Element table outputs (ETABLE printed via PRETAB)
    - Nodal solution outputs (PRNSOL output blocks)
    - Convergence history from solve.out

All parsing is regex-based on ANSYS text output formats.
Returns results as dataclasses and pandas DataFrames.
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from typing import Any, Optional

import json
import csv
import io

import pandas as pd



# ---------------------------------------------------------------------------
# Result dataclasses
# ---------------------------------------------------------------------------

@dataclass
class NodalResult:
    """Single nodal result value."""
    node_id: int
    x: float = 0.0
    y: float = 0.0
    z: float = 0.0
    value: float = 0.0
    component: str = ""


@dataclass
class ElementResult:
    """Single element result value."""
    element_id: int
    value: float = 0.0
    label: str = ""
    centroid_x: float = 0.0
    centroid_y: float = 0.0
    centroid_z: float = 0.0


@dataclass
class ResultSummary:
    """Summary of analysis results."""
    max_von_mises_mpa: Optional[float] = None
    max_von_mises_node: Optional[int] = None
    max_displacement_mm: Optional[float] = None
    max_displacement_node: Optional[int] = None
    max_principal_stress_mpa: Optional[float] = None
    min_principal_stress_mpa: Optional[float] = None
    total_reaction_force_n: Optional[float] = None
    total_nodes: int = 0
    total_elements: int = 0
    converged: bool = False
    num_substeps: int = 0


@dataclass
class ConvergenceRecord:
    """Single convergence iteration record."""
    substep: int
    iteration: int
    force_convergence: float
    moment_convergence: float = 0.0
    displacement_convergence: float = 0.0
    converged: bool = False



@dataclass
class StressSummary:
    """Summary of stress results for a single analysis."""
    max_von_mises_mpa: float = 0.0
    max_principal_mpa: float = 0.0
    min_principal_mpa: float = 0.0
    max_shear_mpa: float = 0.0
    location_node: int = 0


@dataclass
class DisplacementSummary:
    """Summary of displacement results for a single analysis."""
    max_total_mm: float = 0.0
    max_x_mm: float = 0.0
    max_y_mm: float = 0.0
    max_z_mm: float = 0.0
    location_node: int = 0


@dataclass
class ComparisonRow:
    """Single row in a multi-load-case comparison table."""
    load_case_name: str
    max_stress_mpa: float = 0.0
    max_displacement_mm: float = 0.0
    converged: bool = False
    total_nodes: int = 0
    total_elements: int = 0


# ---------------------------------------------------------------------------
# Regex patterns for ANSYS output parsing
# ---------------------------------------------------------------------------

# PRNSOL output: NODE    SX    SY    SZ    SXY   SYZ   SXZ
_PRNSOL_HEADER_RE = re.compile(r"^\s*NODE\s+", re.IGNORECASE)
_PRNSOL_DATA_RE = re.compile(
    r"^\s*(\d+)\s+([-\d.E+]+(?:\s+[-\d.E+]+)*)\s*$", re.IGNORECASE
)

# Summary line patterns
_MAX_SEQV_RE = re.compile(
    r"MAXIMUM\s+(?:VALUE|SEQV)\s*=?\s*([-\d.E+]+)\s+(?:AT\s+)?NODE\s*=?\s*(\d+)",
    re.IGNORECASE,
)
_MAX_USUM_RE = re.compile(
    r"MAXIMUM\s+(?:USUM|DISPLACEMENT)\s*=?\s*([-\d.E+]+)\s+(?:AT\s+)?NODE\s*=?\s*(\d+)",
    re.IGNORECASE,
)
_TOTAL_NODES_RE = re.compile(
    r"(?:TOTAL\s+)?NUMBER\s+OF\s+NODES\s*=?\s*(\d+)", re.IGNORECASE
)
_TOTAL_ELEMENTS_RE = re.compile(
    r"(?:TOTAL\s+)?NUMBER\s+OF\s+ELEMENTS\s*=?\s*(\d+)", re.IGNORECASE
)
_CONVERGED_RE = re.compile(r"SOLUTION\s+(?:IS\s+)?CONVERGED", re.IGNORECASE)
_SUBSTEP_RE = re.compile(
    r"SUBSTEP\s*=?\s*(\d+).*?CUM\.?\s*ITER\.?\s*=?\s*(\d+)", re.IGNORECASE
)

# PRETAB element table: ELEM  label1  label2 ...
_PRETAB_HEADER_RE = re.compile(r"^\s*ELEM\s+", re.IGNORECASE)
_PRETAB_DATA_RE = re.compile(r"^\s*(\d+)\s+([-\d.E+]+(?:\s+[-\d.E+]+)*)\s*$")

# Reaction force: TOTAL VALUES
_REACTION_TOTAL_RE = re.compile(
    r"TOTAL\s+VALUES?\s*[:=]?\s*([-\d.E+]+)", re.IGNORECASE
)


# ---------------------------------------------------------------------------
# Extractor class
# ---------------------------------------------------------------------------

class ResultsExtractor:
    """Extract and parse ANSYS output file results.

    All methods accept text content (str) rather than file paths,
    making them easy to test without file I/O.
    """

    def extract_summary(self, output_text: str) -> ResultSummary:
        """Extract a results summary from ANSYS output text.

        Parameters
        ----------
        output_text : str
            Content of ANSYS solve.out or result listing file.

        Returns
        -------
        ResultSummary
            Populated summary with extracted values.
        """
        summary = ResultSummary()

        m = _MAX_SEQV_RE.search(output_text)
        if m:
            summary.max_von_mises_mpa = float(m.group(1))
            summary.max_von_mises_node = int(m.group(2))

        m = _MAX_USUM_RE.search(output_text)
        if m:
            summary.max_displacement_mm = float(m.group(1))
            summary.max_displacement_node = int(m.group(2))

        m = _TOTAL_NODES_RE.search(output_text)
        if m:
            summary.total_nodes = int(m.group(1))

        m = _TOTAL_ELEMENTS_RE.search(output_text)
        if m:
            summary.total_elements = int(m.group(1))

        if _CONVERGED_RE.search(output_text):
            summary.converged = True

        m = _REACTION_TOTAL_RE.search(output_text)
        if m:
            summary.total_reaction_force_n = float(m.group(1))

        # Count substeps
        substeps = _SUBSTEP_RE.findall(output_text)
        if substeps:
            summary.num_substeps = max(int(s[0]) for s in substeps)

        return summary

    def parse_prnsol_block(
        self, text_block: str, component: str = "SEQV"
    ) -> pd.DataFrame:
        """Parse a PRNSOL output block into a DataFrame.

        Parameters
        ----------
        text_block : str
            Text block from PRNSOL output.
        component : str
            Result component name.

        Returns
        -------
        pd.DataFrame
            Columns: node_id, value.
        """
        records: list[dict] = []
        in_data = False

        for line in text_block.splitlines():
            if _PRNSOL_HEADER_RE.match(line):
                in_data = True
                continue
            if in_data:
                m = _PRNSOL_DATA_RE.match(line)
                if m:
                    node_id = int(m.group(1))
                    values = m.group(2).split()
                    # Take the last column as the primary value (often SEQV)
                    value = float(values[-1]) if values else 0.0
                    records.append({"node_id": node_id, "value": value})
                elif line.strip() == "" or line.strip().startswith("MINIMUM"):
                    in_data = False

        return pd.DataFrame(records, columns=["node_id", "value"])

    def parse_element_table(
        self, text_block: str
    ) -> pd.DataFrame:
        """Parse PRETAB element table output into a DataFrame.

        Parameters
        ----------
        text_block : str
            Text block from PRETAB output.

        Returns
        -------
        pd.DataFrame
            Columns: element_id, then one column per table label.
        """
        records: list[dict] = []
        headers: list[str] = []
        in_data = False

        for line in text_block.splitlines():
            if _PRETAB_HEADER_RE.match(line):
                headers = line.split()  # ELEM, label1, label2, ...
                in_data = True
                continue
            if in_data:
                m = _PRETAB_DATA_RE.match(line)
                if m:
                    elem_id = int(m.group(1))
                    values = [float(v) for v in m.group(2).split()]
                    row: dict[str, Any] = {"element_id": elem_id}
                    for i, val in enumerate(values):
                        col = headers[i + 1] if i + 1 < len(headers) else f"col_{i}"
                        row[col] = val
                    records.append(row)
                elif line.strip() == "":
                    in_data = False

        if records:
            return pd.DataFrame(records)
        return pd.DataFrame(columns=["element_id"])

    def parse_convergence_history(
        self, output_text: str
    ) -> list[ConvergenceRecord]:
        """Parse convergence iteration data from solve.out.

        Parameters
        ----------
        output_text : str
            Content of ANSYS solve.out file.

        Returns
        -------
        list[ConvergenceRecord]
            Convergence records for each iteration.
        """
        records: list[ConvergenceRecord] = []
        conv_pattern = re.compile(
            r"FORCE\s+CONVERGENCE\s+VALUE\s*=\s*([-\d.E+]+)\s+"
            r"CRITERION\s*=\s*([-\d.E+]+)",
            re.IGNORECASE,
        )

        current_substep = 0
        current_iter = 0

        for line in output_text.splitlines():
            m_sub = _SUBSTEP_RE.search(line)
            if m_sub:
                current_substep = int(m_sub.group(1))
                current_iter = int(m_sub.group(2))

            m_conv = conv_pattern.search(line)
            if m_conv:
                force_val = float(m_conv.group(1))
                criterion = float(m_conv.group(2))
                converged = abs(force_val) <= abs(criterion)
                records.append(ConvergenceRecord(
                    substep=current_substep,
                    iteration=current_iter,
                    force_convergence=force_val,
                    converged=converged,
                ))

        return records

    def extract_max_values(
        self, df: pd.DataFrame, value_col: str = "value"
    ) -> dict[str, float]:
        """Extract max/min statistics from a results DataFrame.

        Parameters
        ----------
        df : pd.DataFrame
            Results DataFrame with numeric value column.
        value_col : str
            Column name to analyze.

        Returns
        -------
        dict[str, float]
            Dictionary with max, min, mean, std values.
        """
        if df.empty or value_col not in df.columns:
            return {"max": 0.0, "min": 0.0, "mean": 0.0, "std": 0.0}

        return {
            "max": float(df[value_col].max()),
            "min": float(df[value_col].min()),
            "mean": float(df[value_col].mean()),
            "std": float(df[value_col].std()),
        }

    def extract_stress_summary(self, output_text: str) -> "StressSummary":
        """Extract stress summary from ANSYS output text.

        Parameters
        ----------
        output_text : str
            ANSYS output text content.

        Returns
        -------
        StressSummary
            Populated stress summary.
        """
        summary = StressSummary()
        m = _MAX_SEQV_RE.search(output_text)
        if m:
            summary.max_von_mises_mpa = float(m.group(1))
            summary.location_node = int(m.group(2))
        return summary

    def extract_displacements(self, output_text: str) -> "DisplacementSummary":
        """Extract displacement summary from ANSYS output text.

        Parameters
        ----------
        output_text : str
            ANSYS output text content.

        Returns
        -------
        DisplacementSummary
            Populated displacement summary.
        """
        summary = DisplacementSummary()
        m = _MAX_USUM_RE.search(output_text)
        if m:
            summary.max_total_mm = float(m.group(1))
            summary.location_node = int(m.group(2))
        return summary

    def compare_load_cases(
        self, results: list[tuple[str, str]]
    ) -> list["ComparisonRow"]:
        """Compare results across multiple load cases.

        Parameters
        ----------
        results : list[tuple[str, str]]
            List of (load_case_name, output_text) tuples.

        Returns
        -------
        list[ComparisonRow]
            Comparison table with one row per load case.
        """
        rows: list[ComparisonRow] = []
        for name, text in results:
            summary = self.extract_summary(text)
            rows.append(ComparisonRow(
                load_case_name=name,
                max_stress_mpa=summary.max_von_mises_mpa or 0.0,
                max_displacement_mm=summary.max_displacement_mm or 0.0,
                converged=summary.converged,
                total_nodes=summary.total_nodes,
                total_elements=summary.total_elements,
            ))
        return rows

    def export_results(
        self, summary: "ResultSummary", format: str = "json"
    ) -> str:
        """Export a ResultSummary to JSON or CSV string.

        Parameters
        ----------
        summary : ResultSummary
            Results summary to export.
        format : str
            Export format: 'json' or 'csv'.

        Returns
        -------
        str
            Formatted result string.
        """
        data = {
            "max_von_mises_mpa": summary.max_von_mises_mpa,
            "max_von_mises_node": summary.max_von_mises_node,
            "max_displacement_mm": summary.max_displacement_mm,
            "max_displacement_node": summary.max_displacement_node,
            "max_principal_stress_mpa": summary.max_principal_stress_mpa,
            "min_principal_stress_mpa": summary.min_principal_stress_mpa,
            "total_reaction_force_n": summary.total_reaction_force_n,
            "total_nodes": summary.total_nodes,
            "total_elements": summary.total_elements,
            "converged": summary.converged,
            "num_substeps": summary.num_substeps,
        }

        if format == "json":
            return json.dumps(data, indent=2)
        else:
            output = io.StringIO()
            writer = csv.DictWriter(output, fieldnames=data.keys())
            writer.writeheader()
            writer.writerow(data)
            return output.getvalue()

    def export_comparison(
        self, rows: list["ComparisonRow"], format: str = "json"
    ) -> str:
        """Export comparison table to JSON or CSV.

        Parameters
        ----------
        rows : list[ComparisonRow]
            Comparison table rows.
        format : str
            Export format: 'json' or 'csv'.

        Returns
        -------
        str
            Formatted comparison string.
        """
        if format == "json":
            data = []
            for row in rows:
                data.append({
                    "load_case": row.load_case_name,
                    "max_stress_mpa": row.max_stress_mpa,
                    "max_displacement_mm": row.max_displacement_mm,
                    "converged": row.converged,
                    "total_nodes": row.total_nodes,
                    "total_elements": row.total_elements,
                })
            return json.dumps(data, indent=2)
        else:
            output = io.StringIO()
            fieldnames = [
                "load_case", "max_stress_mpa", "max_displacement_mm",
                "converged", "total_nodes", "total_elements",
            ]
            writer = csv.DictWriter(output, fieldnames=fieldnames)
            writer.writeheader()
            for row in rows:
                writer.writerow({
                    "load_case": row.load_case_name,
                    "max_stress_mpa": row.max_stress_mpa,
                    "max_displacement_mm": row.max_displacement_mm,
                    "converged": row.converged,
                    "total_nodes": row.total_nodes,
                    "total_elements": row.total_elements,
                })
            return output.getvalue()

