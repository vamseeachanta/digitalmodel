# ABOUTME: ANSYS DesignXplorer DesignPointLog.csv reader
# ABOUTME: Parses parametric study design points and parameter labels

"""
Design Point Reader
===================

Parses ANSYS DesignXplorer ``DesignPointLog.csv`` files into a
:class:`ParametricStudy` containing all design points and their
parameter values.

File format:
    # comment lines (metadata: project name, parameter defs)
    Header row: Name, P1, P2, ...
    Data rows:  DP <n>, <float>, <float>, ...
    Embedded audit comment lines (lines starting with #) are ignored.
"""

from __future__ import annotations

import csv
import re
from pathlib import Path

from digitalmodel.ansys.models import DesignPoint, ParametricStudy

# Matches: # P1 - Force Z Component [N]
_PARAM_LABEL_RE = re.compile(r"^#\s*(P\d+)\s*[-–]\s*(.+)$")
# Matches: # Project name: SomeName
_PROJECT_NAME_RE = re.compile(r"^#\s*Project\s+name\s*:\s*(.+)$", re.IGNORECASE)


class DesignPointReader:
    """Parse ANSYS DesignXplorer DesignPointLog.csv files."""

    def read(self, filepath: str | Path) -> ParametricStudy:
        """Parse a DesignPointLog.csv file into a ParametricStudy.

        Parameters
        ----------
        filepath : str or Path
            Path to the DesignPointLog.csv file.

        Returns
        -------
        ParametricStudy with project_name, parameter_labels, design_points.
        creation_date is set to "" (not present in the log format).
        """
        filepath = Path(filepath)
        project_name = ""
        parameter_labels: dict[str, str] = {}
        header: list[str] = []
        design_points: list[DesignPoint] = []
        dp_index = 0

        with filepath.open(encoding="utf-8", errors="replace") as fh:
            for raw in fh:
                line = raw.rstrip("\n\r")

                # --- Comment / metadata lines ---
                if line.lstrip().startswith("#"):
                    m_proj = _PROJECT_NAME_RE.match(line.lstrip())
                    if m_proj:
                        project_name = m_proj.group(1).strip()
                        continue
                    m_param = _PARAM_LABEL_RE.match(line.lstrip())
                    if m_param:
                        parameter_labels[m_param.group(1)] = m_param.group(2).strip()
                    continue

                # --- Header row (Name, P1, P2, ...) ---
                stripped = line.strip()
                if not stripped:
                    continue
                if not header:
                    header = [h.strip() for h in stripped.split(",")]
                    continue

                # --- Data rows (DP <n>, ...) ---
                row = next(csv.reader([stripped]))
                row = [c.strip() for c in row]
                if not row or not row[0].upper().startswith("DP"):
                    continue

                name = row[0]
                params: dict[str, float] = {}
                for col_idx, col_name in enumerate(header[1:], start=1):
                    if col_idx < len(row):
                        try:
                            params[col_name] = float(row[col_idx])
                        except ValueError:
                            pass

                design_points.append(
                    DesignPoint(name=name, index=dp_index, parameters=params)
                )
                dp_index += 1

        return ParametricStudy(
            project_name=project_name,
            creation_date="",
            parameter_labels=parameter_labels,
            design_points=design_points,
        )
