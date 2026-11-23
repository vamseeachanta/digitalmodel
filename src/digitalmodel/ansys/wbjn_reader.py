# ABOUTME: ANSYS Workbench IronPython journal (.wbjn) metadata reader
# ABOUTME: Extracts release version, system names, DP deletions, parameter expressions

"""
WBJN Reader
===========

Extracts key metadata from ANSYS Workbench IronPython journal files (.wbjn)
without executing or AST-parsing the script.

Extracts:
    release_version         — from ``# Release 16.0`` header comment
    system_names            — from ``GetSystem(Name="...")`` calls
    design_point_operations — from ``GetDesignPoint(Name="...").Delete()``
    parameter_expressions   — from ``SetParameterExpression(Parameter=...,
                                     Expression="...")``
"""

from __future__ import annotations

import re
from pathlib import Path

from digitalmodel.ansys.models import WBJNJournal

# --- compiled patterns ---
_RELEASE_RE = re.compile(r"^#\s*Release\s+([\d.]+)")
_SYSTEM_RE = re.compile(r'GetSystem\s*\(\s*Name\s*=\s*"([^"]+)"')
# Inline form: Parameters.GetDesignPoint(Name="3").Delete()
_DP_DELETE_INLINE_RE = re.compile(
    r'GetDesignPoint\s*\(\s*Name\s*=\s*"([^"]+)"\s*\)\s*\.Delete\s*\(\s*\)'
)
# Assignment form: var = Parameters.GetDesignPoint(Name="3")
_DP_ASSIGN_RE = re.compile(
    r'(\w+)\s*=\s*(?:Parameters\.)?GetDesignPoint\s*\(\s*Name\s*=\s*"([^"]+)"\s*\)'
)
# var.Delete()
_DELETE_VAR_RE = re.compile(r'(\w+)\s*\.\s*Delete\s*\(\s*\)')
_PARAM_EXPR_RE = re.compile(
    r'SetParameterExpression\s*\(\s*'
    r'Parameter\s*=\s*Parameters\.GetParameter\s*\(\s*Name\s*=\s*"([^"]+)"\s*\)'
    r'[^,]*,\s*Expression\s*=\s*"([^"]*)"'
)


class WBJNReader:
    """Extract metadata from an IronPython Workbench journal (.wbjn)."""

    def read_metadata(self, filepath: str | Path) -> WBJNJournal:
        """Parse a .wbjn file and return a WBJNJournal.

        Parameters
        ----------
        filepath : str or Path
            Path to the .wbjn journal file.

        Returns
        -------
        WBJNJournal with extracted metadata fields.
        """
        filepath = Path(filepath)
        release_version = ""
        system_names: list[str] = []
        dp_operations: list[str] = []
        param_expressions: list[tuple[str, str]] = []
        dp_var_map: dict[str, str] = {}  # variable_name → DP name

        # Multi-line SetParameterExpression spans up to 3 lines; accumulate
        # a small look-ahead buffer to handle them.
        lines: list[str] = filepath.read_text(
            encoding="utf-8", errors="replace"
        ).splitlines()

        # Join continuation lines for SetParameterExpression (lines ending
        # with open parenthesis or comma with no closing paren yet)
        joined = self._join_continuations(lines)

        for line in joined:
            stripped = line.strip()

            # Release version from header comment
            m = _RELEASE_RE.match(stripped)
            if m:
                release_version = m.group(1)
                continue

            # GetSystem calls
            for m in _SYSTEM_RE.finditer(stripped):
                name = m.group(1)
                if name not in system_names:
                    system_names.append(name)

            # Inline: Parameters.GetDesignPoint(Name="3").Delete()
            for m in _DP_DELETE_INLINE_RE.finditer(stripped):
                dp_name = m.group(1)
                op = f"Delete DP {dp_name}"
                if op not in dp_operations:
                    dp_operations.append(op)

            # Assignment form: var = ...GetDesignPoint(Name="X")
            m = _DP_ASSIGN_RE.search(stripped)
            if m:
                dp_var_map[m.group(1)] = m.group(2)

            # Deferred delete: var.Delete()
            m_del = _DELETE_VAR_RE.search(stripped)
            if m_del:
                var_name = m_del.group(1)
                if var_name in dp_var_map:
                    dp_name = dp_var_map[var_name]
                    op = f"Delete DP {dp_name}"
                    if op not in dp_operations:
                        dp_operations.append(op)

            # SetParameterExpression
            m = _PARAM_EXPR_RE.search(stripped)
            if m:
                param_name = m.group(1)
                expression = m.group(2)
                param_expressions.append((param_name, expression))

        return WBJNJournal(
            release_version=release_version,
            filepath=str(filepath),
            design_point_operations=dp_operations,
            system_names=system_names,
            parameter_expressions=param_expressions,
        )

    # ------------------------------------------------------------------
    # Private helpers
    # ------------------------------------------------------------------

    @staticmethod
    def _join_continuations(lines: list[str]) -> list[str]:
        """Join multi-line statements into single lines for easier parsing.

        A line is considered a continuation if it ends with ``(`` or ``,``
        and the overall parenthesis depth > 0.
        """
        result: list[str] = []
        buffer = ""
        depth = 0

        for line in lines:
            depth += line.count("(") - line.count(")")
            buffer = (buffer + " " + line).strip() if buffer else line
            if depth <= 0:
                result.append(buffer)
                buffer = ""
                depth = 0

        if buffer:
            result.append(buffer)
        return result
