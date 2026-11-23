#!/usr/bin/env python3
"""
ABOUTME: Parametric CFD study tooling for OpenFOAM. Generates case matrices
from parameter ranges, builds directory trees, and manages case naming.
"""

from __future__ import annotations

import itertools
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, Iterator, List, Optional, Tuple

import numpy as np

from loguru import logger

from .case_builder import OpenFOAMCaseBuilder
from .models import CaseType, OpenFOAMCase


# ============================================================================
# StudyParameter
# ============================================================================


@dataclass
class StudyParameter:
    """A single study parameter with discrete values.

    Attributes:
        name: Parameter identifier (used in case naming).
        values: List of discrete values for this parameter.
    """

    name: str
    values: List[Any]

    @property
    def n_values(self) -> int:
        """Number of discrete values."""
        return len(self.values)

    @classmethod
    def from_range(
        cls,
        name: str,
        start: float,
        stop: float,
        step: float,
    ) -> "StudyParameter":
        """Create a StudyParameter from a linear range.

        The range is inclusive of start and exclusive of values strictly
        greater than stop (numpy arange convention).

        Args:
            name: Parameter name.
            start: Range start (inclusive).
            stop: Range end (inclusive, rounded).
            step: Step size.

        Returns:
            StudyParameter with the generated values.
        """
        values = list(np.arange(start, stop + step * 0.5, step))
        # Round to avoid floating-point noise
        values = [round(v, 10) for v in values]
        return cls(name=name, values=values)


# ============================================================================
# ParametricStudy
# ============================================================================


@dataclass
class ParametricStudy:
    """Parametric CFD study: Cartesian product of study parameters.

    Generates a set of OpenFOAMCase configurations from a parameter
    matrix. All parameter combinations are expanded as a full factorial
    (Cartesian product).

    Attributes:
        case_type: Marine application type for all cases.
        study_name: Optional prefix for generated case names.
        parameters: Ordered list of StudyParameters.
    """

    case_type: CaseType
    study_name: str = ""
    parameters: List[StudyParameter] = field(default_factory=list)

    def add_parameter(self, param: StudyParameter) -> None:
        """Append a parameter to the study matrix."""
        self.parameters.append(param)

    def generate_cases(self) -> List[OpenFOAMCase]:
        """Generate one OpenFOAMCase per parameter combination.

        Returns:
            List of OpenFOAMCase objects, one per combination.

        Raises:
            ValueError: If no parameters have been added.
        """
        if not self.parameters:
            raise ValueError(
                "At least one parameter must be added before generating cases. "
                "Use add_parameter() with a StudyParameter."
            )

        # Build Cartesian product of all parameter values
        names = [p.name for p in self.parameters]
        value_lists = [p.values for p in self.parameters]

        cases: List[OpenFOAMCase] = []
        for combo in itertools.product(*value_lists):
            case_name = self._make_case_name(
                names, combo, self.study_name
            )
            base_case = OpenFOAMCase.for_case_type(self.case_type, case_name)
            # Attach parameter values as metadata
            base_case.metadata["study_parameters"] = dict(
                zip(names, combo)
            )
            cases.append(base_case)

        return cases

    def generate_directories(self, base_dir: Path) -> List[Path]:
        """Build case directory trees for all parameter combinations.

        Calls OpenFOAMCaseBuilder for each case, writing the full
        OpenFOAM directory structure under base_dir.

        Args:
            base_dir: Parent directory for all generated case directories.

        Returns:
            List of paths to created case directories.
        """
        base_dir = Path(base_dir)
        base_dir.mkdir(parents=True, exist_ok=True)

        cases = self.generate_cases()
        dirs: List[Path] = []

        for case in cases:
            builder = OpenFOAMCaseBuilder(case)
            case_dir = builder.build(base_dir)
            dirs.append(case_dir)
            logger.info(f"Generated parametric case: {case_dir}")

        return dirs

    # ------------------------------------------------------------------ #
    #  Private helpers                                                     #
    # ------------------------------------------------------------------ #

    @staticmethod
    def _make_case_name(
        param_names: List[str],
        values: Tuple[Any, ...],
        prefix: str,
    ) -> str:
        """Compose a case name from parameter names and values.

        Format:  [prefix_]<p1>_<v1>_<p2>_<v2>...

        Values are formatted as integers when they are whole numbers to
        keep names concise (e.g. 'speed_1' not 'speed_1.0').
        """
        parts: List[str] = []
        if prefix:
            parts.append(prefix)

        for name, val in zip(param_names, values):
            # Use integer representation when value is a whole number
            if isinstance(val, float) and val == int(val):
                val_str = str(int(val))
            else:
                val_str = str(val).replace(".", "p")
            parts.append(f"{name}_{val_str}")

        return "_".join(parts)
