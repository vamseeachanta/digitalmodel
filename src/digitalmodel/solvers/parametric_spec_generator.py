"""Parametric spec.yml generator for DiffractionSpec-compliant sweeps.

Creates multiple DiffractionSpec instances from a base spec definition
and one or more sweep dimensions (frequency, heading, hull parameters).

Usage:
    from digitalmodel.solvers.parametric_spec_generator import (
        ParametricSpecGenerator,
        FrequencySweep,
        HeadingSweep,
        HullParameterSweep,
    )

    gen = ParametricSpecGenerator(
        base_spec_data,
        frequency_sweeps=[FrequencySweep(start=0.1, end=2.0, steps=5)],
    )
    specs = gen.generate()          # List[DiffractionSpec]
    paths = gen.generate_to_yaml(output_dir)  # write YAML files

Issue: #1596
"""
from __future__ import annotations

import copy
import functools
import operator
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Optional

import numpy as np

from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec


# ---------------------------------------------------------------------------
# Sweep definitions
# ---------------------------------------------------------------------------


@dataclass
class FrequencySweep:
    """Parametric sweep over frequency ranges.

    Generates ``steps`` linearly-spaced frequency ranges between
    ``start`` and ``end`` (rad/s).  Each step produces one spec whose
    frequency list spans a sub-range of the full band.
    """
    start: float
    end: float
    steps: int

    def generate_ranges(self) -> list[list[float]]:
        """Return a list of frequency value lists, one per sweep step."""
        centers = np.linspace(self.start, self.end, self.steps)
        # Build a small band around each center (±10% of spacing)
        spacing = (self.end - self.start) / max(self.steps - 1, 1)
        half_band = max(spacing * 0.4, 0.05)
        ranges = []
        for c in centers:
            lo = max(c - half_band, 0.01)
            hi = c + half_band
            vals = list(np.linspace(lo, hi, 5))
            ranges.append([round(v, 6) for v in vals])
        return ranges


@dataclass
class HeadingSweep:
    """Parametric sweep over wave heading sets.

    ``values`` is a list of heading lists; each inner list defines the
    heading set for one spec.
    """
    values: list[list[float]]


@dataclass
class HullParameterSweep:
    """Parametric sweep over a hull/vessel parameter.

    ``parameter`` is a dot-separated path into the spec dict
    (e.g. 'vessel.inertia.mass').  ``values`` is the list of values
    to iterate over.
    """
    parameter: str
    values: list[Any]


# ---------------------------------------------------------------------------
# Generator
# ---------------------------------------------------------------------------


class ParametricSpecGenerator:
    """Generate DiffractionSpec instances from a base spec + sweep defs."""

    def __init__(
        self,
        base_spec_data: dict[str, Any],
        frequency_sweeps: Optional[list[FrequencySweep]] = None,
        heading_sweeps: Optional[list[HeadingSweep]] = None,
        hull_sweeps: Optional[list[HullParameterSweep]] = None,
    ) -> None:
        self._base = copy.deepcopy(base_spec_data)
        self._freq_sweeps = frequency_sweeps or []
        self._heading_sweeps = heading_sweeps or []
        self._hull_sweeps = hull_sweeps or []

    # ----- public API -----

    def generate(self) -> list[DiffractionSpec]:
        """Return a list of validated DiffractionSpec instances."""
        variants = self._build_variants()
        specs = []
        for i, data in enumerate(variants):
            # Ensure metadata tags include sweep info
            meta = data.setdefault("metadata", {})
            tags = list(meta.get("tags", []))
            if not any("sweep" in t.lower() or "parametric" in t.lower() for t in tags):
                tags.append(f"parametric-sweep-{i}")
            meta["tags"] = tags
            specs.append(DiffractionSpec(**data))
        return specs

    def generate_to_yaml(self, output_dir: Path) -> list[Path]:
        """Generate specs and write each to a YAML file in *output_dir*."""
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)
        specs = self.generate()
        paths = []
        for i, spec in enumerate(specs):
            fname = output_dir / f"spec_sweep_{i:03d}.yml"
            spec.to_yaml(fname)
            paths.append(fname)
        return paths

    # ----- internals -----

    def _build_variants(self) -> list[dict]:
        """Expand sweep dimensions into a list of spec data dicts."""
        # Start with the base spec
        variants = [copy.deepcopy(self._base)]

        # Apply frequency sweeps
        for fsweep in self._freq_sweeps:
            ranges = fsweep.generate_ranges()
            variants = self._expand(variants, "frequencies.values", ranges)

        # Apply heading sweeps
        for hsweep in self._heading_sweeps:
            variants = self._expand(
                variants, "wave_headings.values", hsweep.values
            )

        # Apply hull parameter sweeps
        for hsweep in self._hull_sweeps:
            variants = self._expand(
                variants, hsweep.parameter, hsweep.values
            )

        return variants

    @staticmethod
    def _expand(
        variants: list[dict],
        path: str,
        values: list,
    ) -> list[dict]:
        """Expand existing variants by one sweep dimension."""
        expanded = []
        for val in values:
            for base in variants:
                new = copy.deepcopy(base)
                _set_nested(new, path, val)
                expanded.append(new)
        return expanded


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _set_nested(d: dict, path: str, value: Any) -> None:
    """Set a value in a nested dict via a dot-separated path."""
    keys = path.split(".")
    for k in keys[:-1]:
        d = d.setdefault(k, {})
    d[keys[-1]] = value
