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

The Cartesian product is bounded by ``max_specs`` before expansion (default
from issue #1711). YAML names use a sanitized metadata description (or ``base``)
and zero-based axis/choice coordinates: ``f`` for frequency, ``h`` for heading,
and ``p`` for hull parameters. Coordinates distinguish repeated axis values.

Module CLI (no solver is invoked)::

    python -m digitalmodel.solvers.parametric_spec_generator \\
        --base spec.yml --freq-sweep 0.5:1.5:2 --heading-sweep 0:90:2 \\
        --output-dir output

Ranges use START:END:STEPS, inclusive endpoints; each heading step is a
singleton heading set. The existing frequency-band sampling rule is retained.

Issues: #1596, #1711
"""
from __future__ import annotations

import argparse
import copy
import math
import os
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Optional

import numpy as np
import yaml

from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec


DEFAULT_MAX_SPECS = 1000  # Issue #1711: reject accidental products above 1000.


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

    def __post_init__(self) -> None:
        """Reject malformed bands before allocating their samples."""
        _validate_range(self.start, self.end, self.steps)
        if self.start <= 0:
            raise ValueError("frequency start must be positive")

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
    """Generate a bounded Cartesian product using independent spec copies.

    ``max_specs`` is a positive integer, excluding booleans. The default is
    the limit specified in issue #1711. A product exceeding it raises
    ``ValueError`` before range generation, schema construction or output.
    """

    def __init__(
        self,
        base_spec_data: dict[str, Any],
        frequency_sweeps: Optional[list[FrequencySweep]] = None,
        heading_sweeps: Optional[list[HeadingSweep]] = None,
        hull_sweeps: Optional[list[HullParameterSweep]] = None,
        max_specs: int = DEFAULT_MAX_SPECS,
    ) -> None:
        _validate_max_specs(max_specs)
        self._max_specs = max_specs
        self._base = copy.deepcopy(base_spec_data)
        self._freq_sweeps = frequency_sweeps or []
        self._heading_sweeps = heading_sweeps or []
        self._hull_sweeps = hull_sweeps or []

    # ----- public API -----

    def generate(self) -> list[DiffractionSpec]:
        """Return a list of validated DiffractionSpec instances."""
        if not self._check_size():
            return []
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
        """Validate all specs before creating output, then write coordinate names.

        Filenames use the neutral metadata description, falling back to
        ``base``; schema fields and engineering values are unchanged.
        """
        specs = self.generate()
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)
        # Naming policy for #1711: neutral metadata, never a hull name.
        description = (self._base.get("metadata") or {}).get("description") or "base"
        slug = re.sub(r"[^a-z0-9]+", "-", str(description).lower()).strip("-")
        slug = slug or "base"
        # Local naming policy for #1711: 64 ASCII bytes, leaving coordinate space.
        slug = slug[:64].rstrip("-")
        axes = self._axes()
        paths = []
        for i, spec in enumerate(specs):
            remainder = i
            coordinates = []
            # _expand makes the first axis vary fastest; decode in that order.
            for label, length in axes:
                coordinates.append(f"{label}-{remainder % length}")
                remainder //= length
            suffix = "_".join(coordinates) or "base"
            fname = output_dir / f"spec_{slug}_{suffix}.yml"
            fname.write_text(
                yaml.safe_dump(spec.model_dump(mode="json", exclude_none=True),
                               default_flow_style=False, sort_keys=False),
                encoding="utf-8", newline="\n",
            )
            paths.append(fname)
        return paths

    # ----- internals -----

    def _axes(self) -> list[tuple[str, int]]:
        """Describe axis sizes without generating ranges or copying variants."""
        axes = []
        for i, sweep in enumerate(self._freq_sweeps):
            # Sweep dataclasses are mutable: revalidate at generation time.
            sweep.__post_init__()
            axes.append((f"f{i}", sweep.steps))
        axes.extend((f"h{i}", len(s.values)) for i, s in enumerate(self._heading_sweeps))
        axes.extend((f"p{i}", len(s.values)) for i, s in enumerate(self._hull_sweeps))
        return axes

    def _check_size(self) -> int:
        """Bound the entire product before the first call to _expand."""
        count = math.prod(length for _, length in self._axes())
        _check_spec_count(count, self._max_specs)
        return count

    def _build_variants(self) -> list[dict]:
        """Expand sweep dimensions into a list of spec data dicts."""
        # Start with the base spec
        variants = [copy.deepcopy(self._base)]

        # Apply frequency sweeps
        for fsweep in self._freq_sweeps:
            for data in variants:
                frequencies = data.setdefault("frequencies", {})
                frequencies["input_type"] = "frequency"
                frequencies.pop("range", None)
            ranges = fsweep.generate_ranges()
            variants = self._expand(variants, "frequencies.values", ranges)

        # Apply heading sweeps
        for hsweep in self._heading_sweeps:
            for data in variants:
                data.setdefault("wave_headings", {}).pop("range", None)
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
                _set_nested(new, path, copy.deepcopy(val))
                expanded.append(new)
        return expanded


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _rebase_mesh_paths(data: Any, source_dir: Path, output_dir: Path) -> None:
    """Keep every schema mesh referent when YAML moves to another directory."""
    if isinstance(data, dict):
        for key, value in data.items():
            if key == "mesh_file" and isinstance(value, str):
                target = (source_dir / value).resolve()
                data[key] = Path(os.path.relpath(target, output_dir)).as_posix()
            else:
                _rebase_mesh_paths(value, source_dir, output_dir)
    elif isinstance(data, list):
        for value in data:
            _rebase_mesh_paths(value, source_dir, output_dir)


def _set_nested(d: dict, path: str, value: Any) -> None:
    """Set a value in a nested dict via a dot-separated path."""
    keys = path.split(".")
    for k in keys[:-1]:
        d = d.setdefault(k, {})
    d[keys[-1]] = value


def _validate_max_specs(limit: int) -> None:
    """Require a positive integer; booleans are not cardinality limits."""
    if isinstance(limit, bool) or not isinstance(limit, int) or limit <= 0:
        raise ValueError("max_specs must be a positive integer")


def _check_spec_count(count: int, limit: int) -> None:
    """Share the allocation guard between the API and CLI range parsing."""
    _validate_max_specs(limit)
    if count > limit:
        raise ValueError(f"Sweep product {count} exceeds max_specs={limit}")


def _validate_range(start: float, end: float, steps: int) -> None:
    """Validate an inclusive linear range before any sample allocation."""
    if isinstance(steps, bool) or not isinstance(steps, int) or steps <= 0:
        raise ValueError("range steps must be a positive integer")
    if not math.isfinite(start) or not math.isfinite(end) or end < start:
        raise ValueError("range endpoints must be finite and nondecreasing")


def _parse_range(value: str) -> tuple[float, float, int]:
    """Parse START:END:STEPS without materializing the requested samples."""
    try:
        start_text, end_text, steps_text = value.split(":")
        start, end, steps = float(start_text), float(end_text), int(steps_text)
        _validate_range(start, end, steps)
        return start, end, steps
    except ValueError as exc:
        raise argparse.ArgumentTypeError(f"expected START:END:STEPS: {exc}") from exc


def main(argv: Optional[list[str]] = None) -> int:
    """Generate schema-validated YAML files from optional combined sweep axes."""
    parser = argparse.ArgumentParser(description="Generate bounded parametric spec YAML files.")
    parser.add_argument("--base", type=Path, required=True, help="Base spec YAML file")
    parser.add_argument("--freq-sweep", type=_parse_range, metavar="START:END:STEPS",
                        help="Frequency band centers in rad/s")
    parser.add_argument("--heading-sweep", type=_parse_range, metavar="START:END:STEPS",
                        help="Singleton headings in degrees, combined with frequency bands")
    parser.add_argument("--output-dir", type=Path, required=True, help="Directory for YAML files")
    parser.add_argument("--max-specs", type=int, default=DEFAULT_MAX_SPECS,
                        help=f"Maximum product size (default: {DEFAULT_MAX_SPECS})")
    args = parser.parse_args(argv)
    try:
        frequency = FrequencySweep(*args.freq_sweep) if args.freq_sweep else None
        # Count before np.linspace: an oversized heading axis must not allocate.
        count = (frequency.steps if frequency else 1) * (
            args.heading_sweep[2] if args.heading_sweep else 1
        )
        _check_spec_count(count, args.max_specs)
        headings = None
        if args.heading_sweep:
            start, end, steps = args.heading_sweep
            headings = HeadingSweep([[float(h)] for h in np.linspace(start, end, steps)])
        base = DiffractionSpec.model_validate(
            yaml.safe_load(args.base.read_text(encoding="utf-8"))
        ).model_dump(mode="json", exclude_none=True)
        _rebase_mesh_paths(base, args.base.resolve().parent, args.output_dir.resolve())
        generator = ParametricSpecGenerator(
            base, frequency_sweeps=[frequency] if frequency else None,
            heading_sweeps=[headings] if headings else None, max_specs=args.max_specs,
        )
        paths = generator.generate_to_yaml(args.output_dir)
    except (ValueError, OSError, yaml.YAMLError) as exc:
        parser.error(str(exc))
    print(f"Generated {len(paths)} specs.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
