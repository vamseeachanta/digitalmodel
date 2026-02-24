#!/usr/bin/env python
"""Batch spec extraction and modular generation for all OrcaFlex example models.

Discovers .dat files under docs/modules/orcaflex/examples/raw/, converts each
to a spec.yml via MonolithicExtractor, optionally generates modular YAML via
ModularModelGenerator, and optionally validates by running statics in OrcFxAPI.

Usage:
    uv run python scripts/generate_all_specs.py
    uv run python scripts/generate_all_specs.py --model "A01*"
    uv run python scripts/generate_all_specs.py --spec-only --max-models 5
    uv run python scripts/generate_all_specs.py --validate --model "A01*Catenary*"
"""

from __future__ import annotations

import argparse
import fnmatch
import re
import sys
import tempfile
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

import yaml

from digitalmodel.solvers.orcaflex.modular_generator import (
    ModularModelGenerator,
    _NoAliasDumper,
)
from digitalmodel.solvers.orcaflex.modular_generator.extractor import (
    MonolithicExtractor,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

# OrcFxAPI is optional -- validation requires it
try:
    import OrcFxAPI

    HAS_ORCFXAPI = True
except ImportError:
    HAS_ORCFXAPI = False

# Paths relative to project root
RAW_MODELS_ROOT = Path("docs/modules/orcaflex/examples/raw")
DEFAULT_OUTPUT_ROOT = Path("docs/modules/orcaflex/library/model_library")


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _sanitize_name(raw: str) -> str:
    """Convert a raw string to a snake_case identifier.

    Replaces non-alphanumeric characters with underscores, collapses
    consecutive underscores, and strips leading/trailing underscores.
    """
    cleaned = re.sub(r"[^a-zA-Z0-9]+", "_", raw)
    cleaned = re.sub(r"_+", "_", cleaned)
    return cleaned.strip("_").lower()


def _pct_diff(a: float, b: float) -> float:
    """Percentage difference between two values; handles zero denominators."""
    if abs(a) < 1e-9 and abs(b) < 1e-9:
        return 0.0
    denom = max(abs(a), abs(b))
    if denom < 1e-9:
        return 0.0
    return abs(a - b) / denom * 100.0


def _flush_print(msg: str) -> None:
    """Print with immediate flush for progress reporting."""
    print(msg, flush=True)


# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------


@dataclass
class ModelEntry:
    """Discovered .dat model file."""

    dat_path: Path
    sanitized_name: str
    display_name: str


@dataclass
class ValidationResult:
    """Result of monolithic vs modular statics comparison for one line."""

    line_name: str
    mono_end_a_kN: float
    mod_end_a_kN: float
    end_a_diff_pct: float
    mono_end_b_kN: float
    mod_end_b_kN: float
    end_b_diff_pct: float
    status: str  # PASS, WARN, FAIL


@dataclass
class ProcessResult:
    """Outcome of processing a single model."""

    model_name: str
    extracted: bool = False
    generated: bool = False
    validated: bool = False
    validation_results: list[ValidationResult] = field(default_factory=list)
    error: str | None = None
    extract_time_s: float = 0.0
    generate_time_s: float = 0.0
    validate_time_s: float = 0.0


# ---------------------------------------------------------------------------
# Discovery
# ---------------------------------------------------------------------------


def discover_models(raw_root: Path, name_glob: str | None = None) -> list[ModelEntry]:
    """Find all .dat files under the raw examples directory.

    Args:
        raw_root: Root directory containing example .dat files.
        name_glob: Optional glob pattern to filter by filename stem.

    Returns:
        Sorted list of ModelEntry objects.
    """
    dat_files = sorted(raw_root.rglob("*.dat"))
    entries: list[ModelEntry] = []

    for dat_path in dat_files:
        stem = dat_path.stem
        sanitized = _sanitize_name(stem)

        if name_glob and not fnmatch.fnmatch(stem, name_glob):
            # Also try matching against sanitized name
            if not fnmatch.fnmatch(sanitized, name_glob.lower()):
                continue

        entries.append(
            ModelEntry(
                dat_path=dat_path,
                sanitized_name=sanitized,
                display_name=stem,
            )
        )

    return entries


# ---------------------------------------------------------------------------
# Extraction
# ---------------------------------------------------------------------------


def extract_spec(dat_path: Path) -> dict[str, Any]:
    """Load a .dat file via OrcFxAPI, save to temp YAML, extract spec dict.

    Args:
        dat_path: Path to the OrcaFlex .dat binary model file.

    Returns:
        Spec dict from MonolithicExtractor.extract().

    Raises:
        RuntimeError: If OrcFxAPI is not available.
    """
    if not HAS_ORCFXAPI:
        raise RuntimeError(
            "OrcFxAPI is required to load .dat files. "
            "Install OrcFxAPI or provide pre-converted YAML files."
        )

    model = OrcFxAPI.Model(str(dat_path))

    with tempfile.NamedTemporaryFile(
        suffix=".yml", delete=False, mode="w"
    ) as tmp:
        tmp_path = Path(tmp.name)

    try:
        model.SaveData(str(tmp_path))
        extractor = MonolithicExtractor(tmp_path)
        spec_dict = extractor.extract()
    finally:
        tmp_path.unlink(missing_ok=True)

    return spec_dict


def save_spec_yml(spec_dict: dict[str, Any], output_path: Path) -> None:
    """Save spec dict to a YAML file using _NoAliasDumper.

    Args:
        spec_dict: Extracted spec dictionary.
        output_path: Destination path for spec.yml.
    """
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with open(output_path, "w", encoding="utf-8") as f:
        yaml.dump(
            spec_dict,
            f,
            Dumper=_NoAliasDumper,
            default_flow_style=False,
            allow_unicode=True,
            sort_keys=False,
        )


# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------


def validate_model(
    dat_path: Path, master_yml: Path
) -> list[ValidationResult]:
    """Compare monolithic and modular statics results.

    Loads both the original .dat and the generated master.yml in OrcFxAPI,
    runs statics on each, and compares Effective Tension at End A and End B
    for every Line object found in both models.

    Args:
        dat_path: Path to the original .dat model.
        master_yml: Path to the generated master.yml.

    Returns:
        List of ValidationResult, one per matched line.
    """
    if not HAS_ORCFXAPI:
        return []

    results: list[ValidationResult] = []

    # Load and solve monolithic
    mono_model = OrcFxAPI.Model(str(dat_path))
    try:
        mono_model.CalculateStatics()
    except OrcFxAPI.DLLError as e:
        _flush_print(f"      Monolithic statics failed: {e}")
        return []

    # Load and solve modular
    mod_model = OrcFxAPI.Model()
    mod_model.LoadData(str(master_yml))
    try:
        mod_model.CalculateStatics()
    except OrcFxAPI.DLLError as e:
        _flush_print(f"      Modular statics failed: {e}")
        return []

    # Collect line names from both models
    mono_lines: dict[str, Any] = {}
    for obj in mono_model.objects:
        if obj.type == OrcFxAPI.ObjectType.Line:
            mono_lines[obj.Name] = obj

    mod_lines: dict[str, Any] = {}
    for obj in mod_model.objects:
        if obj.type == OrcFxAPI.ObjectType.Line:
            mod_lines[obj.Name] = obj

    # Compare lines present in both models
    common_names = set(mono_lines.keys()) & set(mod_lines.keys())
    if not common_names:
        # Try matching by sanitized name
        mono_sanitized = {_sanitize_name(n): n for n in mono_lines}
        mod_sanitized = {_sanitize_name(n): n for n in mod_lines}
        for san_name in sorted(set(mono_sanitized.keys()) & set(mod_sanitized.keys())):
            common_names.add(mono_sanitized[san_name])

    for line_name in sorted(common_names):
        mono_line = mono_lines.get(line_name)
        # Try exact match first, then sanitized match
        mod_line = mod_lines.get(line_name)
        if mod_line is None:
            san = _sanitize_name(line_name)
            for mod_name, mod_obj in mod_lines.items():
                if _sanitize_name(mod_name) == san:
                    mod_line = mod_obj
                    break
        if mod_line is None or mono_line is None:
            continue

        mono_ea = mono_line.StaticResult("Effective Tension", OrcFxAPI.oeEndA)
        mono_eb = mono_line.StaticResult("Effective Tension", OrcFxAPI.oeEndB)
        mod_ea = mod_line.StaticResult("Effective Tension", OrcFxAPI.oeEndA)
        mod_eb = mod_line.StaticResult("Effective Tension", OrcFxAPI.oeEndB)

        diff_a = _pct_diff(mono_ea, mod_ea)
        diff_b = _pct_diff(mono_eb, mod_eb)
        max_diff = max(diff_a, diff_b)

        if max_diff < 1.0:
            status = "PASS"
        elif max_diff < 5.0:
            status = "WARN"
        else:
            status = "FAIL"

        results.append(
            ValidationResult(
                line_name=line_name,
                mono_end_a_kN=round(mono_ea, 3),
                mod_end_a_kN=round(mod_ea, 3),
                end_a_diff_pct=round(diff_a, 2),
                mono_end_b_kN=round(mono_eb, 3),
                mod_end_b_kN=round(mod_eb, 3),
                end_b_diff_pct=round(diff_b, 2),
                status=status,
            )
        )

    return results


# ---------------------------------------------------------------------------
# Per-model processing
# ---------------------------------------------------------------------------


def process_model(
    entry: ModelEntry,
    output_root: Path,
    spec_only: bool = False,
    do_validate: bool = False,
) -> ProcessResult:
    """Process a single model: extract, optionally generate, optionally validate.

    Args:
        entry: Model entry with paths and names.
        output_root: Root output directory.
        spec_only: If True, only extract spec.yml (no modular generation).
        do_validate: If True, run round-trip statics validation.

    Returns:
        ProcessResult with status and timing.
    """
    result = ProcessResult(model_name=entry.display_name)
    model_dir = output_root / entry.sanitized_name

    # Step 1: Extract spec
    _flush_print(f"  [1/3] Extracting spec from {entry.dat_path.name}...")
    t0 = time.time()
    try:
        spec_dict = extract_spec(entry.dat_path)
        result.extract_time_s = round(time.time() - t0, 2)
    except Exception as e:
        result.extract_time_s = round(time.time() - t0, 2)
        result.error = f"Extraction failed: {e}"
        _flush_print(f"    FAILED: {e}")
        return result

    # Save spec.yml
    spec_path = model_dir / "spec.yml"
    save_spec_yml(spec_dict, spec_path)
    result.extracted = True
    _flush_print(
        f"    Saved spec.yml ({result.extract_time_s}s, "
        f"{spec_path.stat().st_size} bytes)"
    )

    if spec_only:
        return result

    # Step 2: Validate spec with ProjectInputSpec and generate modular YAML
    _flush_print("  [2/3] Generating modular YAML...")
    t1 = time.time()
    try:
        spec = ProjectInputSpec(**spec_dict)
        generator = ModularModelGenerator.from_spec(spec)
        modular_dir = model_dir / "modular"
        generator.generate(modular_dir)
        result.generate_time_s = round(time.time() - t1, 2)
        result.generated = True

        include_files = list((modular_dir / "includes").glob("*.yml"))
        _flush_print(
            f"    Generated master.yml + {len(include_files)} includes "
            f"({result.generate_time_s}s)"
        )
    except Exception as e:
        result.generate_time_s = round(time.time() - t1, 2)
        _flush_print(f"    Generation failed: {e}")
        # Not a fatal error -- spec.yml was still saved
        result.error = f"Generation failed: {e}"

    # Step 3: Validation (optional)
    if do_validate and result.generated:
        _flush_print("  [3/3] Validating (monolithic vs modular statics)...")
        t2 = time.time()
        try:
            master_yml = model_dir / "modular" / "master.yml"
            result.validation_results = validate_model(
                entry.dat_path, master_yml
            )
            result.validate_time_s = round(time.time() - t2, 2)
            result.validated = True

            for vr in result.validation_results:
                _flush_print(
                    f"    {vr.line_name}: EA {vr.mono_end_a_kN:.1f} vs "
                    f"{vr.mod_end_a_kN:.1f} kN ({vr.end_a_diff_pct:.1f}%), "
                    f"EB {vr.mono_end_b_kN:.1f} vs "
                    f"{vr.mod_end_b_kN:.1f} kN ({vr.end_b_diff_pct:.1f}%) "
                    f"[{vr.status}]"
                )

            if not result.validation_results:
                _flush_print("    No matching lines found for comparison")
        except Exception as e:
            result.validate_time_s = round(time.time() - t2, 2)
            _flush_print(f"    Validation failed: {e}")
    elif do_validate and not result.generated:
        _flush_print("  [3/3] Skipped validation (no modular output)")

    return result


# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------


def print_summary(results: list[ProcessResult]) -> None:
    """Print final summary table."""
    total = len(results)
    extracted = sum(1 for r in results if r.extracted)
    generated = sum(1 for r in results if r.generated)
    validated = sum(1 for r in results if r.validated)
    failed = sum(1 for r in results if r.error is not None)

    w = 90
    _flush_print(f"\n{'=' * w}")
    _flush_print("SUMMARY")
    _flush_print(f"{'=' * w}")
    _flush_print(f"  Total models:   {total}")
    _flush_print(f"  Extracted:      {extracted}")
    _flush_print(f"  Generated:      {generated}")
    _flush_print(f"  Validated:      {validated}")
    _flush_print(f"  Failed:         {failed}")

    # Detail table
    _flush_print(f"\n{'-' * w}")
    _flush_print(
        f"{'Model':<45} {'Extract':<10} {'Generate':<10} "
        f"{'Validate':<10} {'Error':<15}"
    )
    _flush_print(f"{'-' * w}")

    for r in results:
        ext = f"{r.extract_time_s}s" if r.extracted else "FAIL"
        gen = f"{r.generate_time_s}s" if r.generated else ("--" if not r.extracted else "FAIL")
        val = f"{r.validate_time_s}s" if r.validated else "--"
        err = r.error[:15] if r.error else ""
        _flush_print(f"  {r.model_name:<43} {ext:<10} {gen:<10} {val:<10} {err:<15}")

    # Validation detail (if any)
    any_validated = any(r.validation_results for r in results)
    if any_validated:
        _flush_print(f"\n{'-' * w}")
        _flush_print("VALIDATION DETAIL")
        _flush_print(f"{'-' * w}")
        _flush_print(
            f"  {'Model':<35} {'Line':<25} {'EA Mono(kN)':<13} "
            f"{'EA Mod(kN)':<13} {'Diff%':<8} {'Status':<6}"
        )
        _flush_print(f"  {'-' * (w - 2)}")
        for r in results:
            for vr in r.validation_results:
                _flush_print(
                    f"  {r.model_name:<35} {vr.line_name:<25} "
                    f"{vr.mono_end_a_kN:<13.1f} {vr.mod_end_a_kN:<13.1f} "
                    f"{vr.end_a_diff_pct:<8.1f} {vr.status:<6}"
                )

    # Report failures
    failures = [r for r in results if r.error]
    if failures:
        _flush_print(f"\n{'-' * w}")
        _flush_print("FAILURES")
        _flush_print(f"{'-' * w}")
        for r in failures:
            _flush_print(f"  {r.model_name}: {r.error}")

    _flush_print(f"\n{'=' * w}")


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(
        description=(
            "Batch extract spec.yml and generate modular YAML for "
            "OrcaFlex example models."
        ),
    )
    parser.add_argument(
        "--model",
        type=str,
        default=None,
        help="Filter models by name glob (e.g. 'A01*', 'C05*')",
    )
    parser.add_argument(
        "--spec-only",
        action="store_true",
        help="Extract spec.yml only, skip modular generation",
    )
    parser.add_argument(
        "--validate",
        action="store_true",
        help="Run round-trip statics validation (requires OrcFxAPI)",
    )
    parser.add_argument(
        "--output-dir",
        type=str,
        default=str(DEFAULT_OUTPUT_ROOT),
        help=f"Output root directory (default: {DEFAULT_OUTPUT_ROOT})",
    )
    parser.add_argument(
        "--max-models",
        type=int,
        default=0,
        help="Limit number of models to process (0=all)",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    """Main entry point."""
    args = parse_args(argv)
    output_root = Path(args.output_dir)

    _flush_print("=" * 70)
    _flush_print("  OrcaFlex Model Library — Batch Spec Generator")
    _flush_print("=" * 70)

    # Check OrcFxAPI availability
    if not HAS_ORCFXAPI:
        _flush_print("\nWARNING: OrcFxAPI not available.")
        _flush_print("  - Cannot load .dat files (extraction requires OrcFxAPI)")
        if args.validate:
            _flush_print("  - --validate disabled (requires OrcFxAPI)")
            args.validate = False
        _flush_print("")

    if args.validate and not HAS_ORCFXAPI:
        _flush_print("ERROR: --validate requires OrcFxAPI")
        return 1

    # Discover models
    _flush_print(f"\nDiscovering models in: {RAW_MODELS_ROOT}")
    entries = discover_models(RAW_MODELS_ROOT, args.model)

    if args.max_models > 0:
        entries = entries[: args.max_models]

    _flush_print(f"Found {len(entries)} model(s)")
    if args.model:
        _flush_print(f"  Filter: {args.model}")
    if args.max_models > 0:
        _flush_print(f"  Limit: {args.max_models}")
    _flush_print(f"  Output: {output_root}")
    _flush_print(f"  Mode: {'spec-only' if args.spec_only else 'full'}")
    if args.validate:
        _flush_print("  Validation: enabled")

    if not entries:
        _flush_print("\nNo models found matching criteria.")
        return 0

    # Process each model
    results: list[ProcessResult] = []
    for i, entry in enumerate(entries, 1):
        _flush_print(f"\n{'—' * 70}")
        _flush_print(f"[{i}/{len(entries)}] {entry.display_name}")
        _flush_print(f"  Source: {entry.dat_path}")
        _flush_print(f"  Target: {output_root / entry.sanitized_name}")

        result = process_model(
            entry,
            output_root,
            spec_only=args.spec_only,
            do_validate=args.validate,
        )
        results.append(result)

    # Summary
    print_summary(results)

    failed_count = sum(1 for r in results if r.error is not None)
    return 1 if failed_count > 0 and failed_count == len(results) else 0


if __name__ == "__main__":
    sys.exit(main())
