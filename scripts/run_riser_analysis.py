#!/usr/bin/env python
"""End-to-End Riser Analysis: spec.yml -> Modular Model -> OrcaFlex -> Results.

This is the official pipeline for generating and analyzing OrcaFlex riser
models from YAML specifications. The workflow is:

    1. Load and validate spec.yml (Pydantic schema)
    2. Generate modular YAML output (master.yml + includes/)
    3. Load into OrcFxAPI
    4. Run statics analysis
    5. Extract engineering results (tensions, bending, curvature)
    6. Save structured JSON results + print summary

Usage:
    # Analyze a single spec
    uv run python scripts/run_riser_analysis.py docs/modules/orcaflex/library/tier2_fast/a01_catenary_riser/spec.yml

    # Specify output directory
    uv run python scripts/run_riser_analysis.py spec.yml --output results/my_run

    # Run all library specs
    uv run python scripts/run_riser_analysis.py --library

    # Include dynamics (requires simulation section in spec)
    uv run python scripts/run_riser_analysis.py spec.yml --dynamics
"""

from __future__ import annotations

import argparse
import json
import sys
import time
from dataclasses import asdict, dataclass, field
from pathlib import Path

try:
    import OrcFxAPI
except ImportError:
    print("ERROR: OrcFxAPI not available. Install OrcaFlex Python API.", file=sys.stderr)
    sys.exit(1)

import yaml

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

LIBRARY_ROOT = Path("docs/modules/orcaflex/library/tier2_fast")


# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------

@dataclass
class StaticsLineResult:
    """Statics results for a single line."""

    name: str
    total_length_m: float
    num_sections: int
    num_segments: int
    end_a_tension_kN: float
    end_b_tension_kN: float
    max_tension_kN: float
    min_tension_kN: float
    end_a_bending_kNm: float
    end_b_bending_kNm: float
    max_bending_kNm: float
    max_curvature_1_m: float


@dataclass
class StaticsLinkResult:
    """Statics results for a single link."""

    name: str
    link_type: str
    tension_kN: float


@dataclass
class AnalysisResults:
    """Complete analysis results for one spec."""

    spec_path: str
    model_name: str
    timestamp: str
    # Generation
    modular_dir: str
    object_count: int
    # Statics
    statics_converged: bool
    statics_time_seconds: float
    lines: list[StaticsLineResult] = field(default_factory=list)
    links: list[StaticsLinkResult] = field(default_factory=list)
    # Dynamics (optional)
    dynamics_ran: bool = False
    dynamics_time_seconds: float = 0.0
    dynamics_lines: dict = field(default_factory=dict)


# ---------------------------------------------------------------------------
# Core pipeline functions
# ---------------------------------------------------------------------------

def load_spec(spec_path: Path) -> ProjectInputSpec:
    """Step 1: Load and validate spec.yml."""
    with open(spec_path) as f:
        data = yaml.safe_load(f)
    return ProjectInputSpec(**data)


def generate_modular(spec: ProjectInputSpec, output_dir: Path) -> Path:
    """Step 2: Generate modular YAML output.

    Returns:
        Path to master.yml
    """
    output_dir.mkdir(parents=True, exist_ok=True)
    generator = ModularModelGenerator.from_spec(spec)
    generator.generate(output_dir)
    master = output_dir / "master.yml"
    if not master.exists():
        raise FileNotFoundError(f"master.yml not generated at {master}")
    return master


def load_model(master_path: Path) -> OrcFxAPI.Model:
    """Step 3: Load modular YAML into OrcFxAPI."""
    model = OrcFxAPI.Model()
    model.LoadData(str(master_path))
    return model


def run_statics(model: OrcFxAPI.Model) -> tuple[bool, float]:
    """Step 4: Run statics analysis.

    Returns:
        (converged, elapsed_seconds)
    """
    start = time.time()
    try:
        model.CalculateStatics()
        return True, round(time.time() - start, 3)
    except OrcFxAPI.DLLError as e:
        elapsed = round(time.time() - start, 3)
        print(f"  WARNING: Statics failed ({elapsed}s): {e}", file=sys.stderr)
        return False, elapsed


def extract_line_statics(model: OrcFxAPI.Model, line_name: str) -> StaticsLineResult:
    """Extract statics results from a single line."""
    line = model[line_name]
    n_sec = line.NumberOfSections
    total_len = sum(line.Length[i] for i in range(n_sec))
    total_seg = sum(line.NumberOfSegments[i] for i in range(n_sec))

    ea_t = line.StaticResult("Effective Tension", OrcFxAPI.oeEndA)
    eb_t = line.StaticResult("Effective Tension", OrcFxAPI.oeEndB)
    ea_b = line.StaticResult("Bend Moment", OrcFxAPI.oeEndA)
    eb_b = line.StaticResult("Bend Moment", OrcFxAPI.oeEndB)

    tension_rg = line.RangeGraph("Effective Tension")
    bending_rg = line.RangeGraph("Bend Moment")
    curvature_rg = line.RangeGraph("Curvature")

    return StaticsLineResult(
        name=line_name,
        total_length_m=round(total_len, 3),
        num_sections=n_sec,
        num_segments=total_seg,
        end_a_tension_kN=round(ea_t, 3),
        end_b_tension_kN=round(eb_t, 3),
        max_tension_kN=round(max(tension_rg.Mean), 3),
        min_tension_kN=round(min(tension_rg.Mean), 3),
        end_a_bending_kNm=round(ea_b, 3),
        end_b_bending_kNm=round(eb_b, 3),
        max_bending_kNm=round(max(abs(v) for v in bending_rg.Mean), 3),
        max_curvature_1_m=round(max(abs(v) for v in curvature_rg.Mean), 6),
    )


def extract_link_statics(model: OrcFxAPI.Model, link_name: str) -> StaticsLinkResult:
    """Extract statics results from a single link."""
    link = model[link_name]
    tension = link.StaticResult("Tension")
    return StaticsLinkResult(
        name=link_name,
        link_type=link.LinkType,
        tension_kN=round(tension, 3),
    )


def extract_results(model: OrcFxAPI.Model) -> tuple[list[StaticsLineResult], list[StaticsLinkResult]]:
    """Step 5: Extract engineering results from converged model."""
    lines = []
    links = []

    for obj in model.objects:
        if obj.type == OrcFxAPI.ObjectType.Line:
            lines.append(extract_line_statics(model, obj.Name))
        elif obj.type == OrcFxAPI.ObjectType.Link:
            links.append(extract_link_statics(model, obj.Name))

    return lines, links


def run_dynamics(model: OrcFxAPI.Model, spec: ProjectInputSpec) -> tuple[float, dict]:
    """Optional: Run dynamics simulation after statics.

    Returns:
        (elapsed_seconds, dynamics_results_dict)
    """
    sim = spec.simulation
    if sim is None:
        return 0.0, {}

    start = time.time()
    try:
        model.RunSimulation()
        elapsed = round(time.time() - start, 3)
    except OrcFxAPI.DLLError as e:
        elapsed = round(time.time() - start, 3)
        print(f"  WARNING: Dynamics failed ({elapsed}s): {e}", file=sys.stderr)
        return elapsed, {}

    # Extract dynamic results for each line
    results = {}
    period = OrcFxAPI.Period(OrcFxAPI.pnLatestWave)

    for obj in model.objects:
        if obj.type != OrcFxAPI.ObjectType.Line:
            continue

        try:
            ea_t = obj.TimeHistory("Effective Tension", period, OrcFxAPI.oeEndA)
            eb_t = obj.TimeHistory("Effective Tension", period, OrcFxAPI.oeEndB)

            results[obj.Name] = {
                "end_a_tension_max_kN": round(max(ea_t), 3),
                "end_a_tension_min_kN": round(min(ea_t), 3),
                "end_b_tension_max_kN": round(max(eb_t), 3),
                "end_b_tension_min_kN": round(min(eb_t), 3),
            }
        except OrcFxAPI.DLLError:
            pass

    return elapsed, results


# ---------------------------------------------------------------------------
# Main pipeline
# ---------------------------------------------------------------------------

def analyze_spec(
    spec_path: Path,
    output_dir: Path | None = None,
    run_dyn: bool = False,
) -> AnalysisResults:
    """Run the full spec.yml -> modular -> OrcaFlex -> results pipeline.

    Args:
        spec_path: Path to spec.yml.
        output_dir: Where to write modular output and results.
                    Defaults to spec_path.parent / "analysis".
        run_dyn: Whether to run dynamics after statics.

    Returns:
        AnalysisResults with all extracted data.
    """
    if output_dir is None:
        output_dir = spec_path.parent / "analysis"

    modular_dir = output_dir / "modular"
    timestamp = time.strftime("%Y-%m-%dT%H:%M:%S")

    print(f"{'='*70}")
    print(f"RISER ANALYSIS: {spec_path.name}")
    print(f"{'='*70}")

    # Step 1: Load spec
    print("  [1/5] Loading spec...")
    spec = load_spec(spec_path)
    model_name = spec.metadata.name
    print(f"        Model: {model_name}")

    if spec.riser:
        n_lines = len(spec.riser.lines)
        n_links = len(spec.riser.links)
        n_lt = len(spec.riser.line_types)
        print(f"        Lines: {n_lines}, Links: {n_links}, LineTypes: {n_lt}")

    # Step 2: Generate modular
    print("  [2/5] Generating modular model...")
    master = generate_modular(spec, modular_dir)
    print(f"        Output: {modular_dir}")

    # Step 3: Load into OrcFxAPI
    print("  [3/5] Loading into OrcFxAPI...")
    model = load_model(master)
    obj_count = len(model.objects)
    print(f"        Objects: {obj_count}")

    # Step 4: Run statics
    print("  [4/5] Running statics...")
    converged, statics_time = run_statics(model)
    status = "CONVERGED" if converged else "FAILED"
    print(f"        {status} ({statics_time}s)")

    results = AnalysisResults(
        spec_path=str(spec_path),
        model_name=model_name,
        timestamp=timestamp,
        modular_dir=str(modular_dir),
        object_count=obj_count,
        statics_converged=converged,
        statics_time_seconds=statics_time,
    )

    # Step 5: Extract results
    if converged:
        print("  [5/5] Extracting results...")
        line_results, link_results = extract_results(model)
        results.lines = line_results
        results.links = link_results
        print(f"        Lines: {len(line_results)}, Links: {len(link_results)}")
    else:
        print("  [5/5] Skipping extraction (statics failed)")

    # Optional: dynamics
    if run_dyn and converged and spec.simulation:
        print(f"  [DYN] Running dynamics (duration={spec.simulation.duration}s)...")
        dyn_time, dyn_results = run_dynamics(model, spec)
        results.dynamics_ran = True
        results.dynamics_time_seconds = dyn_time
        results.dynamics_lines = dyn_results
        print(f"        Done ({dyn_time}s)")

    # Save results
    results_path = output_dir / "results.json"
    results_path.parent.mkdir(parents=True, exist_ok=True)
    with open(results_path, "w") as f:
        json.dump(asdict(results), f, indent=2, default=str)
    print(f"\n  Saved: {results_path}")

    # Print summary table
    if results.lines:
        print(f"\n  {'Line':<30} {'Len(m)':<10} {'Segs':<8} "
              f"{'EA_T(kN)':<12} {'EB_T(kN)':<12} {'MaxT(kN)':<12} {'MaxB(kN.m)':<12}")
        print(f"  {'-'*96}")
        for lr in results.lines:
            print(f"  {lr.name:<30} {lr.total_length_m:<10.1f} {lr.num_segments:<8} "
                  f"{lr.end_a_tension_kN:<12.1f} {lr.end_b_tension_kN:<12.1f} "
                  f"{lr.max_tension_kN:<12.1f} {lr.max_bending_kNm:<12.2f}")

    if results.links:
        print(f"\n  {'Link':<30} {'Type':<15} {'Tension(kN)':<12}")
        print(f"  {'-'*57}")
        for lk in results.links:
            print(f"  {lk.name:<30} {lk.link_type:<15} {lk.tension_kN:<12.1f}")

    return results


def run_library() -> list[AnalysisResults]:
    """Run analysis on all spec.yml files in the riser library."""
    if not LIBRARY_ROOT.exists():
        print(f"ERROR: Library root not found: {LIBRARY_ROOT}", file=sys.stderr)
        return []

    specs = sorted(LIBRARY_ROOT.glob("*/spec.yml"))
    if not specs:
        print(f"ERROR: No spec.yml files found in {LIBRARY_ROOT}", file=sys.stderr)
        return []

    print(f"Found {len(specs)} specs in {LIBRARY_ROOT}\n")
    all_results = []

    for spec_path in specs:
        try:
            result = analyze_spec(spec_path)
            all_results.append(result)
        except Exception as e:
            print(f"ERROR: {spec_path.parent.name}: {e}", file=sys.stderr)

    # Print combined summary
    print(f"\n\n{'='*80}")
    print("LIBRARY ANALYSIS SUMMARY")
    print(f"{'='*80}")
    print(f"{'Model':<30} {'Status':<12} {'Time(s)':<10} {'Objects':<10} "
          f"{'MaxT(kN)':<12} {'MaxB(kN.m)':<12}")
    print(f"{'-'*80}")

    for r in all_results:
        status = "PASS" if r.statics_converged else "FAIL"
        max_t = max((lr.max_tension_kN for lr in r.lines), default=0)
        max_b = max((lr.max_bending_kNm for lr in r.lines), default=0)
        print(f"{r.model_name:<30} {status:<12} {r.statics_time_seconds:<10.2f} "
              f"{r.object_count:<10} {max_t:<12.1f} {max_b:<12.2f}")

    # Save combined summary
    summary_dir = Path("benchmark_output/library_analysis")
    summary_dir.mkdir(parents=True, exist_ok=True)
    summary_path = summary_dir / "library_results.json"
    with open(summary_path, "w") as f:
        json.dump([asdict(r) for r in all_results], f, indent=2, default=str)
    print(f"\nSaved: {summary_path}")

    return all_results


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main() -> int:
    parser = argparse.ArgumentParser(
        description="End-to-end riser analysis: spec.yml -> modular -> OrcaFlex -> results",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Single spec
  uv run python scripts/run_riser_analysis.py path/to/spec.yml

  # With custom output directory
  uv run python scripts/run_riser_analysis.py spec.yml --output results/run1

  # Run all library models
  uv run python scripts/run_riser_analysis.py --library

  # Include dynamics
  uv run python scripts/run_riser_analysis.py spec.yml --dynamics
""",
    )

    parser.add_argument(
        "spec",
        nargs="?",
        help="Path to spec.yml file",
    )
    parser.add_argument(
        "--output", "-o",
        help="Output directory (default: spec parent / analysis)",
    )
    parser.add_argument(
        "--library",
        action="store_true",
        help="Run analysis on all specs in the riser library",
    )
    parser.add_argument(
        "--dynamics",
        action="store_true",
        help="Run dynamics simulation after statics",
    )

    args = parser.parse_args()

    if args.library:
        results = run_library()
        all_pass = all(r.statics_converged for r in results)
        return 0 if all_pass else 1

    if not args.spec:
        parser.print_help()
        return 1

    spec_path = Path(args.spec)
    if not spec_path.exists():
        print(f"ERROR: Spec not found: {spec_path}", file=sys.stderr)
        return 1

    output_dir = Path(args.output) if args.output else None

    result = analyze_spec(spec_path, output_dir, run_dyn=args.dynamics)
    return 0 if result.statics_converged else 1


if __name__ == "__main__":
    sys.exit(main())
