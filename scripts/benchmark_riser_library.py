#!/usr/bin/env python
"""Benchmark: Monolithic vs Modular OrcaFlex Riser Models.

Generates modular output from each spec.yml, runs statics on both
monolithic and modular, extracts engineering results, and compares.

Saves structured comparison to validation/benchmark.json per model
and a combined summary to benchmark_output/riser_library_benchmark.json.

Usage:
    uv run python scripts/benchmark_riser_library.py
"""

from __future__ import annotations

import json
import time
from dataclasses import asdict, dataclass, field
from pathlib import Path

try:
    import OrcFxAPI
except ImportError:
    print("OrcFxAPI not available - cannot run benchmark")
    exit(1)

import yaml

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

LIBRARY_ROOT = Path("docs/modules/orcaflex/library/tier2_fast")
OUTPUT_ROOT = Path("benchmark_output")

# Line name mapping: monolithic name -> modular name (spec line name)
# These differ because the spec uses "10in" while monolithic uses '10"'
LINE_MAPPINGS = {
    "a01_catenary_riser": {
        "Catenary Hose": "Catenary Hose",
    },
    "a01_lazy_wave_riser": {
        '10" Lazy Wave Distributed': "10in Lazy Wave Distributed",
        '10" Lazy Wave Discrete': "10in Lazy Wave Discrete",
    },
    "a01_pliant_wave_riser": {
        '10" Pliant Simple': "10in Pliant Simple",
    },
    "a01_steep_wave_riser": {
        '10" Steep Wave1': "10in Steep Wave",
    },
}


@dataclass
class LineResults:
    """Engineering results extracted from a single line after statics."""

    line_name: str
    total_length_m: float
    num_sections: int
    end_a_tension_kN: float
    end_b_tension_kN: float
    end_a_bending_kNm: float
    end_b_bending_kNm: float
    max_tension_kN: float
    min_tension_kN: float
    max_bending_kNm: float


@dataclass
class ModelResults:
    """Results from a complete model (monolithic or modular)."""

    source_type: str  # "monolithic" or "modular"
    source_path: str
    statics_converged: bool
    statics_time_seconds: float
    object_count: int
    lines: dict[str, LineResults] = field(default_factory=dict)


@dataclass
class LineComparison:
    """Comparison of a single line between monolithic and modular."""

    line_name_mono: str
    line_name_mod: str
    end_a_tension_mono_kN: float
    end_a_tension_mod_kN: float
    end_a_tension_diff_pct: float
    end_b_tension_mono_kN: float
    end_b_tension_mod_kN: float
    end_b_tension_diff_pct: float
    max_tension_mono_kN: float
    max_tension_mod_kN: float
    max_tension_diff_pct: float
    end_a_bending_mono_kNm: float
    end_a_bending_mod_kNm: float
    end_b_bending_mono_kNm: float
    end_b_bending_mod_kNm: float
    max_bending_mono_kNm: float
    max_bending_mod_kNm: float
    length_mono_m: float
    length_mod_m: float
    length_diff_pct: float
    tension_pass: bool  # Within 5% or 10 kN
    bending_pass: bool  # Within 15% or 5 kN.m


@dataclass
class BenchmarkResult:
    """Complete benchmark result for a model."""

    model_name: str
    timestamp: str
    monolithic: ModelResults
    modular: ModelResults
    comparisons: list[LineComparison] = field(default_factory=list)
    overall_pass: bool = False


def pct_diff(a: float, b: float) -> float:
    """Calculate percentage difference; handle zero denominators."""
    if abs(a) < 1e-9 and abs(b) < 1e-9:
        return 0.0
    denom = max(abs(a), abs(b))
    if denom < 1e-9:
        return 0.0
    return abs(a - b) / denom * 100.0


def extract_line_results(model: OrcFxAPI.Model, line_name: str) -> LineResults:
    """Extract statics results from a named line in a converged model."""
    line = model[line_name]
    num_sections = line.NumberOfSections
    total_length = sum(line.Length[i] for i in range(num_sections))

    end_a_tension = line.StaticResult("Effective Tension", OrcFxAPI.oeEndA)
    end_b_tension = line.StaticResult("Effective Tension", OrcFxAPI.oeEndB)
    end_a_bending = line.StaticResult("Bend Moment", OrcFxAPI.oeEndA)
    end_b_bending = line.StaticResult("Bend Moment", OrcFxAPI.oeEndB)

    tension_rg = line.RangeGraph("Effective Tension")
    bending_rg = line.RangeGraph("Bend Moment")

    return LineResults(
        line_name=line_name,
        total_length_m=total_length,
        num_sections=num_sections,
        end_a_tension_kN=end_a_tension,
        end_b_tension_kN=end_b_tension,
        end_a_bending_kNm=end_a_bending,
        end_b_bending_kNm=end_b_bending,
        max_tension_kN=max(tension_rg.Mean),
        min_tension_kN=min(tension_rg.Mean),
        max_bending_kNm=max(abs(v) for v in bending_rg.Mean),
    )


def run_monolithic(model_dir: Path) -> ModelResults:
    """Load monolithic model, run statics, extract results."""
    mono_dir = model_dir / "monolithic"
    dat_files = list(mono_dir.glob("*.dat"))
    if not dat_files:
        raise FileNotFoundError(f"No .dat file in {mono_dir}")

    mono_path = dat_files[0]
    print(f"  Loading monolithic: {mono_path.name}")

    model = OrcFxAPI.Model(str(mono_path))
    obj_count = len(model.objects)

    start = time.time()
    try:
        model.CalculateStatics()
        elapsed = time.time() - start
        converged = True
    except OrcFxAPI.DLLError as e:
        elapsed = time.time() - start
        print(f"  Monolithic statics FAILED: {e}")
        converged = False

    results = ModelResults(
        source_type="monolithic",
        source_path=str(mono_path),
        statics_converged=converged,
        statics_time_seconds=round(elapsed, 3),
        object_count=obj_count,
    )

    if converged:
        lines = [o for o in model.objects if o.type == OrcFxAPI.ObjectType.Line]
        for line in lines:
            lr = extract_line_results(model, line.Name)
            results.lines[line.Name] = lr

    return results


def run_modular(model_dir: Path) -> ModelResults:
    """Generate modular output, load in OrcFxAPI, run statics, extract."""
    spec_path = model_dir / "spec.yml"
    if not spec_path.exists():
        raise FileNotFoundError(f"Spec not found: {spec_path}")

    with open(spec_path) as f:
        data = yaml.safe_load(f)
    spec = ProjectInputSpec(**data)

    # Generate modular output to a temp directory within the model dir
    modular_dir = model_dir / "modular"
    modular_dir.mkdir(parents=True, exist_ok=True)

    print(f"  Generating modular from: {spec_path.name}")
    generator = ModularModelGenerator.from_spec(spec)
    generator.generate(modular_dir)

    master = modular_dir / "master.yml"
    if not master.exists():
        raise FileNotFoundError(f"master.yml not generated: {master}")

    print(f"  Loading modular: master.yml")
    model = OrcFxAPI.Model()
    model.LoadData(str(master))
    obj_count = len(model.objects)

    start = time.time()
    try:
        model.CalculateStatics()
        elapsed = time.time() - start
        converged = True
    except OrcFxAPI.DLLError as e:
        elapsed = time.time() - start
        print(f"  Modular statics FAILED: {e}")
        converged = False

    results = ModelResults(
        source_type="modular",
        source_path=str(master),
        statics_converged=converged,
        statics_time_seconds=round(elapsed, 3),
        object_count=obj_count,
    )

    if converged:
        lines = [o for o in model.objects if o.type == OrcFxAPI.ObjectType.Line]
        for line in lines:
            lr = extract_line_results(model, line.Name)
            results.lines[line.Name] = lr

    return results


def compare_lines(
    mono_lr: LineResults, mod_lr: LineResults
) -> LineComparison:
    """Compare monolithic and modular line results."""
    ea_t_diff = pct_diff(mono_lr.end_a_tension_kN, mod_lr.end_a_tension_kN)
    eb_t_diff = pct_diff(mono_lr.end_b_tension_kN, mod_lr.end_b_tension_kN)
    max_t_diff = pct_diff(mono_lr.max_tension_kN, mod_lr.max_tension_kN)
    len_diff = pct_diff(mono_lr.total_length_m, mod_lr.total_length_m)

    # Pass criteria: within 5% relative OR 10 kN absolute
    tension_pass = all([
        ea_t_diff < 5.0 or abs(mono_lr.end_a_tension_kN - mod_lr.end_a_tension_kN) < 10.0,
        eb_t_diff < 5.0 or abs(mono_lr.end_b_tension_kN - mod_lr.end_b_tension_kN) < 10.0,
    ])

    # Bending: within 15% relative OR 5 kN.m absolute
    bending_pass = all([
        pct_diff(mono_lr.end_a_bending_kNm, mod_lr.end_a_bending_kNm) < 15.0
        or abs(mono_lr.end_a_bending_kNm - mod_lr.end_a_bending_kNm) < 5.0,
        pct_diff(mono_lr.end_b_bending_kNm, mod_lr.end_b_bending_kNm) < 15.0
        or abs(mono_lr.end_b_bending_kNm - mod_lr.end_b_bending_kNm) < 5.0,
    ])

    return LineComparison(
        line_name_mono=mono_lr.line_name,
        line_name_mod=mod_lr.line_name,
        end_a_tension_mono_kN=round(mono_lr.end_a_tension_kN, 3),
        end_a_tension_mod_kN=round(mod_lr.end_a_tension_kN, 3),
        end_a_tension_diff_pct=round(ea_t_diff, 2),
        end_b_tension_mono_kN=round(mono_lr.end_b_tension_kN, 3),
        end_b_tension_mod_kN=round(mod_lr.end_b_tension_kN, 3),
        end_b_tension_diff_pct=round(eb_t_diff, 2),
        max_tension_mono_kN=round(mono_lr.max_tension_kN, 3),
        max_tension_mod_kN=round(mod_lr.max_tension_kN, 3),
        max_tension_diff_pct=round(max_t_diff, 2),
        end_a_bending_mono_kNm=round(mono_lr.end_a_bending_kNm, 3),
        end_a_bending_mod_kNm=round(mod_lr.end_a_bending_kNm, 3),
        end_b_bending_mono_kNm=round(mono_lr.end_b_bending_kNm, 3),
        end_b_bending_mod_kNm=round(mod_lr.end_b_bending_kNm, 3),
        max_bending_mono_kNm=round(mono_lr.max_bending_kNm, 3),
        max_bending_mod_kNm=round(mod_lr.max_bending_kNm, 3),
        length_mono_m=mono_lr.total_length_m,
        length_mod_m=mod_lr.total_length_m,
        length_diff_pct=round(len_diff, 4),
        tension_pass=tension_pass,
        bending_pass=bending_pass,
    )


def benchmark_model(model_name: str) -> BenchmarkResult | None:
    """Run full benchmark for a single model."""
    model_dir = LIBRARY_ROOT / model_name
    if not model_dir.exists():
        print(f"SKIP {model_name}: directory not found")
        return None

    print(f"\n{'='*60}")
    print(f"BENCHMARKING: {model_name}")
    print(f"{'='*60}")

    timestamp = time.strftime("%Y-%m-%dT%H:%M:%S")

    # Run monolithic
    mono_results = run_monolithic(model_dir)
    print(f"  Monolithic: {'CONVERGED' if mono_results.statics_converged else 'FAILED'}"
          f" ({mono_results.statics_time_seconds}s, {mono_results.object_count} objects)")

    # Run modular
    mod_results = run_modular(model_dir)
    print(f"  Modular:    {'CONVERGED' if mod_results.statics_converged else 'FAILED'}"
          f" ({mod_results.statics_time_seconds}s, {mod_results.object_count} objects)")

    benchmark = BenchmarkResult(
        model_name=model_name,
        timestamp=timestamp,
        monolithic=mono_results,
        modular=mod_results,
    )

    # Compare matched lines
    if mono_results.statics_converged and mod_results.statics_converged:
        mapping = LINE_MAPPINGS.get(model_name, {})
        for mono_name, mod_name in mapping.items():
            if mono_name in mono_results.lines and mod_name in mod_results.lines:
                comp = compare_lines(
                    mono_results.lines[mono_name],
                    mod_results.lines[mod_name],
                )
                benchmark.comparisons.append(comp)

                status = "PASS" if (comp.tension_pass and comp.bending_pass) else "FAIL"
                print(f"\n  Line comparison: {mono_name} vs {mod_name}")
                print(f"    End A tension: {comp.end_a_tension_mono_kN:.1f} vs "
                      f"{comp.end_a_tension_mod_kN:.1f} kN ({comp.end_a_tension_diff_pct:.1f}%)")
                print(f"    End B tension: {comp.end_b_tension_mono_kN:.1f} vs "
                      f"{comp.end_b_tension_mod_kN:.1f} kN ({comp.end_b_tension_diff_pct:.1f}%)")
                print(f"    Max tension:   {comp.max_tension_mono_kN:.1f} vs "
                      f"{comp.max_tension_mod_kN:.1f} kN ({comp.max_tension_diff_pct:.1f}%)")
                print(f"    Max bending:   {comp.max_bending_mono_kNm:.2f} vs "
                      f"{comp.max_bending_mod_kNm:.2f} kN.m")
                print(f"    Length:        {comp.length_mono_m:.1f} vs "
                      f"{comp.length_mod_m:.1f} m ({comp.length_diff_pct:.3f}%)")
                print(f"    Result: {status}")

        benchmark.overall_pass = all(
            c.tension_pass and c.bending_pass for c in benchmark.comparisons
        )

    # Save per-model benchmark
    bench_path = model_dir / "validation" / "benchmark.json"
    bench_path.parent.mkdir(parents=True, exist_ok=True)
    with open(bench_path, "w") as f:
        json.dump(asdict(benchmark), f, indent=2, default=str)
    print(f"\n  Saved: {bench_path}")

    return benchmark


def main() -> int:
    """Run benchmark across all riser models in library."""
    models = [
        "a01_catenary_riser",
        "a01_lazy_wave_riser",
        "a01_pliant_wave_riser",
        "a01_steep_wave_riser",
    ]

    results = []
    for model_name in models:
        result = benchmark_model(model_name)
        if result:
            results.append(result)

    # Save combined summary
    OUTPUT_ROOT.mkdir(parents=True, exist_ok=True)
    summary_path = OUTPUT_ROOT / "riser_library_benchmark.json"
    with open(summary_path, "w") as f:
        json.dump([asdict(r) for r in results], f, indent=2, default=str)

    # Print summary table
    print(f"\n\n{'='*80}")
    print("BENCHMARK SUMMARY — Monolithic vs Modular Riser Models")
    print(f"{'='*80}")
    print(f"{'Model':<30} {'Mono (s)':<10} {'Mod (s)':<10} {'Lines':<8} {'Status':<10}")
    print(f"{'-'*80}")

    for r in results:
        mono_t = f"{r.monolithic.statics_time_seconds:.2f}" if r.monolithic.statics_converged else "FAIL"
        mod_t = f"{r.modular.statics_time_seconds:.2f}" if r.modular.statics_converged else "FAIL"
        n_comp = len(r.comparisons)
        status = "PASS" if r.overall_pass else "FAIL"
        print(f"{r.model_name:<30} {mono_t:<10} {mod_t:<10} {n_comp:<8} {status:<10}")

    if results:
        print(f"\n{'='*80}")
        print("LINE-LEVEL COMPARISON")
        print(f"{'='*80}")
        print(f"{'Model':<25} {'Line':<30} {'EA_T(kN)':<12} {'EB_T(kN)':<12} {'MaxT(kN)':<12} {'Pass'}")
        print(f"{'-'*100}")

        for r in results:
            for c in r.comparisons:
                ea = f"{c.end_a_tension_diff_pct:.1f}%"
                eb = f"{c.end_b_tension_diff_pct:.1f}%"
                mt = f"{c.max_tension_diff_pct:.1f}%"
                status = "YES" if (c.tension_pass and c.bending_pass) else "NO"
                print(f"{r.model_name:<25} {c.line_name_mono:<30} {ea:<12} {eb:<12} {mt:<12} {status}")

    print(f"\nSaved: {summary_path}")
    return 0


if __name__ == "__main__":
    exit(main())
