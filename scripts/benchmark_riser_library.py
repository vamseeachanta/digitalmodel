#!/usr/bin/env python
"""Comprehensive Benchmark: Monolithic vs Spec-Driven Modular OrcaFlex Riser Models.

Three-way comparison plus mesh sensitivity convergence analysis:

  1. MONOLITHIC  — legacy .dat loaded directly into OrcFxAPI
  2. SPEC-DRIVEN — spec.yml -> ModularModelGenerator -> master.yml -> OrcFxAPI
  3. MESH SENSITIVITY — coarse/medium/fine variants from spec.yml

This is the definitive report for validating the spec.yml pipeline against
the monolithic reference and checking numerical convergence.

Saves:
  - Per-model: validation/benchmark.json
  - Combined:  docs/modules/orcaflex/library/tier2_fast/validation/riser_library_benchmark.json

Usage:
    uv run python scripts/benchmark_riser_library.py
"""

from __future__ import annotations

import copy
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
OUTPUT_ROOT = LIBRARY_ROOT / "validation"

# Line name mapping: monolithic name -> modular name (spec line name)
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

MESH_PRESETS = {
    "coarse": {"bare": 5.0, "buoyancy": 5.0},
    "medium": {"bare": 2.0, "buoyancy": 2.0},
    "fine": {"bare": 1.0, "buoyancy": 0.5},
}


# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------

@dataclass
class LineResults:
    """Engineering results extracted from a single line after statics."""

    line_name: str
    total_length_m: float
    num_sections: int
    num_segments: int
    end_a_tension_kN: float
    end_b_tension_kN: float
    end_a_bending_kNm: float
    end_b_bending_kNm: float
    max_tension_kN: float
    min_tension_kN: float
    max_bending_kNm: float


@dataclass
class LinkResults:
    """Results from a single Link object after statics."""

    link_name: str
    link_type: str
    tension_kN: float


@dataclass
class ModelResults:
    """Results from a complete model (monolithic or modular)."""

    source_type: str
    source_path: str
    statics_converged: bool
    statics_time_seconds: float
    object_count: int
    lines: dict[str, LineResults] = field(default_factory=dict)
    links: dict[str, LinkResults] = field(default_factory=dict)


@dataclass
class SpecInfo:
    """Input data stats from the spec.yml file."""

    spec_lines: int
    line_type_count: int
    line_count: int
    link_count: int
    clump_type_count: int
    configuration: str
    water_depth_m: float
    total_riser_length_m: float


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
    tension_pass: bool
    bending_pass: bool


@dataclass
class MeshLevelResult:
    """Results for one mesh level."""

    level: str
    bare_seg_m: float
    buoy_seg_m: float
    statics_converged: bool
    statics_time_seconds: float
    total_segments: int
    lines: dict[str, LineResults] = field(default_factory=dict)


@dataclass
class MeshConvergence:
    """Convergence metric for one line across mesh levels."""

    line_name: str
    metric: str
    coarse: float
    medium: float
    fine: float
    coarse_to_fine_pct: float
    converged: bool


@dataclass
class BenchmarkResult:
    """Complete benchmark result for a model."""

    model_name: str
    timestamp: str
    spec_info: SpecInfo
    monolithic: ModelResults
    modular: ModelResults
    comparisons: list[LineComparison] = field(default_factory=list)
    mesh_levels: dict[str, MeshLevelResult] = field(default_factory=dict)
    mesh_convergence: list[MeshConvergence] = field(default_factory=list)
    overall_pass: bool = False


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def pct_diff(a: float, b: float) -> float:
    """Percentage difference; handle zero denominators."""
    if abs(a) < 1e-9 and abs(b) < 1e-9:
        return 0.0
    denom = max(abs(a), abs(b))
    if denom < 1e-9:
        return 0.0
    return abs(a - b) / denom * 100.0


def extract_line_results(model: OrcFxAPI.Model, line_name: str) -> LineResults:
    """Extract statics results from a named line."""
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

    return LineResults(
        line_name=line_name,
        total_length_m=round(total_len, 3),
        num_sections=n_sec,
        num_segments=total_seg,
        end_a_tension_kN=round(ea_t, 3),
        end_b_tension_kN=round(eb_t, 3),
        end_a_bending_kNm=round(ea_b, 3),
        end_b_bending_kNm=round(eb_b, 3),
        max_tension_kN=round(max(tension_rg.Mean), 3),
        min_tension_kN=round(min(tension_rg.Mean), 3),
        max_bending_kNm=round(max(abs(v) for v in bending_rg.Mean), 3),
    )


def extract_all_results(model: OrcFxAPI.Model) -> tuple[dict[str, LineResults], dict[str, LinkResults]]:
    """Extract line and link results from converged model."""
    lines = {}
    links = {}
    for obj in model.objects:
        if obj.type == OrcFxAPI.ObjectType.Line:
            lines[obj.Name] = extract_line_results(model, obj.Name)
        elif obj.type == OrcFxAPI.ObjectType.Link:
            tension = obj.StaticResult("Tension")
            links[obj.Name] = LinkResults(
                link_name=obj.Name,
                link_type=obj.LinkType,
                tension_kN=round(tension, 3),
            )
    return lines, links


def run_model(source_type: str, path: str, model: OrcFxAPI.Model) -> ModelResults:
    """Run statics on a loaded model and extract results."""
    obj_count = len(model.objects)

    start = time.time()
    try:
        model.CalculateStatics()
        elapsed = round(time.time() - start, 3)
        converged = True
    except OrcFxAPI.DLLError as e:
        elapsed = round(time.time() - start, 3)
        print(f"    Statics FAILED: {e}")
        converged = False

    results = ModelResults(
        source_type=source_type,
        source_path=path,
        statics_converged=converged,
        statics_time_seconds=elapsed,
        object_count=obj_count,
    )

    if converged:
        results.lines, results.links = extract_all_results(model)

    return results


# ---------------------------------------------------------------------------
# Spec info extraction
# ---------------------------------------------------------------------------

def extract_spec_info(spec_path: Path, spec: ProjectInputSpec) -> SpecInfo:
    """Extract input data statistics from spec."""
    spec_lines = spec_path.read_text().count("\n") + 1
    riser = spec.riser

    configs = set()
    total_len = 0.0
    for line in riser.lines:
        configs.add(line.configuration.value)
        total_len += line.get_total_length()

    return SpecInfo(
        spec_lines=spec_lines,
        line_type_count=len(riser.line_types),
        line_count=len(riser.lines),
        link_count=len(riser.links),
        clump_type_count=len(riser.clump_types),
        configuration=", ".join(sorted(configs)),
        water_depth_m=spec.environment.water.depth,
        total_riser_length_m=round(total_len, 1),
    )


# ---------------------------------------------------------------------------
# Monolithic runner
# ---------------------------------------------------------------------------

def run_monolithic(model_dir: Path) -> ModelResults:
    """Load monolithic .dat, run statics, extract results."""
    mono_dir = model_dir / "monolithic"
    dat_files = list(mono_dir.glob("*.dat"))
    if not dat_files:
        raise FileNotFoundError(f"No .dat file in {mono_dir}")

    mono_path = dat_files[0]
    print(f"    Loading: {mono_path.name}")
    model = OrcFxAPI.Model(str(mono_path))
    return run_model("monolithic", str(mono_path), model)


# ---------------------------------------------------------------------------
# Spec-driven modular runner
# ---------------------------------------------------------------------------

def run_spec_modular(model_dir: Path) -> tuple[ProjectInputSpec, ModelResults]:
    """Spec -> modular -> OrcFxAPI -> statics -> results."""
    spec_path = model_dir / "spec.yml"
    with open(spec_path) as f:
        data = yaml.safe_load(f)
    spec = ProjectInputSpec(**data)

    modular_dir = model_dir / "modular"
    modular_dir.mkdir(parents=True, exist_ok=True)

    print(f"    Generating from: {spec_path.name}")
    generator = ModularModelGenerator.from_spec(spec)
    generator.generate(modular_dir)

    master = modular_dir / "master.yml"
    print(f"    Loading: master.yml")
    model = OrcFxAPI.Model()
    model.LoadData(str(master))

    return spec, run_model("spec-modular", str(master), model)


# ---------------------------------------------------------------------------
# Mesh sensitivity runner
# ---------------------------------------------------------------------------

def apply_mesh_preset(spec_data: dict, preset: str) -> dict:
    """Override segment_length values with mesh preset."""
    data = copy.deepcopy(spec_data)
    cfg = MESH_PRESETS[preset]

    riser = data.get("riser", {})
    buoyancy_types = set()
    for lt in riser.get("line_types", []):
        name = lt.get("name", "").lower()
        if "buoy" in name or lt.get("contact_diameter"):
            buoyancy_types.add(lt["name"])

    for line in riser.get("lines", []):
        for section in line.get("sections", []):
            if section["line_type"] in buoyancy_types:
                section["segment_length"] = cfg["buoyancy"]
            else:
                section["segment_length"] = cfg["bare"]

    return data


def run_mesh_variant(
    model_dir: Path, spec_data: dict, preset: str
) -> MeshLevelResult:
    """Generate and run one mesh variant."""
    cfg = MESH_PRESETS[preset]
    variant_data = apply_mesh_preset(spec_data, preset)
    spec = ProjectInputSpec(**variant_data)

    mesh_dir = model_dir / "mesh" / preset
    mesh_dir.mkdir(parents=True, exist_ok=True)

    generator = ModularModelGenerator.from_spec(spec)
    generator.generate(mesh_dir)

    master = mesh_dir / "master.yml"
    model = OrcFxAPI.Model()
    model.LoadData(str(master))

    obj_count = len(model.objects)
    start = time.time()
    try:
        model.CalculateStatics()
        elapsed = round(time.time() - start, 3)
        converged = True
    except OrcFxAPI.DLLError as e:
        elapsed = round(time.time() - start, 3)
        print(f"      {preset} statics FAILED: {e}")
        converged = False

    result = MeshLevelResult(
        level=preset,
        bare_seg_m=cfg["bare"],
        buoy_seg_m=cfg["buoyancy"],
        statics_converged=converged,
        statics_time_seconds=elapsed,
        total_segments=0,
    )

    if converged:
        total_segs = 0
        for obj in model.objects:
            if obj.type == OrcFxAPI.ObjectType.Line:
                lr = extract_line_results(model, obj.Name)
                result.lines[obj.Name] = lr
                total_segs += lr.num_segments
        result.total_segments = total_segs

    return result


def run_mesh_sensitivity(model_dir: Path) -> tuple[dict[str, MeshLevelResult], list[MeshConvergence]]:
    """Run coarse/medium/fine mesh variants for one model."""
    spec_path = model_dir / "spec.yml"
    with open(spec_path) as f:
        spec_data = yaml.safe_load(f)

    levels = {}
    for preset in ("coarse", "medium", "fine"):
        cfg = MESH_PRESETS[preset]
        print(f"    Mesh {preset} (bare={cfg['bare']}m, buoy={cfg['buoyancy']}m)...")
        levels[preset] = run_mesh_variant(model_dir, spec_data, preset)
        r = levels[preset]
        status = "OK" if r.statics_converged else "FAIL"
        print(f"      {status} ({r.statics_time_seconds}s, {r.total_segments} segs)")

    # Compute convergence
    convergence = []
    if all(v.statics_converged for v in levels.values()):
        common = set(levels["coarse"].lines) & set(levels["medium"].lines) & set(levels["fine"].lines)
        for ln in sorted(common):
            c = levels["coarse"].lines[ln]
            m = levels["medium"].lines[ln]
            f = levels["fine"].lines[ln]
            for metric, cv, mv, fv in [
                ("end_a_tension_kN", c.end_a_tension_kN, m.end_a_tension_kN, f.end_a_tension_kN),
                ("max_tension_kN", c.max_tension_kN, m.max_tension_kN, f.max_tension_kN),
                ("max_bending_kNm", c.max_bending_kNm, m.max_bending_kNm, f.max_bending_kNm),
            ]:
                diff = round(pct_diff(cv, fv), 2)
                convergence.append(MeshConvergence(
                    line_name=ln,
                    metric=metric,
                    coarse=cv,
                    medium=mv,
                    fine=fv,
                    coarse_to_fine_pct=diff,
                    converged=diff < 5.0,
                ))

    return levels, convergence


# ---------------------------------------------------------------------------
# Comparison
# ---------------------------------------------------------------------------

def compare_lines(mono_lr: LineResults, mod_lr: LineResults) -> LineComparison:
    """Compare monolithic and modular line results."""
    ea_t_diff = pct_diff(mono_lr.end_a_tension_kN, mod_lr.end_a_tension_kN)
    eb_t_diff = pct_diff(mono_lr.end_b_tension_kN, mod_lr.end_b_tension_kN)
    max_t_diff = pct_diff(mono_lr.max_tension_kN, mod_lr.max_tension_kN)
    len_diff = pct_diff(mono_lr.total_length_m, mod_lr.total_length_m)

    tension_pass = all([
        ea_t_diff < 5.0 or abs(mono_lr.end_a_tension_kN - mod_lr.end_a_tension_kN) < 10.0,
        eb_t_diff < 5.0 or abs(mono_lr.end_b_tension_kN - mod_lr.end_b_tension_kN) < 10.0,
    ])
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


# ---------------------------------------------------------------------------
# Per-model benchmark
# ---------------------------------------------------------------------------

def benchmark_model(model_name: str) -> BenchmarkResult | None:
    """Run full benchmark for a single model."""
    model_dir = LIBRARY_ROOT / model_name
    if not model_dir.exists():
        print(f"SKIP {model_name}: directory not found")
        return None

    print(f"\n{'='*70}")
    print(f"  BENCHMARKING: {model_name}")
    print(f"{'='*70}")

    timestamp = time.strftime("%Y-%m-%dT%H:%M:%S")

    # --- A. Monolithic ---
    print("\n  [A] MONOLITHIC (reference .dat)")
    mono = run_monolithic(model_dir)
    print(f"    {'CONVERGED' if mono.statics_converged else 'FAILED'}"
          f" ({mono.statics_time_seconds}s, {mono.object_count} objects)")

    # --- B. Spec-driven modular ---
    print("\n  [B] SPEC-DRIVEN (spec.yml -> modular -> OrcaFlex)")
    spec, modular = run_spec_modular(model_dir)
    spec_info = extract_spec_info(model_dir / "spec.yml", spec)
    print(f"    {'CONVERGED' if modular.statics_converged else 'FAILED'}"
          f" ({modular.statics_time_seconds}s, {modular.object_count} objects)")
    print(f"    Spec: {spec_info.spec_lines} lines, "
          f"{spec_info.line_type_count} line types, "
          f"{spec_info.link_count} links, "
          f"config={spec_info.configuration}")

    # --- C. Mesh sensitivity ---
    print("\n  [C] MESH SENSITIVITY (coarse / medium / fine)")
    mesh_levels, mesh_conv = run_mesh_sensitivity(model_dir)

    # --- Compare monolithic vs modular ---
    comparisons = []
    if mono.statics_converged and modular.statics_converged:
        mapping = LINE_MAPPINGS.get(model_name, {})
        for mono_name, mod_name in mapping.items():
            if mono_name in mono.lines and mod_name in modular.lines:
                comp = compare_lines(mono.lines[mono_name], modular.lines[mod_name])
                comparisons.append(comp)

    overall = all(c.tension_pass and c.bending_pass for c in comparisons) if comparisons else False

    result = BenchmarkResult(
        model_name=model_name,
        timestamp=timestamp,
        spec_info=spec_info,
        monolithic=mono,
        modular=modular,
        comparisons=comparisons,
        mesh_levels=mesh_levels,
        mesh_convergence=mesh_conv,
        overall_pass=overall,
    )

    # --- Print per-model detail ---
    print(f"\n  --- COMPARISON: Monolithic vs Spec-Driven ---")
    for c in comparisons:
        status = "PASS" if (c.tension_pass and c.bending_pass) else "FAIL"
        print(f"    {c.line_name_mono} vs {c.line_name_mod}:")
        print(f"      End A tension: {c.end_a_tension_mono_kN:8.1f} vs {c.end_a_tension_mod_kN:8.1f} kN  ({c.end_a_tension_diff_pct:.1f}%)")
        print(f"      End B tension: {c.end_b_tension_mono_kN:8.1f} vs {c.end_b_tension_mod_kN:8.1f} kN  ({c.end_b_tension_diff_pct:.1f}%)")
        print(f"      Max tension:   {c.max_tension_mono_kN:8.1f} vs {c.max_tension_mod_kN:8.1f} kN  ({c.max_tension_diff_pct:.1f}%)")
        print(f"      Max bending:   {c.max_bending_mono_kNm:8.2f} vs {c.max_bending_mod_kNm:8.2f} kN.m")
        print(f"      Length:        {c.length_mono_m:8.1f} vs {c.length_mod_m:8.1f} m   ({c.length_diff_pct:.3f}%)")
        print(f"      Result: {status}")

    if modular.links:
        print(f"\n  --- LINK RESULTS (spec-driven) ---")
        for lk in modular.links.values():
            print(f"    {lk.link_name}: {lk.link_type}, tension={lk.tension_kN:.1f} kN")

    # Save per-model
    bench_path = model_dir / "validation" / "benchmark.json"
    bench_path.parent.mkdir(parents=True, exist_ok=True)
    with open(bench_path, "w") as f:
        json.dump(asdict(result), f, indent=2, default=str)
    print(f"\n  Saved: {bench_path}")

    return result


# ---------------------------------------------------------------------------
# Summary tables
# ---------------------------------------------------------------------------

def print_summary(results: list[BenchmarkResult]) -> None:
    """Print comprehensive summary tables."""
    W = 110

    # --- TABLE 1: Input Data Comparison ---
    print(f"\n\n{'='*W}")
    print("TABLE 1: INPUT DATA COMPARISON")
    print(f"{'='*W}")
    print(f"{'Model':<28} {'Spec Lines':<12} {'Config':<14} {'LineTypes':<10} "
          f"{'Links':<8} {'Length(m)':<10} {'Depth(m)':<10} {'Mono Objs':<10} {'Mod Objs':<10}")
    print(f"{'-'*W}")
    for r in results:
        s = r.spec_info
        print(f"{r.model_name:<28} {s.spec_lines:<12} {s.configuration:<14} "
              f"{s.line_type_count:<10} {s.link_count:<8} {s.total_riser_length_m:<10.1f} "
              f"{s.water_depth_m:<10.0f} {r.monolithic.object_count:<10} {r.modular.object_count:<10}")

    # --- TABLE 2: Monolithic vs Spec-Driven ---
    print(f"\n{'='*W}")
    print("TABLE 2: MONOLITHIC vs SPEC-DRIVEN (End A Tension, Max Tension, Max Bending)")
    print(f"{'='*W}")
    print(f"{'Model':<28} {'Line':<28} {'Mono EA(kN)':<13} {'Spec EA(kN)':<13} "
          f"{'Diff%':<8} {'Mono MaxB':<11} {'Spec MaxB':<11} {'Pass':<6}")
    print(f"{'-'*W}")
    for r in results:
        for c in r.comparisons:
            status = "PASS" if (c.tension_pass and c.bending_pass) else "FAIL"
            print(f"{r.model_name:<28} {c.line_name_mono:<28} "
                  f"{c.end_a_tension_mono_kN:<13.1f} {c.end_a_tension_mod_kN:<13.1f} "
                  f"{c.end_a_tension_diff_pct:<8.1f} "
                  f"{c.max_bending_mono_kNm:<11.2f} {c.max_bending_mod_kNm:<11.2f} "
                  f"{status:<6}")

    # --- TABLE 3: Spec-Driven Results (all lines + links) ---
    print(f"\n{'='*W}")
    print("TABLE 3: SPEC-DRIVEN RESULTS (complete extraction)")
    print(f"{'='*W}")
    print(f"{'Model':<28} {'Line/Link':<28} {'Type':<10} {'EA_T(kN)':<11} "
          f"{'EB_T(kN)':<11} {'MaxT(kN)':<11} {'MaxB(kN.m)':<11} {'Segs':<8}")
    print(f"{'-'*W}")
    for r in results:
        for lr in r.modular.lines.values():
            print(f"{r.model_name:<28} {lr.line_name:<28} {'Line':<10} "
                  f"{lr.end_a_tension_kN:<11.1f} {lr.end_b_tension_kN:<11.1f} "
                  f"{lr.max_tension_kN:<11.1f} {lr.max_bending_kNm:<11.2f} "
                  f"{lr.num_segments:<8}")
        for lk in r.modular.links.values():
            print(f"{r.model_name:<28} {lk.link_name:<28} {lk.link_type:<10} "
                  f"{lk.tension_kN:<11.1f} {'':<11} {'':<11} {'':<11} {'':<8}")

    # --- TABLE 4: Mesh Sensitivity ---
    print(f"\n{'='*W}")
    print("TABLE 4: MESH SENSITIVITY (coarse 5m / medium 2m / fine 1m)")
    print(f"{'='*W}")
    print(f"{'Model':<28} {'Line':<28} {'Metric':<18} {'Coarse':<12} "
          f"{'Medium':<12} {'Fine':<12} {'C-F Diff%':<10} {'Conv?':<6}")
    print(f"{'-'*W}")
    for r in results:
        for mc in r.mesh_convergence:
            conv = "YES" if mc.converged else "NO"
            print(f"{r.model_name:<28} {mc.line_name:<28} {mc.metric:<18} "
                  f"{mc.coarse:<12.2f} {mc.medium:<12.2f} {mc.fine:<12.2f} "
                  f"{mc.coarse_to_fine_pct:<10.1f} {conv:<6}")

    # --- TABLE 5: Performance ---
    print(f"\n{'='*W}")
    print("TABLE 5: PERFORMANCE (statics time in seconds)")
    print(f"{'='*W}")
    print(f"{'Model':<28} {'Mono(s)':<10} {'Spec(s)':<10} {'Coarse(s)':<10} "
          f"{'Medium(s)':<10} {'Fine(s)':<10} {'Overall':<10}")
    print(f"{'-'*W}")
    for r in results:
        mt = f"{r.monolithic.statics_time_seconds:.2f}" if r.monolithic.statics_converged else "FAIL"
        st = f"{r.modular.statics_time_seconds:.2f}" if r.modular.statics_converged else "FAIL"
        ct = f"{r.mesh_levels['coarse'].statics_time_seconds:.2f}" if r.mesh_levels.get("coarse", MeshLevelResult("",0,0,False,0,0)).statics_converged else "FAIL"
        medt = f"{r.mesh_levels['medium'].statics_time_seconds:.2f}" if r.mesh_levels.get("medium", MeshLevelResult("",0,0,False,0,0)).statics_converged else "FAIL"
        ft = f"{r.mesh_levels['fine'].statics_time_seconds:.2f}" if r.mesh_levels.get("fine", MeshLevelResult("",0,0,False,0,0)).statics_converged else "FAIL"
        overall = "PASS" if r.overall_pass else "FAIL"
        print(f"{r.model_name:<28} {mt:<10} {st:<10} {ct:<10} {medt:<10} {ft:<10} {overall:<10}")


def main() -> int:
    """Run comprehensive benchmark across all riser models."""
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

    print_summary(results)

    print(f"\n\nSaved: {summary_path}")
    return 0


if __name__ == "__main__":
    exit(main())
