#!/usr/bin/env python
"""Mesh Sensitivity Matrix for Riser Library Models.

Runs coarse/medium/fine mesh variants for each riser model to assess
convergence behavior. Outputs JSON results and a summary table.

Mesh variants override segment_length in each section:
  - coarse:  5m (bare), 5m (buoyancy)
  - medium:  2m (bare), 2m (buoyancy) — default in specs
  - fine:    1m (bare), 0.5m (buoyancy)

Usage:
    uv run python scripts/mesh_sensitivity_riser.py
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
    print("OrcFxAPI not available - cannot run mesh sensitivity")
    exit(1)

import yaml

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

LIBRARY_ROOT = Path("docs/domains/orcaflex/library/tier2_fast")
OUTPUT_ROOT = Path("benchmark_output/mesh_sensitivity")

MESH_PRESETS = {
    "coarse": {"bare": 5.0, "buoyancy": 5.0},
    "medium": {"bare": 2.0, "buoyancy": 2.0},
    "fine": {"bare": 1.0, "buoyancy": 0.5},
}

MODELS = [
    "a01_catenary_riser",
    "a01_lazy_wave_riser",
    "a01_pliant_wave_riser",
    "a01_steep_wave_riser",
]


@dataclass
class MeshLineResult:
    """Results for a single line at a specific mesh level."""

    line_name: str
    num_segments: int
    total_length_m: float
    end_a_tension_kN: float
    end_b_tension_kN: float
    max_tension_kN: float
    max_bending_kNm: float


@dataclass
class MeshVariantResult:
    """Results for one model at one mesh level."""

    model_name: str
    mesh_level: str
    bare_segment_length: float
    buoyancy_segment_length: float
    statics_converged: bool
    statics_time_seconds: float
    total_segments: int
    lines: dict[str, MeshLineResult] = field(default_factory=dict)


@dataclass
class MeshConvergence:
    """Convergence metrics comparing coarse/medium/fine for one line."""

    line_name: str
    metric: str
    coarse_value: float
    medium_value: float
    fine_value: float
    coarse_to_fine_pct: float


@dataclass
class ModelMeshSensitivity:
    """Complete mesh sensitivity results for one model."""

    model_name: str
    timestamp: str
    variants: dict[str, MeshVariantResult] = field(default_factory=dict)
    convergence: list[MeshConvergence] = field(default_factory=list)


def apply_mesh_preset(spec_data: dict, preset: str) -> dict:
    """Apply mesh preset to spec data by overriding segment_length values.

    Uses a heuristic: sections with line_type containing 'buoy' or having
    contact_diameter use buoyancy segment length; others use bare.
    """
    data = copy.deepcopy(spec_data)
    preset_cfg = MESH_PRESETS[preset]

    riser = data.get("riser", {})
    buoyancy_types = set()

    # Identify buoyancy line types by name patterns
    for lt in riser.get("line_types", []):
        name = lt.get("name", "").lower()
        if "buoy" in name or lt.get("contact_diameter"):
            buoyancy_types.add(lt["name"])

    for line in riser.get("lines", []):
        for section in line.get("sections", []):
            if section["line_type"] in buoyancy_types:
                section["segment_length"] = preset_cfg["buoyancy"]
            else:
                section["segment_length"] = preset_cfg["bare"]

    return data


def extract_line_results(model: OrcFxAPI.Model, line_name: str) -> MeshLineResult:
    """Extract statics results from a line in a converged model."""
    line = model[line_name]
    num_sections = line.NumberOfSections
    total_length = sum(line.Length[i] for i in range(num_sections))

    end_a_tension = line.StaticResult("Effective Tension", OrcFxAPI.oeEndA)
    end_b_tension = line.StaticResult("Effective Tension", OrcFxAPI.oeEndB)

    tension_rg = line.RangeGraph("Effective Tension")
    bending_rg = line.RangeGraph("Bend Moment")

    total_segs = sum(line.NumberOfSegments[i] for i in range(num_sections))

    return MeshLineResult(
        line_name=line_name,
        num_segments=total_segs,
        total_length_m=total_length,
        end_a_tension_kN=round(end_a_tension, 3),
        end_b_tension_kN=round(end_b_tension, 3),
        max_tension_kN=round(max(tension_rg.Mean), 3),
        max_bending_kNm=round(max(abs(v) for v in bending_rg.Mean), 3),
    )


def pct_diff(a: float, b: float) -> float:
    """Percentage difference between two values."""
    denom = max(abs(a), abs(b))
    if denom < 1e-9:
        return 0.0
    return abs(a - b) / denom * 100.0


def run_mesh_variant(
    model_name: str, spec_data: dict, preset: str, output_dir: Path
) -> MeshVariantResult:
    """Generate and run one mesh variant."""
    preset_cfg = MESH_PRESETS[preset]
    variant_data = apply_mesh_preset(spec_data, preset)
    spec = ProjectInputSpec(**variant_data)

    modular_dir = output_dir / preset
    modular_dir.mkdir(parents=True, exist_ok=True)

    generator = ModularModelGenerator.from_spec(spec)
    generator.generate(modular_dir)

    master = modular_dir / "master.yml"
    model = OrcFxAPI.Model()
    model.LoadData(str(master))

    start = time.time()
    try:
        model.CalculateStatics()
        elapsed = time.time() - start
        converged = True
    except OrcFxAPI.DLLError as e:
        elapsed = time.time() - start
        print(f"    {preset} statics FAILED: {e}")
        converged = False

    result = MeshVariantResult(
        model_name=model_name,
        mesh_level=preset,
        bare_segment_length=preset_cfg["bare"],
        buoyancy_segment_length=preset_cfg["buoyancy"],
        statics_converged=converged,
        statics_time_seconds=round(elapsed, 3),
        total_segments=0,
    )

    if converged:
        lines = [o for o in model.objects if o.type == OrcFxAPI.ObjectType.Line]
        total_segs = 0
        for line in lines:
            lr = extract_line_results(model, line.Name)
            result.lines[line.Name] = lr
            total_segs += lr.num_segments
        result.total_segments = total_segs

    return result


def compute_convergence(sensitivity: ModelMeshSensitivity) -> None:
    """Compute convergence metrics across mesh levels."""
    if not all(k in sensitivity.variants for k in ("coarse", "medium", "fine")):
        return
    if not all(v.statics_converged for v in sensitivity.variants.values()):
        return

    coarse = sensitivity.variants["coarse"]
    medium = sensitivity.variants["medium"]
    fine = sensitivity.variants["fine"]

    # Find common line names
    common_lines = set(coarse.lines.keys()) & set(medium.lines.keys()) & set(fine.lines.keys())

    for line_name in sorted(common_lines):
        c, m, f = coarse.lines[line_name], medium.lines[line_name], fine.lines[line_name]

        for metric, c_val, m_val, f_val in [
            ("end_a_tension_kN", c.end_a_tension_kN, m.end_a_tension_kN, f.end_a_tension_kN),
            ("max_tension_kN", c.max_tension_kN, m.max_tension_kN, f.max_tension_kN),
            ("max_bending_kNm", c.max_bending_kNm, m.max_bending_kNm, f.max_bending_kNm),
        ]:
            sensitivity.convergence.append(
                MeshConvergence(
                    line_name=line_name,
                    metric=metric,
                    coarse_value=c_val,
                    medium_value=m_val,
                    fine_value=f_val,
                    coarse_to_fine_pct=round(pct_diff(c_val, f_val), 2),
                )
            )


def run_model_sensitivity(model_name: str) -> ModelMeshSensitivity | None:
    """Run mesh sensitivity for a single model."""
    spec_path = LIBRARY_ROOT / model_name / "spec.yml"
    if not spec_path.exists():
        print(f"SKIP {model_name}: spec not found")
        return None

    print(f"\n{'='*60}")
    print(f"MESH SENSITIVITY: {model_name}")
    print(f"{'='*60}")

    with open(spec_path) as f:
        spec_data = yaml.safe_load(f)

    timestamp = time.strftime("%Y-%m-%dT%H:%M:%S")
    sensitivity = ModelMeshSensitivity(model_name=model_name, timestamp=timestamp)

    output_dir = OUTPUT_ROOT / model_name

    for preset in ("coarse", "medium", "fine"):
        print(f"  Running {preset} (bare={MESH_PRESETS[preset]['bare']}m, "
              f"buoy={MESH_PRESETS[preset]['buoyancy']}m)...")
        result = run_mesh_variant(model_name, spec_data, preset, output_dir)
        sensitivity.variants[preset] = result
        status = "CONVERGED" if result.statics_converged else "FAILED"
        print(f"    {status} ({result.statics_time_seconds}s, "
              f"{result.total_segments} segments)")

    compute_convergence(sensitivity)

    # Save per-model results
    result_path = output_dir / "mesh_sensitivity.json"
    result_path.parent.mkdir(parents=True, exist_ok=True)
    with open(result_path, "w") as f:
        json.dump(asdict(sensitivity), f, indent=2, default=str)
    print(f"  Saved: {result_path}")

    return sensitivity


def print_summary(results: list[ModelMeshSensitivity]) -> None:
    """Print mesh sensitivity summary table."""
    print(f"\n\n{'='*100}")
    print("MESH SENSITIVITY SUMMARY")
    print(f"{'='*100}")
    print(f"{'Model':<28} {'Level':<8} {'Seg(m)':<8} {'Segs':<8} "
          f"{'Time(s)':<8} {'MaxT(kN)':<12} {'MaxB(kN.m)':<12}")
    print(f"{'-'*100}")

    for s in results:
        for level in ("coarse", "medium", "fine"):
            v = s.variants.get(level)
            if not v or not v.statics_converged:
                continue
            for line_name, lr in v.lines.items():
                seg = f"{v.bare_segment_length}"
                print(f"{s.model_name:<28} {level:<8} {seg:<8} {lr.num_segments:<8} "
                      f"{v.statics_time_seconds:<8} {lr.max_tension_kN:<12} "
                      f"{lr.max_bending_kNm:<12}")

    # Convergence table
    print(f"\n{'='*100}")
    print("CONVERGENCE: Coarse vs Fine")
    print(f"{'='*100}")
    print(f"{'Model':<28} {'Line':<28} {'Metric':<20} "
          f"{'Coarse':<12} {'Fine':<12} {'Diff%':<8}")
    print(f"{'-'*100}")

    for s in results:
        for c in s.convergence:
            print(f"{s.model_name:<28} {c.line_name:<28} {c.metric:<20} "
                  f"{c.coarse_value:<12.2f} {c.fine_value:<12.2f} "
                  f"{c.coarse_to_fine_pct:<8.1f}")


def main() -> int:
    """Run mesh sensitivity across all riser models."""
    results = []
    for model_name in MODELS:
        sensitivity = run_model_sensitivity(model_name)
        if sensitivity:
            results.append(sensitivity)

    # Save combined summary
    OUTPUT_ROOT.mkdir(parents=True, exist_ok=True)
    summary_path = OUTPUT_ROOT / "mesh_sensitivity_summary.json"
    with open(summary_path, "w") as f:
        json.dump([asdict(r) for r in results], f, indent=2, default=str)

    print_summary(results)
    print(f"\nSaved: {summary_path}")
    return 0


if __name__ == "__main__":
    exit(main())
