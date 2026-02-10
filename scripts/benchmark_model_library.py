#!/usr/bin/env python
"""General-purpose OrcaFlex Model Library Benchmark.

Discovers .dat files under docs/modules/orcaflex/examples/raw/, runs statics,
extracts line results, optionally performs mesh sensitivity, and produces a
comprehensive HTML report.  Designed for overnight batch runs.

Optional 3-way comparison mode (--three-way) adds:
  Path A: Monolithic  -- existing .dat -> statics -> extract
  Path B: Spec-driven -- .dat -> YAML -> extract -> spec -> generate -> statics
  Path C: Modular-direct -- spec.yml from library -> generate -> statics

Usage:
    uv run python scripts/benchmark_model_library.py
    uv run python scripts/benchmark_model_library.py --html-only
    uv run python scripts/benchmark_model_library.py --max-models 5
    uv run python scripts/benchmark_model_library.py --skip-mesh
    uv run python scripts/benchmark_model_library.py --three-way
"""
from __future__ import annotations

import argparse, html, json, re, sys, tempfile, time
from dataclasses import asdict, dataclass, field
from pathlib import Path

try:
    import OrcFxAPI
except ImportError:
    print("OrcFxAPI not available - cannot run benchmark"); exit(1)

try:
    from digitalmodel.solvers.orcaflex.modular_generator.extractor import MonolithicExtractor
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
    from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
    _HAS_MODULAR = True
except ImportError:
    _HAS_MODULAR = False

EXAMPLES_ROOT = Path("docs/modules/orcaflex/examples/raw")
OUTPUT_ROOT = Path("benchmark_output")
DOCS_OUTPUT = Path("docs/modules/orcaflex/examples")  # reports alongside models
JSON_PATH = OUTPUT_ROOT / "model_library_benchmark.json"
HTML_PATH = OUTPUT_ROOT / "model_library_report.html"
DOCS_HTML_PATH = DOCS_OUTPUT / "model_library_report.html"
MAX_FILE_SIZE = 5_000_000
MESH_TIME_LIMIT = 30.0
STATICS_TIMEOUT = 120.0  # max seconds for any single statics run
MAX_SEGMENTS = 5000  # skip mesh levels that would exceed this
MESH_SCALES = {"coarse_2x": 2.0, "baseline": 1.0, "fine_05x": 0.5,
               "vfine_025x": 0.25, "ufine_0125x": 0.125}
MESH_ORDER = list(MESH_SCALES.keys())
MESH_COLORS = {"coarse_2x": "#d62728", "baseline": "#ff7f0e",
               "fine_05x": "#1f77b4", "vfine_025x": "#2ca02c", "ufine_0125x": "#9467bd"}
CONVERGENCE_THR = 1.0
PLOTLY_CDN = "https://cdn.plot.ly/plotly-latest.min.js"
LINE_COLORS = ["#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd",
               "#8c564b","#e377c2","#7f7f7f","#bcbd22","#17becf"]
LIBRARY_ROOT = Path("docs/modules/orcaflex/library")
THREE_WAY_COLORS = {
    "monolithic": "#e74c3c",    # Red
    "spec_driven": "#3498db",   # Blue
    "modular": "#27ae60",       # Green
}
THREE_WAY_DASH = {
    "monolithic": None,         # Solid
    "spec_driven": "dash",      # Dashed
    "modular": "dot",           # Dotted
}
THREE_WAY_LABELS = {
    "monolithic": "Monolithic",
    "spec_driven": "Spec-Driven",
    "modular": "Modular-Direct",
}

# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------
@dataclass
class LineResult:
    name: str; length_m: float; segments: int
    end_a_tension_kN: float; end_b_tension_kN: float
    max_tension_kN: float; max_bending_kNm: float

@dataclass
class RangeData:
    arc_length: list[float]; tension: list[float]; bending: list[float]
    x: list[float]; z: list[float]

@dataclass
class MeshLevel:
    scale: float; label: str; total_segments: int
    statics_time_s: float; converged: bool
    lines: dict[str, LineResult] = field(default_factory=dict)
    range_data: dict[str, RangeData] = field(default_factory=dict)

@dataclass
class MeshConvergence:
    line_name: str; metric: str; values: dict[str, float]
    coarsest_to_finest_pct: float; converged: bool
    adjacent_diffs: dict[str, float] = field(default_factory=dict)
    recommended_level: str | None = None

@dataclass
class ModelBenchmark:
    name: str; category: str; dat_path: str; file_size_kb: int
    object_count: int; line_count: int; link_count: int
    vessel_count: int; buoy_count: int
    statics_converged: bool; statics_time_s: float; error_message: str | None
    lines: dict[str, LineResult] = field(default_factory=dict)
    range_data: dict[str, RangeData] = field(default_factory=dict)
    mesh_levels: dict[str, MeshLevel] = field(default_factory=dict)
    mesh_convergence: list[MeshConvergence] = field(default_factory=list)
    # 3-way comparison: Path B (spec-driven)
    spec_lines: dict[str, LineResult] = field(default_factory=dict)
    spec_range_data: dict[str, RangeData] = field(default_factory=dict)
    spec_statics_converged: bool = False
    spec_statics_time_s: float = 0.0
    spec_error: str | None = None
    # 3-way comparison: Path C (modular-direct from library spec.yml)
    modular_lines: dict[str, LineResult] = field(default_factory=dict)
    modular_range_data: dict[str, RangeData] = field(default_factory=dict)
    modular_statics_converged: bool = False
    modular_statics_time_s: float = 0.0
    modular_error: str | None = None

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
def pct_diff(a: float, b: float) -> float:
    if abs(a) < 1e-9 and abs(b) < 1e-9: return 0.0
    d = max(abs(a), abs(b))
    return 0.0 if d < 1e-9 else abs(a - b) / d * 100.0

def _flush(*args):
    """Print and flush immediately (for background runs)."""
    print(*args); sys.stdout.flush()

def _calc_statics(model) -> tuple[bool, float, str | None]:
    """Run statics. Returns (converged, elapsed, error)."""
    start = time.time()
    try:
        model.CalculateStatics()
        return True, round(time.time() - start, 3), None
    except OrcFxAPI.DLLError as e:
        return False, round(time.time() - start, 3), str(e)
    except Exception as e:
        return False, round(time.time() - start, 3), str(e)

def _sid(name: str) -> str:
    return re.sub(r"[^a-zA-Z0-9]", "_", name).strip("_").lower()

def _cat(p: Path) -> str:
    m = re.match(r"^[A-Z]\d{2}", p.parent.name)
    return m.group(0) if m else p.parent.name[:3]

def _counts(model):
    c = {"total": 0, "lines": 0, "links": 0, "vessels": 0, "buoys": 0}
    for o in model.objects:
        c["total"] += 1
        if o.type == OrcFxAPI.ObjectType.Line: c["lines"] += 1
        elif o.type == OrcFxAPI.ObjectType.Link: c["links"] += 1
        elif o.type == OrcFxAPI.ObjectType.Vessel: c["vessels"] += 1
        elif o.type == OrcFxAPI.ObjectType.Buoy6D: c["buoys"] += 1
    return c

def _recount(model) -> int:
    t = 0
    for o in model.objects:
        if o.type == OrcFxAPI.ObjectType.Line:
            try:
                for i in range(o.NumberOfSections): t += o.NumberOfSegments[i]
            except Exception: pass
    return t

def _dcls(p: float) -> str:
    if abs(p) < 1: return "ok"
    return "warn" if abs(p) < 5 else "high"

def _fail_bm(p: Path, msg: str) -> ModelBenchmark:
    return ModelBenchmark(name=p.stem, category=_cat(p), dat_path=str(p),
        file_size_kb=p.stat().st_size // 1024, object_count=0, line_count=0,
        link_count=0, vessel_count=0, buoy_count=0, statics_converged=False,
        statics_time_s=0.0, error_message=msg)

def _sanitize_name(name: str) -> str:
    """Convert a model name to a sanitized directory name."""
    return re.sub(r"[^a-zA-Z0-9]+", "_", name).strip("_").lower()

def _find_library_spec(model_name: str) -> Path | None:
    """Search LIBRARY_ROOT for a spec.yml matching the sanitized model name."""
    sanitized = _sanitize_name(model_name)
    for spec_path in LIBRARY_ROOT.rglob("spec.yml"):
        if spec_path.parent.name == sanitized:
            return spec_path
    return None

def _embed_file(label: str, file_path: Path, max_lines: int = 200) -> str:
    """Generate a collapsible <details> block with file content."""
    if not file_path.exists():
        return ""
    text = file_path.read_text(encoding="utf-8", errors="replace")
    lines = text.splitlines()
    total = len(lines)
    truncated = total > max_lines
    display = "\n".join(lines[:max_lines])
    if truncated:
        display += f"\n\n... truncated ({total} total lines)"
    size_kb = file_path.stat().st_size / 1024
    return (
        f'<details><summary>{html.escape(label)} ({size_kb:.1f} KB)</summary>'
        f'<pre>{html.escape(display)}</pre>'
        f'<div class="file-meta">{html.escape(file_path.name)}'
        f' &mdash; {total} lines</div></details>'
    )


def _build_file_browser(model_name: str) -> str:
    """Build the file browser section for a model."""
    sanitized = _sanitize_name(model_name)
    parts: list[str] = ['<div class="file-browser"><h3>Input Files</h3>']

    # Search LIBRARY_ROOT for the matching directory
    lib_dir: Path | None = None
    for candidate in LIBRARY_ROOT.rglob("spec.yml"):
        if candidate.parent.name == sanitized:
            lib_dir = candidate.parent
            break

    if lib_dir is None:
        return ""

    # spec.yml
    spec = lib_dir / "spec.yml"
    parts.append(_embed_file("spec.yml", spec, max_lines=200))

    # master.yml
    master = lib_dir / "modular" / "master.yml"
    parts.append(_embed_file("master.yml", master, max_lines=50))

    # Include files
    includes_dir = lib_dir / "modular" / "includes"
    if includes_dir.exists():
        for f in sorted(includes_dir.glob("*.yml")):
            parts.append(_embed_file(f"includes/{f.name}", f, max_lines=300))

    parts.append('</div>')
    # Only return if we found at least one file
    if any('<details>' in p for p in parts):
        return "\n".join(parts)
    return ""


# ---------------------------------------------------------------------------
# Extraction
# ---------------------------------------------------------------------------
def extract_line(model, ln: str) -> LineResult:
    line = model[ln]; ns = line.NumberOfSections
    tl = sum(line.Length[i] for i in range(ns))
    ts = sum(line.NumberOfSegments[i] for i in range(ns))
    ea = line.StaticResult("Effective Tension", OrcFxAPI.oeEndA)
    eb = line.StaticResult("Effective Tension", OrcFxAPI.oeEndB)
    trg = line.RangeGraph("Effective Tension")
    brg = line.RangeGraph("Bend Moment")
    return LineResult(name=ln, length_m=round(tl, 3), segments=ts,
        end_a_tension_kN=round(ea, 3), end_b_tension_kN=round(eb, 3),
        max_tension_kN=round(max(trg.Mean), 3),
        max_bending_kNm=round(max(abs(v) for v in brg.Mean), 3))

def extract_range(model, ln: str) -> RangeData:
    line = model[ln]
    trg = line.RangeGraph("Effective Tension"); brg = line.RangeGraph("Bend Moment")
    xrg = line.RangeGraph("x"); zrg = line.RangeGraph("z")
    return RangeData(arc_length=[round(v, 4) for v in trg.X],
        tension=[round(v, 4) for v in trg.Mean],
        bending=[round(v, 4) for v in brg.Mean],
        x=[round(v, 4) for v in xrg.Mean], z=[round(v, 4) for v in zrg.Mean])

def _extract_all(model) -> tuple[dict, dict]:
    lines, rd = {}, {}
    for o in model.objects:
        if o.type == OrcFxAPI.ObjectType.Line:
            try:
                lines[o.Name] = extract_line(model, o.Name)
                rd[o.Name] = extract_range(model, o.Name)
            except Exception as e:
                _flush(f"    WARNING: extraction failed for '{o.Name}': {e}")
    return lines, rd

# ---------------------------------------------------------------------------
# Load + run statics
# ---------------------------------------------------------------------------
def load_and_run(dat_path: Path) -> ModelBenchmark:
    try: model = OrcFxAPI.Model(str(dat_path))
    except Exception as e: return _fail_bm(dat_path, f"Load error: {e}")
    c = _counts(model)
    converged, elapsed, error = _calc_statics(model)
    lines, rd = _extract_all(model) if converged else ({}, {})
    return ModelBenchmark(name=dat_path.stem, category=_cat(dat_path),
        dat_path=str(dat_path), file_size_kb=dat_path.stat().st_size // 1024,
        object_count=c["total"], line_count=c["lines"], link_count=c["links"],
        vessel_count=c["vessels"], buoy_count=c["buoys"],
        statics_converged=converged, statics_time_s=elapsed,
        error_message=error, lines=lines, range_data=rd)

# ---------------------------------------------------------------------------
# 3-way comparison
# ---------------------------------------------------------------------------
def run_spec_driven(dat_path: Path) -> tuple[dict, dict, bool, float, str | None]:
    """Run Path B: .dat -> YAML -> extract spec -> generate modular -> statics.

    Returns:
        (lines, range_data, converged, elapsed, error)
    """
    if not _HAS_MODULAR:
        return {}, {}, False, 0.0, "Modular generator not available"
    with tempfile.TemporaryDirectory() as tmpdir:
        tmp = Path(tmpdir)
        # Export .dat to YAML
        model = OrcFxAPI.Model(str(dat_path))
        yml_path = tmp / "mono.yml"
        model.SaveData(str(yml_path))
        # Extract to spec
        extractor = MonolithicExtractor(yml_path)
        spec_dict = extractor.extract()
        spec = ProjectInputSpec(**spec_dict)
        # Generate modular
        gen = ModularModelGenerator.from_spec(spec)
        mod_dir = tmp / "modular"
        gen.generate(mod_dir)
        # Load generated master.yml and run statics
        master = mod_dir / "master.yml"
        model2 = OrcFxAPI.Model(str(master))
        converged, elapsed, error = _calc_statics(model2)
        lines, rd = _extract_all(model2) if converged else ({}, {})
        return lines, rd, converged, elapsed, error

def run_modular_direct(spec_path: Path) -> tuple[dict, dict, bool, float, str | None]:
    """Run Path C: library spec.yml -> generate modular -> statics.

    Returns:
        (lines, range_data, converged, elapsed, error)
    """
    if not _HAS_MODULAR:
        return {}, {}, False, 0.0, "Modular generator not available"
    with tempfile.TemporaryDirectory() as tmpdir:
        tmp = Path(tmpdir)
        gen = ModularModelGenerator(spec_path)
        mod_dir = tmp / "modular"
        gen.generate(mod_dir)
        master = mod_dir / "master.yml"
        model = OrcFxAPI.Model(str(master))
        converged, elapsed, error = _calc_statics(model)
        lines, rd = _extract_all(model) if converged else ({}, {})
        return lines, rd, converged, elapsed, error

# ---------------------------------------------------------------------------
# Mesh sensitivity
# ---------------------------------------------------------------------------
def _estimate_segments(model, scale: float) -> int:
    """Estimate total segments after scaling (before statics)."""
    total = 0
    for o in model.objects:
        if o.type == OrcFxAPI.ObjectType.Line:
            try:
                for i in range(o.NumberOfSections):
                    seg_len = max(0.1, o.TargetSegmentLength[i] * scale)
                    total += max(1, int(o.Length[i] / seg_len + 0.5))
            except Exception:
                pass
    return total

def run_mesh_sensitivity(dat_path: Path) -> dict[str, MeshLevel]:
    levels: dict[str, MeshLevel] = {}
    for lname in MESH_ORDER:
        scale = MESH_SCALES[lname]
        try: model = OrcFxAPI.Model(str(dat_path))
        except Exception:
            levels[lname] = MeshLevel(scale=scale, label=lname.replace("_"," ").title(),
                total_segments=0, statics_time_s=0.0, converged=False)
            _flush(f"      {lname}: LOAD FAIL"); continue
        # Estimate segments and skip if too many
        est = _estimate_segments(model, scale)
        if est > MAX_SEGMENTS:
            levels[lname] = MeshLevel(scale=scale, label=lname.replace("_"," ").title(),
                total_segments=est, statics_time_s=0.0, converged=False)
            _flush(f"      {lname}: SKIP ({est} est. segs > {MAX_SEGMENTS} limit)")
            continue
        for o in model.objects:
            if o.type == OrcFxAPI.ObjectType.Line:
                try:
                    for i in range(o.NumberOfSections):
                        o.TargetSegmentLength[i] = max(0.1, o.TargetSegmentLength[i] * scale)
                except Exception: pass
        ok, elapsed, _ = _calc_statics(model)
        lr, rr = _extract_all(model) if ok else ({}, {})
        ts = _recount(model) if ok else 0
        levels[lname] = MeshLevel(scale=scale, label=lname.replace("_"," ").title(),
            total_segments=ts, statics_time_s=elapsed, converged=ok, lines=lr, range_data=rr)
        _flush(f"      {lname}: {'OK' if ok else 'FAIL'} ({elapsed:.1f}s, {ts} segs)")
    return levels

def compute_convergence(levels: dict[str, MeshLevel]) -> list[MeshConvergence]:
    co = [n for n in MESH_ORDER if n in levels and levels[n].converged]
    if len(co) < 2: return []
    common = set.intersection(*(set(levels[n].lines.keys()) for n in co))
    if not common: return []
    results: list[MeshConvergence] = []
    for ln in sorted(common):
        for mname, attr in [("max_tension_kN","max_tension_kN"),
                            ("max_bending_kNm","max_bending_kNm")]:
            vals = {l: getattr(levels[l].lines[ln], attr) for l in co}
            od = round(pct_diff(vals[co[0]], vals[co[-1]]), 2)
            adj: dict[str, float] = {}
            for i in range(len(co)-1):
                adj[f"{co[i]}->{co[i+1]}"] = round(pct_diff(vals[co[i]], vals[co[i+1]]), 2)
            rec = None
            for i in range(len(co)-1):
                if adj[f"{co[i]}->{co[i+1]}"] < CONVERGENCE_THR: rec = co[i]; break
            if rec is None: rec = co[-1]
            results.append(MeshConvergence(line_name=ln, metric=mname, values=vals,
                coarsest_to_finest_pct=od, converged=od < CONVERGENCE_THR,
                adjacent_diffs=adj, recommended_level=rec))
    return results

# ---------------------------------------------------------------------------
# Discovery + JSON
# ---------------------------------------------------------------------------
def discover_models() -> list[Path]:
    if not EXAMPLES_ROOT.exists(): print(f"ERROR: {EXAMPLES_ROOT} not found"); return []
    fs = sorted(EXAMPLES_ROOT.rglob("*.dat"), key=lambda p: p.stat().st_size)
    return [f for f in fs if f.stat().st_size < MAX_FILE_SIZE]

def save_json(results: list[ModelBenchmark]) -> None:
    OUTPUT_ROOT.mkdir(parents=True, exist_ok=True)
    with open(JSON_PATH, "w", encoding="utf-8") as f:
        json.dump([asdict(r) for r in results], f, indent=2, default=str)
    print(f"\nJSON: {JSON_PATH} ({JSON_PATH.stat().st_size:,} bytes)")

def load_json() -> list[ModelBenchmark]:
    with open(JSON_PATH, encoding="utf-8") as f: data = json.load(f)
    results: list[ModelBenchmark] = []
    for e in data:
        ln = {k: LineResult(**v) for k, v in e.pop("lines", {}).items()}
        rd = {k: RangeData(**v) for k, v in e.pop("range_data", {}).items()}
        mlr = e.pop("mesh_levels", {})
        ml: dict[str, MeshLevel] = {}
        for mn, md in mlr.items():
            mll = {k: LineResult(**v) for k, v in md.pop("lines", {}).items()}
            mlrd = {k: RangeData(**v) for k, v in md.pop("range_data", {}).items()}
            ml[mn] = MeshLevel(**md, lines=mll, range_data=mlrd)
        mc = [MeshConvergence(**m) for m in e.pop("mesh_convergence", [])]
        # 3-way: Path B (spec-driven)
        sl = {k: LineResult(**v) for k, v in e.pop("spec_lines", {}).items()}
        srd = {k: RangeData(**v) for k, v in e.pop("spec_range_data", {}).items()}
        # 3-way: Path C (modular-direct)
        mol = {k: LineResult(**v) for k, v in e.pop("modular_lines", {}).items()}
        mord = {k: RangeData(**v) for k, v in e.pop("modular_range_data", {}).items()}
        results.append(ModelBenchmark(**e, lines=ln, range_data=rd,
            mesh_levels=ml, mesh_convergence=mc,
            spec_lines=sl, spec_range_data=srd,
            modular_lines=mol, modular_range_data=mord))
    return results

# ---------------------------------------------------------------------------
# CSS (matches riser benchmark)
# ---------------------------------------------------------------------------
_CSS = (
"*{box-sizing:border-box}"
"body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Arial,sans-serif;"
"margin:0;padding:0;color:#333;background:#f8f9fa;font-size:14px;line-height:1.5}"
".container{max-width:1400px;margin:0 auto;padding:1.5em 2em}"
".report-header{background:#2c3e50;color:#fff;padding:1.2em 2em;margin-bottom:1.5em;border-radius:6px}"
".report-header h1{margin:0 0 .3em;font-size:1.6em}"
".report-header .meta{font-size:.9em;opacity:.85}"
".section{background:#fff;border-radius:6px;box-shadow:0 1px 3px rgba(0,0,0,.08);"
"margin-bottom:1.5em;padding:1.2em 1.5em}"
".section h2{margin:0 0 .8em;font-size:1.2em;color:#2c3e50;"
"border-bottom:2px solid #3498db;padding-bottom:.3em}"
".section h3{font-size:1em;color:#2c3e50;margin:1em 0 .5em;"
"border-bottom:1px solid #ddd;padding-bottom:.15em}"
"table{border-collapse:collapse;margin:.5em 0 1em;font-size:.85em;width:100%}"
"th,td{border:1px solid #ddd;padding:.45em .7em;text-align:left}"
"th{background:#34495e;color:#fff;font-weight:600;font-size:.85em;"
"text-transform:uppercase;letter-spacing:.3px}"
"tbody tr:nth-child(even){background:#f8f9fa}"
"tbody tr:nth-child(odd){background:#fff}"
"tbody tr:hover{background:#ebf5fb}"
"td.num{text-align:right;font-family:'SF Mono','Cascadia Code','Consolas',monospace;font-size:.85em}"
".section-row td{background:#2c3e50!important;color:#fff;font-weight:700;"
"font-size:.8em;text-transform:uppercase;letter-spacing:.5px;padding:.5em .7em}"
".diff-ok{color:#27ae60;font-weight:600}"
".diff-warn{color:#f39c12;font-weight:600}"
".diff-high{color:#e74c3c;font-weight:600}"
"tr.status-pass{background:#eafaf1!important}"
"tr.status-fail{background:#fdedec!important}"
".badge{display:inline-block;padding:3px 10px;border-radius:3px;color:#fff;font-size:.8em;font-weight:700}"
".badge-pass{background:#27ae60}.badge-fail{background:#e74c3c}.badge-warn{background:#f39c12}"
".badge-rec{background:#27ae60;color:#fff;padding:2px 8px;border-radius:4px;font-size:.8em;font-weight:600}"
".model-grid{display:grid;grid-template-columns:45% 55%;gap:1em;align-items:start}"
".model-text{font-size:.85em}.model-plot{min-height:380px}"
".chart-container{width:100%;min-height:380px;margin:.8em 0}"
".schematic-container{width:100%;min-height:320px;margin:.5em 0 1em;"
"border:1px solid #ddd;border-radius:6px;overflow:hidden}"
".toc{background:#fff;border:1px solid #ddd;border-radius:6px;padding:1em 1.2em;"
"margin-bottom:1.5em;box-shadow:0 1px 3px rgba(0,0,0,.08)}"
".toc a{color:#3498db;text-decoration:none;font-size:.85em}"
".toc a:hover{text-decoration:underline}.toc ol{padding-left:1.5em}.toc li{margin:.2em 0}"
".summary-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(200px,1fr));"
"gap:1em;margin:1em 0 1.5em}"
".summary-card{background:#fff;border:1px solid #ddd;border-radius:6px;padding:1em;"
"text-align:center;box-shadow:0 1px 3px rgba(0,0,0,.08)}"
".summary-card .label{color:#888;font-size:.8em}"
".summary-card .value{font-size:1.4em;font-weight:700;margin:.3em 0;color:#2c3e50}"
".footer{margin-top:2em;padding-top:1em;border-top:1px solid #ddd;"
"color:#999;font-size:.75em;text-align:center}"
"@media(max-width:900px){.model-grid{grid-template-columns:1fr}}"
"@media print{body{background:#fff}.section{box-shadow:none;border:1px solid #ddd}}"
".file-browser{margin:1em 0}"
".file-browser details{border:1px solid #ddd;border-radius:4px;margin:.3em 0}"
".file-browser summary{padding:.5em .8em;background:#f0f0f0;cursor:pointer;"
"font-size:.85em;font-weight:600;color:#2c3e50}"
".file-browser summary:hover{background:#e0e0e0}"
".file-browser pre{margin:0;padding:.8em;background:#fafafa;"
"font-size:.78em;font-family:'SF Mono','Cascadia Code','Consolas',monospace;"
"overflow-x:auto;max-height:500px;overflow-y:auto;border-top:1px solid #eee}"
".file-browser .file-meta{color:#888;font-size:.75em;padding:.3em .8em;"
"border-top:1px solid #eee;background:#f8f8f8}"
)

# ---------------------------------------------------------------------------
# Plotly helpers
# ---------------------------------------------------------------------------
def _trace(x, y, name, color, width=2, dash=None):
    d = f"{{x:{json.dumps(x)},y:{json.dumps(y)},name:'{name}',mode:'lines',line:{{color:'{color}',width:{width}"
    if dash: d += f",dash:'{dash}'"
    return d + "}}"

def _layout(xt, yt, title, h=320):
    return (f"{{xaxis:{{title:'{xt}',automargin:true}},yaxis:{{title:'{yt}',automargin:true}},"
        f"legend:{{font:{{size:10}},orientation:'h',yanchor:'bottom',y:1.02,xanchor:'center',x:0.5}},"
        f"margin:{{l:60,r:30,t:40,b:40}},paper_bgcolor:'white',plot_bgcolor:'white',height:{h},"
        f"title:{{text:'{title}',font:{{size:13,color:'#2c3e50'}}}}}}")

# ---------------------------------------------------------------------------
# 3-way aggregate summary
# ---------------------------------------------------------------------------
def _build_3way_summary(results: list[ModelBenchmark]) -> str:
    """Build an aggregate 3-way comparison summary section."""
    rows: list[str] = []
    pass_b, warn_b, fail_b = 0, 0, 0
    pass_c, warn_c, fail_c = 0, 0, 0
    pass_bc, warn_bc, fail_bc = 0, 0, 0

    for r in results:
        has_spec = bool(r.spec_lines)
        has_mod = bool(r.modular_lines)
        if not has_spec and not has_mod:
            continue

        # Compute worst-case diff per path
        worst_spec = 0.0
        worst_mod = 0.0
        worst_bc = 0.0
        for ln_name, mono_lr in r.lines.items():
            for mattr in ("end_a_tension_kN", "end_b_tension_kN",
                          "max_tension_kN", "max_bending_kNm"):
                mono_val = getattr(mono_lr, mattr)
                if has_spec:
                    spec_lr = r.spec_lines.get(ln_name)
                    if spec_lr:
                        worst_spec = max(worst_spec,
                                         pct_diff(mono_val, getattr(spec_lr, mattr)))
                if has_mod:
                    mod_lr = r.modular_lines.get(ln_name)
                    if mod_lr:
                        worst_mod = max(worst_mod,
                                        pct_diff(mono_val, getattr(mod_lr, mattr)))
                if has_spec and has_mod:
                    spec_lr = r.spec_lines.get(ln_name)
                    mod_lr = r.modular_lines.get(ln_name)
                    if spec_lr and mod_lr:
                        worst_bc = max(worst_bc,
                                       pct_diff(getattr(spec_lr, mattr),
                                                getattr(mod_lr, mattr)))

        # Classify each path
        def _classify(val):
            if val < 1: return "pass"
            return "warn" if val < 5 else "fail"

        cls_spec = _classify(worst_spec) if has_spec else None
        cls_mod = _classify(worst_mod) if has_mod else None
        cls_bc = _classify(worst_bc) if has_spec and has_mod else None

        if cls_spec == "pass": pass_b += 1
        elif cls_spec == "warn": warn_b += 1
        elif cls_spec == "fail": fail_b += 1

        if cls_mod == "pass": pass_c += 1
        elif cls_mod == "warn": warn_c += 1
        elif cls_mod == "fail": fail_c += 1

        if cls_bc == "pass": pass_bc += 1
        elif cls_bc == "warn": warn_bc += 1
        elif cls_bc == "fail": fail_bc += 1

        # Table row
        def _fmt_cell(val, cls):
            if cls is None:
                return "<td>N/A</td>"
            return (f'<td class="num diff-{_dcls(val)}">{val:.2f}%</td>')

        def _status_badge(cls):
            if cls is None:
                return "<td>N/A</td>"
            badge_map = {"pass": "pass", "warn": "warn", "fail": "fail"}
            label_map = {"pass": "OK", "warn": "WARN", "fail": "FAIL"}
            return (f'<td><span class="badge badge-{badge_map[cls]}">'
                    f'{label_map[cls]}</span></td>')

        overall = cls_spec or "pass"
        if cls_mod and _dcls(worst_mod) != "ok":
            overall = cls_mod
        if cls_spec == "fail" or cls_mod == "fail":
            overall = "fail"
        elif cls_spec == "warn" or cls_mod == "warn":
            overall = "warn" if overall != "fail" else overall

        rows.append(
            f"<tr><td>{r.name}</td>"
            f"{_fmt_cell(worst_spec, cls_spec)}"
            f"{_fmt_cell(worst_mod, cls_mod)}"
            f"{_fmt_cell(worst_bc, cls_bc)}"
            f"{_status_badge(overall)}</tr>"
        )

    if not rows:
        return ""

    # Summary cards
    cards_html = '<div class="summary-grid">'
    for label, p, w, f_, color in [
        ("Spec-Driven", pass_b, warn_b, fail_b, "#3498db"),
        ("Modular-Direct", pass_c, warn_c, fail_c, "#27ae60"),
        ("B vs C", pass_bc, warn_bc, fail_bc, "#8e44ad"),
    ]:
        total = p + w + f_
        if total == 0:
            continue
        cards_html += (
            f'<div class="summary-card">'
            f'<div class="label">{label}</div>'
            f'<div class="value" style="color:{color}">'
            f'<span class="diff-ok">{p}</span> / '
            f'<span class="diff-warn">{w}</span> / '
            f'<span class="diff-high">{f_}</span>'
            f'</div>'
            f'<div class="label">pass / warn / fail</div></div>'
        )
    cards_html += '</div>'

    table_html = (
        '<table><thead><tr>'
        '<th>Model</th><th>Spec-Driven Worst %</th>'
        '<th>Modular-Direct Worst %</th><th>B vs C %</th>'
        '<th>Status</th></tr></thead><tbody>'
        + "\n".join(rows)
        + '</tbody></table>'
    )

    return (
        '<div class="section"><h2>3-Way Comparison Summary</h2>'
        + cards_html + table_html + '</div>'
    )


# ---------------------------------------------------------------------------
# HTML report
# ---------------------------------------------------------------------------
def generate_report(results: list[ModelBenchmark]) -> None:
    OUTPUT_ROOT.mkdir(parents=True, exist_ok=True)
    ts = time.strftime("%Y-%m-%d %H:%M")
    nt = len(results); np_ = sum(1 for r in results if r.statics_converged)
    nf = nt - np_; nm = sum(1 for r in results if r.mesh_levels)
    has_3way = any(r.spec_lines or r.spec_error or r.modular_lines or r.modular_error
                   for r in results)
    n_spec = sum(1 for r in results if r.spec_statics_converged)
    n_mod = sum(1 for r in results if r.modular_statics_converged)
    bp: list[str] = []; sp: list[str] = []

    # Summary cards
    bp.append('<div class="summary-grid">')
    cards = [("Total Models",nt,"#2c3e50"),("Converged",np_,"#27ae60"),
            ("Failed",nf,"#e74c3c" if nf else "#27ae60"),("Mesh Sensitivity",nm,"#3498db")]
    if has_3way:
        n_library = sum(1 for r in results if _find_library_spec(r.name) is not None)
        cards.append(("Library Models", f"{n_library}/{nt}", "#8e44ad"))
        cards.append(("Spec-Driven OK", n_spec, "#3498db"))
        cards.append(("Modular-Direct OK", n_mod, "#27ae60"))
    for lb, val, col in cards:
        bp.append(f'<div class="summary-card"><div class="label">{lb}</div>'
                  f'<div class="value" style="color:{col}">{val}</div></div>')
    bp.append("</div>")

    # Executive summary table
    exec_hdr = ("<th>#</th><th>Model</th><th>Category</th><th>Lines</th>"
        "<th>Objects</th><th>Statics</th><th>Time (s)</th><th>Max Tension (kN)</th>")
    if has_3way:
        exec_hdr += "<th>Spec</th><th>Modular</th>"
    exec_hdr += "<th>Mesh Sens.</th><th>Status</th>"
    bp.append(f'<div class="section"><h2>Executive Summary</h2>'
        f"<table><thead><tr>{exec_hdr}</tr></thead><tbody>")
    for i, r in enumerate(results, 1):
        rc = "status-pass" if r.statics_converged else "status-fail"
        sb = f'<span class="badge badge-{"pass" if r.statics_converged else "fail"}">{"PASS" if r.statics_converged else "FAIL"}</span>'
        mt = f"{max(l.max_tension_kN for l in r.lines.values()):.1f}" if r.lines else ""
        ms = ""
        if r.mesh_levels: ms = f"{sum(1 for m in r.mesh_levels.values() if m.converged)}/{len(r.mesh_levels)}"
        elif r.statics_converged: ms = "skipped"
        tw_cols = ""
        if has_3way:
            # Spec column
            if r.spec_lines:
                tw_cols += '<td><span class="badge badge-pass">PASS</span></td>'
            elif r.spec_error:
                tw_cols += '<td><span class="badge badge-fail">FAIL</span></td>'
            else:
                tw_cols += "<td>N/A</td>"
            # Modular column
            if r.modular_lines:
                tw_cols += '<td><span class="badge badge-pass">PASS</span></td>'
            elif r.modular_error:
                tw_cols += '<td><span class="badge badge-fail">FAIL</span></td>'
            else:
                tw_cols += "<td>N/A</td>"
        bp.append(f'<tr class="{rc}"><td class="num">{i}</td><td>{r.name}</td>'
            f'<td>{r.category}</td><td class="num">{r.line_count}</td>'
            f'<td class="num">{r.object_count}</td><td>{"Yes" if r.statics_converged else "No"}</td>'
            f'<td class="num">{r.statics_time_s:.2f}</td><td class="num">{mt}</td>'
            f"{tw_cols}<td>{ms}</td><td>{sb}</td></tr>")
    bp.append("</tbody></table></div>")

    # 3-way aggregate summary
    if has_3way:
        summary_3w = _build_3way_summary(results)
        if summary_3w:
            bp.append(summary_3w)

    # TOC
    conv = [r for r in results if r.statics_converged]
    if conv:
        bp.append('<div class="toc"><strong>Converged Model Details</strong><ol>'
            + "".join(f'<li><a href="#{_sid(r.name)}">{r.name}</a></li>' for r in conv)
            + "</ol></div>")

    # Per-model sections
    for r in conv:
        ms = _sid(r.name)
        bp.append(f'<div class="section" id="{ms}"><h2>{r.name}</h2>')

        # Schematic
        if r.range_data:
            sid = f"sch_{ms}"; bp.append(f'<h3>Static Profile</h3><div class="schematic-container" id="{sid}"></div>')
            trs = [_trace(rd.x, rd.z, ln, LINE_COLORS[i % len(LINE_COLORS)])
                   for i, (ln, rd) in enumerate(r.range_data.items())]
            lo = _layout("x (m)", "z (m)", f"Static Profile -- {r.name}")
            # Add scaleanchor for equal axes
            lo = lo.replace("automargin:true}", "automargin:true,scaleanchor:'y'}", 1)
            sp.append(f"Plotly.newPlot('{sid}',[{','.join(trs)}],{lo},{{responsive:true}});")

        # 2-column: info tables | tension chart
        bp.append('<div class="model-grid"><div class="model-text">')
        bp.append("<h3>Model Info</h3><table><thead><tr><th>Property</th><th>Value</th></tr></thead><tbody>"
            f'<tr><td>File</td><td>{Path(r.dat_path).name}</td></tr>'
            f'<tr><td>Size</td><td class="num">{r.file_size_kb} KB</td></tr>'
            f'<tr><td>Objects</td><td class="num">{r.object_count}</td></tr>'
            f'<tr><td>Lines</td><td class="num">{r.line_count}</td></tr>'
            f'<tr><td>Links</td><td class="num">{r.link_count}</td></tr>'
            f'<tr><td>Vessels</td><td class="num">{r.vessel_count}</td></tr>'
            f'<tr><td>6D Buoys</td><td class="num">{r.buoy_count}</td></tr>'
            f'<tr><td>Statics Time</td><td class="num">{r.statics_time_s:.3f} s</td></tr>'
            "</tbody></table>")
        if r.lines:
            bp.append("<h3>Line Results</h3><table><thead><tr><th>Line</th><th>Length (m)</th>"
                "<th>Segments</th><th>End A (kN)</th><th>End B (kN)</th><th>Max T (kN)</th>"
                "<th>Max BM (kN.m)</th></tr></thead><tbody>")
            for lr in r.lines.values():
                bp.append(f'<tr><td>{lr.name}</td><td class="num">{lr.length_m:.1f}</td>'
                    f'<td class="num">{lr.segments}</td><td class="num">{lr.end_a_tension_kN:.1f}</td>'
                    f'<td class="num">{lr.end_b_tension_kN:.1f}</td><td class="num">{lr.max_tension_kN:.1f}</td>'
                    f'<td class="num">{lr.max_bending_kNm:.2f}</td></tr>')
            bp.append("</tbody></table>")
        # File browser (collapsible input files)
        fb = _build_file_browser(r.name)
        if fb:
            bp.append(fb)
        bp.append("</div>")  # model-text

        # Right: tension along-length
        if r.range_data:
            cid = f"t_{ms}"; bp.append(f'<div class="model-plot" id="{cid}"></div>')
            trs = [_trace(rd.arc_length, rd.tension, ln, LINE_COLORS[i % len(LINE_COLORS)])
                   for i, (ln, rd) in enumerate(r.range_data.items())]
            sp.append(f"Plotly.newPlot('{cid}',[{','.join(trs)}],"
                f"{_layout('Arc Length (m)','Effective Tension (kN)','Tension Along Length',380)},"
                f"{{responsive:true}});")
        else:
            bp.append("<div></div>")
        bp.append("</div>")  # model-grid

        # 3-way comparison section
        if r.spec_lines or r.modular_lines:
            bp.append("<h3>3-Way Comparison</h3>")
            # Build per-line comparison table
            # Collect all line names across the paths, preserving monolithic order
            all_3w_lines = list(r.lines.keys())
            for ln_name in list(r.spec_lines.keys()) + list(r.modular_lines.keys()):
                if ln_name not in all_3w_lines:
                    all_3w_lines.append(ln_name)
            has_both = bool(r.spec_lines) and bool(r.modular_lines)
            bp.append("<table><thead><tr><th>Line</th><th>Metric</th>"
                "<th>Monolithic</th>")
            if r.spec_lines:
                bp.append("<th>Spec-Driven</th><th>Diff %</th>")
            if r.modular_lines:
                bp.append("<th>Modular-Direct</th><th>Diff %</th>")
            if has_both:
                bp.append("<th>B vs C %</th>")
            bp.append("<th>Status</th></tr></thead><tbody>")
            metrics = [
                ("End A Tension", "end_a_tension_kN"),
                ("End B Tension", "end_b_tension_kN"),
                ("Max Tension", "max_tension_kN"),
                ("Max Bending", "max_bending_kNm"),
            ]
            for ln_name in all_3w_lines:
                mono_lr = r.lines.get(ln_name)
                spec_lr = r.spec_lines.get(ln_name)
                mod_lr = r.modular_lines.get(ln_name)
                if not mono_lr:
                    continue
                for mi, (mlab, mattr) in enumerate(metrics):
                    mono_val = getattr(mono_lr, mattr)
                    row = f"<tr><td>{ln_name if mi == 0 else ''}</td><td>{mlab}</td>"
                    row += f'<td class="num">{mono_val:.2f}</td>'
                    worst_pct = 0.0
                    if r.spec_lines:
                        if spec_lr:
                            sv = getattr(spec_lr, mattr)
                            dp_ = round(pct_diff(mono_val, sv), 2)
                            worst_pct = max(worst_pct, dp_)
                            row += f'<td class="num">{sv:.2f}</td>'
                            row += f'<td class="num diff-{_dcls(dp_)}">{dp_:.2f}%</td>'
                        else:
                            row += "<td>--</td><td>--</td>"
                    if r.modular_lines:
                        if mod_lr:
                            mv = getattr(mod_lr, mattr)
                            dp_ = round(pct_diff(mono_val, mv), 2)
                            worst_pct = max(worst_pct, dp_)
                            row += f'<td class="num">{mv:.2f}</td>'
                            row += f'<td class="num diff-{_dcls(dp_)}">{dp_:.2f}%</td>'
                        else:
                            row += "<td>--</td><td>--</td>"
                    if has_both:
                        if spec_lr and mod_lr:
                            sv = getattr(spec_lr, mattr)
                            mv = getattr(mod_lr, mattr)
                            bc_ = round(pct_diff(sv, mv), 2)
                            row += f'<td class="num diff-{_dcls(bc_)}">{bc_:.2f}%</td>'
                        else:
                            row += "<td>--</td>"
                    sc = _dcls(worst_pct)
                    badge = "pass" if sc == "ok" else ("warn" if sc == "warn" else "fail")
                    row += f'<td><span class="badge badge-{badge}">{"OK" if sc == "ok" else sc.upper()}</span></td>'
                    row += "</tr>"
                    bp.append(row)
            bp.append("</tbody></table>")

            # 3-way overlaid tension chart per line
            for li, ln_name in enumerate(all_3w_lines):
                mono_rd = r.range_data.get(ln_name)
                spec_rd = r.spec_range_data.get(ln_name)
                mod_rd = r.modular_range_data.get(ln_name)
                if not mono_rd:
                    continue
                twid = f"tw_{ms}_{li}"
                bp.append(f'<div class="chart-container" id="{twid}"></div>')
                trs = []
                trs.append(_trace(mono_rd.arc_length, mono_rd.tension,
                    f"Monolithic: {ln_name}", THREE_WAY_COLORS["monolithic"], 2,
                    THREE_WAY_DASH["monolithic"]))
                if spec_rd:
                    trs.append(_trace(spec_rd.arc_length, spec_rd.tension,
                        f"Spec-Driven: {ln_name}", THREE_WAY_COLORS["spec_driven"], 2,
                        THREE_WAY_DASH["spec_driven"]))
                if mod_rd:
                    trs.append(_trace(mod_rd.arc_length, mod_rd.tension,
                        f"Modular-Direct: {ln_name}", THREE_WAY_COLORS["modular"], 2,
                        THREE_WAY_DASH["modular"]))
                title_esc = f"3-Way Tension -- {ln_name}".replace("'", "\\'")
                sp.append(f"Plotly.newPlot('{twid}',[{','.join(trs)}],"
                    f"{_layout('Arc Length (m)','Effective Tension (kN)',title_esc,350)},"
                    f"{{responsive:true}});")

                # Bending moment overlay
                bmid = f"bm_{ms}_{li}"
                bp.append(f'<div class="chart-container" id="{bmid}"></div>')
                bm_trs = []
                bm_trs.append(_trace(mono_rd.arc_length, mono_rd.bending,
                    f"Monolithic: {ln_name}", THREE_WAY_COLORS["monolithic"], 2,
                    THREE_WAY_DASH["monolithic"]))
                if spec_rd:
                    bm_trs.append(_trace(spec_rd.arc_length, spec_rd.bending,
                        f"Spec-Driven: {ln_name}", THREE_WAY_COLORS["spec_driven"], 2,
                        THREE_WAY_DASH["spec_driven"]))
                if mod_rd:
                    bm_trs.append(_trace(mod_rd.arc_length, mod_rd.bending,
                        f"Modular-Direct: {ln_name}", THREE_WAY_COLORS["modular"], 2,
                        THREE_WAY_DASH["modular"]))
                bm_title = f"3-Way Bend Moment -- {ln_name}".replace("'", "\\'")
                sp.append(f"Plotly.newPlot('{bmid}',[{','.join(bm_trs)}],"
                    f"{_layout('Arc Length (m)','Bend Moment (kN.m)',bm_title,350)},"
                    f"{{responsive:true}});")

        # 3-way error info (when paths failed)
        if r.spec_error and not r.spec_lines:
            bp.append(f'<h3>Spec-Driven Path</h3><p style="color:#e74c3c;font-size:.85em">'
                f'Error: {r.spec_error[:300]}</p>')
        if r.modular_error and not r.modular_lines:
            bp.append(f'<h3>Modular-Direct Path</h3><p style="color:#e74c3c;font-size:.85em">'
                f'Error: {r.modular_error[:300]}</p>')

        # Mesh sensitivity
        if r.mesh_levels:
            bp.append("<h3>Mesh Sensitivity</h3><div class=\"model-grid\"><div class=\"model-text\">")
            bp.append("<table><thead><tr><th>Level</th><th>Scale</th><th>Segments</th>"
                "<th>Time (s)</th><th>Converged</th></tr></thead><tbody>")
            for ln in MESH_ORDER:
                ml = r.mesh_levels.get(ln)
                if not ml: continue
                cb = f'<span class="badge badge-{"pass" if ml.converged else "fail"}">{"Yes" if ml.converged else "No"}</span>'
                bp.append(f"<tr><td>{ml.label}</td><td class=\"num\">{ml.scale:.3f}</td>"
                    f"<td class=\"num\">{ml.total_segments}</td><td class=\"num\">{ml.statics_time_s:.3f}</td><td>{cb}</td></tr>")
            bp.append("</tbody></table>")

            # Convergence table
            if r.mesh_convergence:
                cl = [n for n in MESH_ORDER if n in r.mesh_levels and r.mesh_levels[n].converged]
                bp.append("<h3>Convergence</h3><table><thead><tr><th>Line</th><th>Metric</th>"
                    + "".join(f"<th>{n.replace('_',' ').title()}</th>" for n in cl)
                    + "<th>Diff%</th><th>Recommended</th></tr></thead><tbody>")
                for mc in r.mesh_convergence:
                    dc = _dcls(mc.coarsest_to_finest_pct)
                    rl = (mc.recommended_level or "").replace("_"," ").title()
                    ml_ = mc.metric.replace("_kN"," (kN)").replace("_kNm"," (kN.m)")
                    row = f"<tr><td>{mc.line_name}</td><td>{ml_}</td>"
                    for n in cl: row += f'<td class="num">{mc.values.get(n,0):.2f}</td>'
                    row += f'<td class="diff-{dc}">{mc.coarsest_to_finest_pct:.2f}%</td>'
                    row += f'<td><span class="badge-rec">{rl}</span></td></tr>'
                    bp.append(row)
                bp.append("</tbody></table>")
            bp.append("</div>")  # model-text

            # Right: bar chart + overlay
            bid = f"mb_{ms}"; bp.append(f'<div class="model-plot"><div id="{bid}" style="min-height:300px"></div>')
            cl = [n for n in MESH_ORDER if n in r.mesh_levels and r.mesh_levels[n].converged]
            if cl:
                lsets = [set(r.mesh_levels[n].lines.keys()) for n in cl]
                clines = sorted(set.intersection(*lsets) if lsets else set())
                if clines:
                    bts = []
                    for cln in clines:
                        xv = [n.replace("_"," ").title() for n in cl]
                        yv = [r.mesh_levels[n].lines[cln].max_tension_kN for n in cl]
                        bts.append(f"{{x:{json.dumps(xv)},y:{json.dumps(yv)},name:'{cln}',type:'bar'}}")
                    lo = _layout('Mesh Level','Max Tension (kN)','Max Tension by Mesh Level',300)
                    # Inject barmode into layout object
                    lo = "{barmode:'group'," + lo[1:]
                    sp.append(f"Plotly.newPlot('{bid}',[{','.join(bts)}],{lo},{{responsive:true}});")
                    # Along-length overlay for first line
                    oid = f"mo_{ms}"
                    bp.append(f'<div id="{oid}" style="min-height:300px;margin-top:1em"></div>')
                    fl = clines[0]; ots = []
                    for ln in cl:
                        rd = r.mesh_levels[ln].range_data.get(fl)
                        if rd: ots.append(_trace(rd.arc_length, rd.tension,
                            ln.replace("_"," ").title(), MESH_COLORS.get(ln, "#94a3b8")))
                    sp.append(f"Plotly.newPlot('{oid}',[{','.join(ots)}],"
                        f"{_layout('Arc Length (m)','Tension (kN)',f'Tension Along Length ({fl})',300)},"
                        f"{{responsive:true}});")
            bp.append("</div></div>")  # model-plot, model-grid
        bp.append("</div>")  # section

    # Failed models
    failed = [r for r in results if not r.statics_converged]
    if failed:
        bp.append('<div class="section"><h2>Failed Models</h2><table><thead>'
            "<tr><th>#</th><th>Model</th><th>Category</th><th>Size</th><th>Error</th></tr></thead><tbody>")
        for i, r in enumerate(failed, 1):
            err = (r.error_message or "Unknown")[:200]
            bp.append(f'<tr class="status-fail"><td class="num">{i}</td><td>{r.name}</td>'
                f'<td>{r.category}</td><td class="num">{r.file_size_kb} KB</td>'
                f'<td style="font-size:.8em">{err}</td></tr>')
        bp.append("</tbody></table></div>")

    # Assemble
    html = (f'<!DOCTYPE html>\n<html lang="en">\n<head>\n<meta charset="utf-8">\n'
        f'<meta name="viewport" content="width=device-width,initial-scale=1.0">\n'
        f'<title>OrcaFlex Model Library Benchmark</title>\n'
        f'<script src="{PLOTLY_CDN}"></script>\n<style>{_CSS}</style>\n</head>\n'
        f'<body>\n<div class="container">\n<div class="report-header">\n'
        f'<h1>OrcaFlex Model Library Benchmark</h1>\n<div class="meta">\n'
        f'<strong>Models:</strong> {nt} &nbsp;|&nbsp; <strong>Converged:</strong> {np_}/{nt}'
        f' &nbsp;|&nbsp; <strong>Mesh Sensitivity:</strong> {nm}'
        + (f' &nbsp;|&nbsp; <strong>3-Way:</strong> Spec {n_spec} / Modular {n_mod}'
           if has_3way else '')
        + f' &nbsp;|&nbsp; <strong>Date:</strong> {ts}</div>\n</div>\n'
        + "\n".join(bp)
        + f'\n<div class="footer">Generated {ts} &mdash; scripts/benchmark_model_library.py'
        f' &mdash; digitalmodel</div>\n</div>\n<script>\n'
        + "\n".join(sp) + '\n</script>\n</body>\n</html>')
    HTML_PATH.write_text(html, encoding="utf-8")
    print(f"HTML: {HTML_PATH} ({HTML_PATH.stat().st_size:,} bytes)")
    DOCS_OUTPUT.mkdir(parents=True, exist_ok=True)
    DOCS_HTML_PATH.write_text(html, encoding="utf-8")
    print(f"Docs: {DOCS_HTML_PATH} ({DOCS_HTML_PATH.stat().st_size:,} bytes)")

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
def main() -> None:
    parser = argparse.ArgumentParser(description="OrcaFlex Model Library Benchmark")
    parser.add_argument("--html-only", action="store_true", help="Regenerate HTML from JSON")
    parser.add_argument("--max-models", type=int, default=0, help="Limit models (0=all)")
    parser.add_argument("--skip-mesh", action="store_true", help="Skip mesh sensitivity")
    parser.add_argument("--three-way", action="store_true",
        help="Enable 3-way comparison (monolithic vs spec-driven vs modular-direct)")
    parser.add_argument("--library-only", action="store_true",
        help="Only process models with a matching library spec.yml (implies --three-way)")
    args = parser.parse_args()

    if args.three_way and not _HAS_MODULAR:
        _flush("WARNING: --three-way requires modular generator imports.")
        _flush("  Install: digitalmodel.solvers.orcaflex.modular_generator")
        _flush("  Falling back to standard benchmark mode.")

    if args.html_only:
        _flush(f"Loading {JSON_PATH}..."); r = load_json()
        _flush(f"Loaded {len(r)} models"); generate_report(r); return

    t0 = time.time()
    dats = discover_models()
    if not dats: _flush("No .dat files found"); return
    if args.library_only:
        args.three_way = True
        dats = [d for d in dats if _find_library_spec(d.stem) is not None]
        _flush(f"Library-only mode: {len(dats)} models with matching spec.yml")
    if args.max_models > 0: dats = dats[:args.max_models]
    _flush(f"Found {len(dats)} models (max {MAX_FILE_SIZE // 1_000_000} MB)")
    _flush(f"Mesh sensitivity: {'OFF' if args.skip_mesh else f'ON (statics < {MESH_TIME_LIMIT:.0f}s)'}")
    _flush(f"3-way comparison: {'ON' if args.three_way and _HAS_MODULAR else 'OFF'}")
    _flush(f"Statics timeout: {STATICS_TIMEOUT}s | Max segments: {MAX_SEGMENTS}")

    results: list[ModelBenchmark] = []
    for i, dp in enumerate(dats, 1):
        _flush(f"\n{'='*60}\n[{i}/{len(dats)}] {dp.stem}\n  File: {dp} ({dp.stat().st_size // 1024} KB)")
        try: bm = load_and_run(dp)
        except Exception as e:
            _flush(f"  UNEXPECTED ERROR: {e}"); bm = _fail_bm(dp, f"Unexpected: {e}")
        st = "CONVERGED" if bm.statics_converged else "FAILED"
        _flush(f"  Statics: {st} ({bm.statics_time_s:.2f}s, {bm.object_count} obj, {bm.line_count} lines)")
        if bm.error_message: _flush(f"  Error: {bm.error_message}")
        if bm.lines: _flush(f"  Max tension: {max(l.max_tension_kN for l in bm.lines.values()):.1f} kN")

        # 3-way comparison (only for converged models)
        if args.three_way and _HAS_MODULAR and bm.statics_converged:
            # Path B: spec-driven
            _flush("  3-way Path B (spec-driven)...")
            try:
                sl, srd, sc, st_s, se = run_spec_driven(dp)
                bm.spec_lines = sl
                bm.spec_range_data = srd
                bm.spec_statics_converged = sc
                bm.spec_statics_time_s = st_s
                bm.spec_error = se
                _flush(f"    Spec-driven: {'CONVERGED' if sc else 'FAILED'} ({st_s:.2f}s)")
                if se: _flush(f"    Error: {se}")
            except Exception as e:
                bm.spec_error = str(e)
                _flush(f"    Spec-driven ERROR: {e}")

            # Path C: modular-direct (only if library spec.yml exists)
            spec_path = _find_library_spec(bm.name)
            if spec_path:
                _flush(f"  3-way Path C (modular-direct): {spec_path}")
                try:
                    ml, mrd, mc_, mt_s, me = run_modular_direct(spec_path)
                    bm.modular_lines = ml
                    bm.modular_range_data = mrd
                    bm.modular_statics_converged = mc_
                    bm.modular_statics_time_s = mt_s
                    bm.modular_error = me
                    _flush(f"    Modular-direct: {'CONVERGED' if mc_ else 'FAILED'} ({mt_s:.2f}s)")
                    if me: _flush(f"    Error: {me}")
                except Exception as e:
                    bm.modular_error = str(e)
                    _flush(f"    Modular-direct ERROR: {e}")
            else:
                _flush("  3-way Path C: no library spec.yml found")

        if not args.skip_mesh and bm.statics_converged and bm.statics_time_s < MESH_TIME_LIMIT:
            _flush(f"  Mesh sensitivity (baseline {bm.statics_time_s:.1f}s)...")
            try:
                bm.mesh_levels = run_mesh_sensitivity(dp)
                bm.mesh_convergence = compute_convergence(bm.mesh_levels)
            except Exception as e: _flush(f"  Mesh sensitivity FAILED: {e}")
        results.append(bm)

    te = time.time() - t0
    np_ = sum(1 for r in results if r.statics_converged)
    nm = sum(1 for r in results if r.mesh_levels)
    n_spec = sum(1 for r in results if r.spec_statics_converged)
    n_mod = sum(1 for r in results if r.modular_statics_converged)
    _flush(f"\n{'='*60}\nDONE: {len(results)} models in {te:.1f}s")
    _flush(f"  Converged: {np_}/{len(results)}  |  Mesh sensitivity: {nm}")
    if args.three_way and _HAS_MODULAR:
        _flush(f"  3-way: Spec-driven {n_spec}/{np_}  |  Modular-direct {n_mod}/{np_}")
    save_json(results); generate_report(results)

if __name__ == "__main__":
    main()
