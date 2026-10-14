"""Build and run matched roll/pitch sloshing cases, one case at a time.

The frame is x transverse, y up, z longitudinal. Vessel roll is rotation
about z; vessel pitch is about x. Outputs are execution evidence, without
physical validation. Production comparisons and acceptance belong to the
separate results dataset. Source equations and reductions are reused from
sloshing_2d_config, sloshing_2d_dicts and sloshing_sweep_reduction.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
import shutil
import subprocess
import tempfile
import time
from datetime import datetime, timezone
import re
from pathlib import Path

from digitalmodel.solvers.openfoam.validation.sloshing_2d import (
    SloshingForcedRollConfig, SloshingFreeDecayConfig,
    build_forced_roll_case, build_free_decay_case,
)
from digitalmodel.solvers.openfoam.validation.sloshing_3d import (
    Sloshing3DConfig, write_sloshing_case,
)
from run_sloshing_response_sweep import PERIOD_RATIOS, _patch_wall_probe

SETS = ("reproduction-2d", "longitudinal-2d", "free-decay-3d", "medium-3d",
        "coarse-3d", "fine-3d", "time-step-3d")


def case_specs(*, t1_roll=None, t1_pitch=None, ranks=4, smoke=False):
    """Enumerate the fixed tank and uniform meshes; cpb is per 0.9 m.

    Explicit periods are the corresponding 2D basis. Omitted values are
    analytical preparation values and are labelled accordingly.
    """
    if type(ranks) is not int or not 1 <= ranks <= 4:
        raise ValueError("ranks must be an integer from 1 to 4")
    specs = []
    for direction, section, supplied in (("roll", 0.9, t1_roll), ("pitch", 1.35, t1_pitch)):
        analytical = SloshingForcedRollConfig(breadth=section, tank_height=0.9, fill_depth=0.45).first_mode_period
        t1 = analytical if supplied is None else supplied
        if not math.isfinite(t1) or t1 <= 0:
            raise ValueError("natural periods must be finite and positive")
        for group in SETS:
            if group == "reproduction-2d" and direction != "roll":
                continue
            if group == "longitudinal-2d" and direction != "pitch":
                continue
            kinds = ("free-decay", "forced") if group == "longitudinal-2d" else ("free-decay",) if group == "free-decay-3d" else ("forced",)
            for kind in kinds:
                ratios = PERIOD_RATIOS if kind == "forced" and group in ("longitudinal-2d", "medium-3d") else (1.0,)
                cpb = 80 if group == "fine-3d" or (group == "longitudinal-2d" and kind == "free-decay") else 40 if group == "coarse-3d" else 60
                dimension = 2 if group.endswith("2d") else 3
                nx = round((section if dimension == 2 else 0.9) / 0.9 * cpb)
                for ratio in ratios:
                    period = t1 * ratio
                    specs.append(dict(
                        name=f'{"smoke-" if smoke else ""}{group}-{direction}-{kind}-r{ratio:.2f}',
                        group=group, dimension=dimension, direction=direction, kind=kind,
                        ratio=ratio, period=period, t1_s=t1, analytical_t1_s=analytical,
                        period_basis="analytical" if supplied is None else "two-dimensional-input",
                        breadth=section if dimension == 2 else 0.9, height=0.9,
                        length=0.01 if dimension == 2 else 1.35, fill_depth=0.45,
                        cpb=cpb, nx=nx, ny=cpb, nz=1 if dimension == 2 else round(1.35 / 0.9 * cpb),
                        decomposition=None if dimension == 2 else dict(method="simple", n=[1, ranks, 1], delta=0.001),
                        ranks=1 if dimension == 2 else ranks, max_co=0.25 if group == "time-step-3d" else 0.5,
                        delta_t=0.002 if kind == "free-decay" else 0.001,
                        end_time=0.03 if smoke else (10 * t1 if kind == "free-decay" else 6 * period),
                        smoke=smoke,
                    ))
    return specs


def _static_decay(case, spec):
    """Extend the held cosine_mode_setfields_body to the excitation axis."""
    field = case / "0/U"
    field.write_text(field.read_text().replace("type movingWallVelocity; value uniform (0 0 0);", "type slip;"))
    (case / "constant/dynamicMeshDict").write_text(
        'FoamFile { version 2.0; format ascii; class dictionary; object dynamicMeshDict; }\ndynamicFvMesh staticFvMesh;\n')
    along_x = spec["direction"] == "roll"
    n = spec["nx"] if along_x else spec["nz"]
    section = spec["breadth"] if along_x else spec["length"]
    lines = ["defaultFieldValues ( volScalarFieldValue alpha.water 0 );", "regions", "("]
    for i in range(n):
        lo, hi = i * section / n, (i + 1) * section / n
        eta = spec["fill_depth"] + 0.02 * math.cos(math.pi * (lo + hi) / (2 * section))
        lower = (lo, 0, 0) if along_x else (0, 0, lo)
        upper = (hi, eta, spec["length"]) if along_x else (spec["breadth"], eta, hi)
        a, b = (" ".join(f"{v:.6g}" for v in xyz) for xyz in (lower, upper))
        lines.append(f"boxToCell {{ box ({a}) ({b}); fieldValues (volScalarFieldValue alpha.water 1); }}")
    (case / "system/setFieldsDict").write_text(
        'FoamFile { version 2.0; format ascii; class dictionary; object setFieldsDict; }\n' + "\n".join(lines) + "\n);\n")
    control = case / "system/controlDict"
    text = re.sub(r"adjustTimeStep\s+yes;", "adjustTimeStep  no;", control.read_text())
    # Fixed dt and stride match the held decay's uniformly sampled FFT input.
    text = text.replace("writeInterval   5;", "writeInterval   10;")
    control.write_text(text)


def _probe_decomposition(spec):
    """Keep vertical interface-height rays off parallel partition faces.

    interfaceHeight.C sums per-rank line integrals. simpleGeomDecomp.C assigns
    equal cell groups, so complete vertical layers are required. The geometric
    perturbation is the installed geomDecomp.C default.
    """
    if spec["ny"] % spec["ranks"]:
        raise ValueError("vertical cell count must be divisible by ranks for probe sampling")
    n = " ".join(str(v) for v in spec["decomposition"]["n"])
    return ('FoamFile { version 2.0; format ascii; class dictionary; object decomposeParDict; }\n'
            f'numberOfSubdomains {spec["ranks"]};\nmethod simple;\n'
            f'simpleCoeffs {{ n ({n}); delta {spec["decomposition"]["delta"]}; }}\n')


def build_case(work_dir, spec):
    """Create deterministic inputs, refusing to overwrite an existing case."""
    decomposition = _probe_decomposition(spec) if spec["dimension"] == 3 else None
    case = Path(work_dir) / spec["name"]
    if case.exists():
        old = json.loads((case / "_spec.json").read_text())
        if old != spec:
            raise ValueError("existing case specification differs")
        return case
    if spec["dimension"] == 3:
        cfg = Sloshing3DConfig(
            cpb=spec["cpb"], end_time=spec["end_time"], delta_t=spec["delta_t"],
            length=spec["length"], rotation=spec["direction"], period=spec["period"],
            max_co=spec["max_co"], function_objects=True, use_2d_settings=True,
            decompose_ranks=spec["ranks"],
        )
        write_sloshing_case(case, cfg)
        (case / "system/decomposeParDict").write_text(decomposition)
        if spec["kind"] == "free-decay":
            _static_decay(case, spec)
    elif spec["kind"] == "free-decay":
        build_free_decay_case(SloshingFreeDecayConfig(
            breadth=spec["breadth"], tank_height=spec["height"], fill_level=spec["fill_depth"] / spec["height"],
            cells_per_breadth=spec["nx"], end_time=spec["end_time"], name=spec["name"],
        ), work_dir)
    else:
        build_forced_roll_case(SloshingForcedRollConfig(
            breadth=spec["breadth"], tank_height=spec["height"], fill_depth=spec["fill_depth"],
            cells_per_breadth=spec["nx"], roll_period=spec["period"],
            n_cycles=spec["end_time"] / spec["period"], name=spec["name"],
        ), work_dir, with_moment=True)
        _patch_wall_probe(case, spec["breadth"], spec["nx"])
    control = case / "system/controlDict"
    text = re.sub(r"startFrom\s+startTime;", "startFrom latestTime;", control.read_text())
    # Replace only the top-level field write interval, never function strides.
    head, functions = text.split("functions", 1)
    head = re.sub(r"writeInterval\s+[\d.e+-]+;", f'writeInterval {min(0.05, spec["end_time"]):.8g};', head)
    head = re.sub(r"purgeWrite\s+\d+;", "purgeWrite 0;", head)
    control.write_text(head + "functions" + functions)
    shutil.copytree(case / "0", case / "_input_fields")
    (case / "_spec.json").write_text(json.dumps(spec, indent=2, sort_keys=True) + "\n")
    return case


def _json_write(path, value):
    path.write_text(json.dumps(value, indent=2, sort_keys=True, allow_nan=False) + "\n")


def _file_digests(case, paths):
    return {p.relative_to(case).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest()
            for p in sorted(paths) if p.is_file()}


def input_digest(case):
    """Bind configuration, immutable initial fields and all reduction sources."""
    root = Path(__file__).resolve().parents[2]
    names = ("motion.py", "partial_fill.py", "spectral_analysis.py", "runner.py")
    package = root / "src/digitalmodel/solvers/openfoam"
    sources = [Path(__file__), Path(__file__).with_name("run_sloshing_response_sweep.py")]
    sources += [package / n for n in names]
    sources += list((package / "validation").glob("sloshing*.py"))
    files = list((case / "system").glob("*")) + list((case / "constant").glob("*"))
    files += list((case / "_input_fields").glob("*")) + [case / "_spec.json"]
    payload = dict(inputs=_file_digests(case, files), code=_file_digests(root, sources))
    return hashlib.sha256(json.dumps(payload, sort_keys=True).encode()).hexdigest()


def _available_memory_gb():
    """Available Linux memory; unavailable evidence prevents a launch."""
    lines = Path("/proc/meminfo").read_text().splitlines()
    return next(float(line.split()[1]) / 1024 ** 2 for line in lines if line.startswith("MemAvailable:"))


def _launch(case, argv):
    """Stream native output to disk; no automatic retries or solver timeout."""
    from digitalmodel.solvers.openfoam.runner import OpenFOAMRunner
    if _available_memory_gb() < 6:
        raise RuntimeError("available memory is below 6 GB")
    name = "interFoam" if argv[0] == "mpirun" else argv[0]
    log = case / f"log.{name}"
    offset = log.stat().st_size if log.exists() else 0
    start = time.monotonic()
    with log.open("a") as handle:
        proc = subprocess.run(argv, cwd=case, stdout=handle, stderr=subprocess.STDOUT, check=False)
    with log.open("rb") as handle:
        handle.seek(offset)
        output = handle.read().decode(errors="replace")
    utility = name in {"blockMesh", "setFields", "decomposePar"}
    if problem := OpenFOAMRunner._detect_error(name, proc.returncode, output, check_divergence=not utility):
        raise RuntimeError(f"{' '.join(argv)}: {problem}")
    return time.monotonic() - start


def _merged_history(case, name, filename):
    """Numerically merge native restart histories; reject contradictory rows."""
    files = sorted((case / "postProcessing" / name).glob(f"*/{filename}"), key=lambda p: float(p.parent.name))
    if not files:
        raise FileNotFoundError(f"missing {name}/{filename}")
    rows = {}
    for path in files:
        start = float(path.parent.name)
        # A resumed segment supersedes the uncheckpointed earlier trajectory.
        # Raw histories remain untouched for audit and reduction readback.
        rows = {t: row for t, row in rows.items() if t < start}
        for line in path.read_text().splitlines():
            if not line.strip() or line.lstrip().startswith("#"):
                continue
            values = [float(v) for v in line.replace("(", " ").replace(")", " ").split()]
            if not all(math.isfinite(v) for v in values):
                raise ValueError("nonfinite native output")
            timestamp = values[0]
            if timestamp < start:
                raise ValueError("contradictory duplicate output time precedes segment start")
            if timestamp in rows and rows[timestamp][0] != values:
                raise ValueError("contradictory duplicate output time")
            rows[timestamp] = (values, line)
    if len(rows) < 4:
        raise ValueError("need at least four native output samples")
    return [(t, *rows[t]) for t in sorted(rows)]


def _signal(case, name, filename, spec):
    """Reuse the held parsers after numeric ordering and axis selection."""
    from digitalmodel.solvers.openfoam.validation.sloshing_2d import parse_interface_height, parse_roll_moment
    rows = _merged_history(case, name, filename)
    stride = 10 if spec["kind"] == "free-decay" else 5
    tolerance = stride * spec["delta_t"] * 1.1
    if rows[0][0] > tolerance or rows[-1][0] < spec["end_time"] - tolerance:
        raise ValueError("native output does not cover configured duration")
    starts = [float(p.parent.name) for p in (case / "postProcessing" / name).glob(f"*/{filename}")]
    for a, b in zip(rows, rows[1:]):
        boundary = any(a[0] < start <= b[0] for start in starts)
        if b[0] - a[0] > tolerance * (2 if boundary else 1):
            raise ValueError("missing sampling interval in native output")
    with tempfile.TemporaryDirectory(prefix="sloshing-history-") as temporary:
        folder = Path(temporary) / "postProcessing" / name / "0"
        folder.mkdir(parents=True)
        lines = []
        for timestamp, values, line in rows:
            if filename == "moment.dat" and spec["dimension"] == 3 and spec["direction"] == "pitch":
                vectors = [values[i:i+3] for i in range(1, len(values), 3)]
                line = str(timestamp) + " " + " ".join(f"({v[2]} {v[1]} {v[0]})" for v in vectors)
            lines.append(line)
        (folder / filename).write_text("\n".join(lines) + "\n")
        if filename == "height.dat":
            return parse_interface_height(temporary, name, expected_height=spec["fill_depth"])
        return parse_roll_moment(temporary, name)


def _harmonic(times, values, spec):
    from digitalmodel.solvers.openfoam.validation.sloshing_sweep import reduce_roll_moment
    return reduce_roll_moment(times, values, spec["period"], fill_level=spec["fill_depth"] / spec["height"],
                              roll_amplitude_deg=4.0)["moment_amplitude"]


def _cycle_change(times, values, spec):
    """Last versus previous forced cycle using the shared harmonic fit."""
    end, period = spec["end_time"], spec["period"]
    amplitudes = []
    for lo, hi in ((end - 2 * period, end - period), (end - period, end)):
        pairs = [(t, v) for t, v in zip(times, values) if lo <= t <= hi]
        if lo < 0 or len(pairs) < 4:
            return None
        t, v = zip(*pairs)
        amplitudes.append(_harmonic(t, v, spec))
    if amplitudes[0] == 0:
        return 0.0 if amplitudes[1] == 0 else None
    return abs(amplitudes[1] - amplitudes[0]) / amplitudes[0] * 100


def reduce_case(case, spec):
    """Emit distinct held run-up and first-harmonic quantities, without rounding."""
    from run_sloshing_response_sweep import _runup_amplitude
    from digitalmodel.solvers.openfoam.validation.sloshing_2d import measure_natural_frequency
    names = range(1, 7) if spec["dimension"] == 3 else (1,)
    signals = [_signal(case, f"interfaceHeight{i}", "height.dat", spec) for i in names]
    times, elevation = signals[0]
    if spec["kind"] == "free-decay":
        # This helper requires uniform sampling, as in the held fixed-step case.
        spacings = [b - a for a, b in zip(times, times[1:])]
        mode = "native-uniform"
        interval = spec["delta_t"] * 10
        if max(spacings) - min(spacings) > 1e-6:
            if len(list((case / "postProcessing/interfaceHeight1").glob("*/height.dat"))) <= 1:
                raise ValueError("free-decay samples must be uniformly spaced")
            # The held FFT helper requires a uniform grid. Explicitly record
            # interpolation across the restart sampling boundary; retain raw data.
            import numpy as np
            count = math.floor((times[-1] - times[0]) / interval + 1e-8)
            uniform = times[0] + np.arange(count + 1) * interval
            elevation = np.interp(uniform, times, elevation).tolist()
            times = uniform.tolist()
            mode = "interpolated-restart"
        measured = measure_natural_frequency(times, elevation)["refined_frequency"]
        return dict(natural_period_cfd_s=1 / measured, analytical_t1_s=spec["analytical_t1_s"],
                    sampling=dict(mode=mode, interval_s=interval))
    mt, moment = _signal(case, "tankRollMoment", "moment.dat", spec)
    amplitudes = [_harmonic(t, y, spec) for t, y in signals]
    m = _harmonic(mt, moment, spec)
    wall = amplitudes[0]
    span = spec["length"] if spec["direction"] == "roll" else spec["breadth"]
    divisor = 0.01 if spec["dimension"] == 2 else span
    changes = dict(wall=_cycle_change(times, elevation, spec), moment=_cycle_change(mt, moment, spec))
    return dict(runup_amp_m=_runup_amplitude(elevation), wall_harmonic_amp_m=wall,
                probe_harmonic_amp_m=amplitudes, moment_amplitude_nm=m, moment_per_m_nm=m / divisor,
                out_of_plane_ratio=max(amplitudes[2:4]) / wall if len(amplitudes) == 6 and wall > 0 else None,
                corner_harmonic_amp_m=amplitudes[4:], last_cycle_change_pct=changes,
                steady_at_six_cycles=all(v is not None and v <= 5 for v in changes.values()) if not spec["smoke"] else None)


def _restart_fields(case, spec):
    """Require a complete, same-rank native checkpoint before a solver restart."""
    roots = [case / f"processor{i}" for i in range(spec["ranks"])] if spec["ranks"] > 1 else [case]
    latest = []
    for root in roots:
        times = [p for p in root.iterdir() if p.is_dir() and re.fullmatch(r"[0-9]+(?:\.[0-9]+)?(?:e[+-]?[0-9]+)?", p.name)]
        positive = [p for p in times if float(p.name) > 0]
        if not positive:
            raise ValueError("interrupted solve has no complete checkpoint; inputs are preserved")
        folder = max(positive, key=lambda p: float(p.name))
        required = ("U", "p_rgh", "alpha.water") + (("polyMesh/points",) if spec["kind"] == "forced" else ())
        if any(not (folder / name).is_file() for name in required):
            raise ValueError("incomplete restart field state")
        latest.append(float(folder.name))
    if len(set(latest)) != 1:
        raise ValueError("restart times differ across ranks")


def prepared_digests(case, spec):
    """Bind actual initialized fields and generated mesh bytes, on every rank."""
    roots = [case / "0", case / "constant/polyMesh"]
    for i in range(spec["ranks"] if spec["ranks"] > 1 else 0):
        roots += [case / f"processor{i}/0", case / f"processor{i}/constant/polyMesh"]
    return _file_digests(case, [p for root in roots for p in root.rglob("*")])


def completed_times(log, control):
    """Judge completion against the rendered dictionary, retaining precision."""
    end = float(re.search(r"endTime\s+([\d.e+-]+);", control)[1])
    times = [float(t) for t in re.findall(r"^Time = ([\d.e+-]+)", log, re.M)]
    if not times or times[-1] < end * (1 - 1e-6) or not re.search(r"^End\s*$", log, re.M):
        raise RuntimeError("solver did not reach configured end time")
    return times, end


def _prepare(case, spec, state_path, state):
    """Never overwrite authored inputs or repeat completed setup stages."""
    if not state["stages"]:
        actual = _file_digests(case / "0", (case / "0").glob("*"))
        authored = _file_digests(case / "_input_fields", (case / "_input_fields").glob("*"))
        if actual != authored:
            raise ValueError("authored field digest mismatch")
        if (case / "constant/polyMesh").exists():
            raise ValueError("unrecorded mesh exists; preparation cannot overwrite it")
    stages = [["blockMesh"], ["setFields"]] + ([["decomposePar"]] if spec["ranks"] > 1 else [])
    for argv in stages:
        if argv[0] not in state["stages"]:
            state["preparing"] = argv[0]
            _json_write(state_path, state)
            state["setup_wall_s"] = state.get("setup_wall_s", 0) + _launch(case, argv)
            state["stages"].append(argv[0])
            state.pop("preparing")
            _json_write(state_path, state)
    state["prepared_sha256"] = prepared_digests(case, spec)
    _json_write(state_path, state)


def _cached_result(case, state, digest, result_path):
    """Return a completed result only when its retained bytes still agree."""
    if not result_path.exists():
        return None
    result = json.loads(result_path.read_text())
    if result.get("status") not in ("completed", "smoke-completed"):
        return None
    paths = list((case / "postProcessing").rglob("*.dat")) + [case / "log.interFoam"]
    if _file_digests(case, paths) != result["output_sha256"]:
        raise ValueError("output digest mismatch")
    if hashlib.sha256(result_path.read_bytes()).hexdigest() != state.get("result_sha256"):
        raise ValueError("result digest mismatch")
    if result["input_sha256"] != digest:
        raise ValueError("input digest mismatch")
    return result


def run_case(case):
    """Resume only verified results; interrupted solves retain native fields."""
    case = Path(case)
    spec = json.loads((case / "_spec.json").read_text())
    if not 1 <= spec["ranks"] <= 4 or (spec["dimension"] == 2 and spec["ranks"] != 1):
        raise ValueError("invalid per-case ranks")
    digest = input_digest(case)
    state_path, result_path = case / "_state.json", case / "_result.json"
    state = json.loads(state_path.read_text()) if state_path.exists() else dict(input_sha256=digest, stages=[], failures=0)
    if state["input_sha256"] != digest:
        raise ValueError("input digest mismatch")
    if "prepared_sha256" in state and prepared_digests(case, spec) != state["prepared_sha256"]:
        raise ValueError("prepared state digest mismatch")
    cached = _cached_result(case, state, digest, result_path)
    if cached is not None:
        return cached
    if state["failures"] >= 2:
        raise RuntimeError("case failed twice with the same settings")
    if state.get("preparing"):
        raise ValueError("interrupted preparation requires inspection; no inputs overwritten")
    _prepare(case, spec, state_path, state)
    solver = ["mpirun", "-np", str(spec["ranks"]), "interFoam", "-parallel"] if spec["ranks"] > 1 else ["interFoam"]
    if (case / "log.interFoam").exists():
        _restart_fields(case, spec)
    offset = (case / "log.interFoam").stat().st_size if (case / "log.interFoam").exists() else 0
    started = datetime.now(timezone.utc).isoformat()
    try:
        wall = _launch(case, solver)
        state["solver_wall_s"] = state.get("solver_wall_s", 0) + wall
        with (case / "log.interFoam").open("rb") as handle:
            handle.seek(offset)
            log = handle.read().decode(errors="replace")
        times, configured_end = completed_times(log, (case / "system/controlDict").read_text())
        row = dict(spec=spec, status="smoke-completed" if spec["smoke"] else "completed",
                   input_sha256=digest, started_utc=started, solver_wall_s=wall,
                   solver_wall_s_total=state["solver_wall_s"], setup_wall_s=state.get("setup_wall_s", 0),
                   requested_end_time_s=spec["end_time"], configured_end_time_s=configured_end,
                   steps=len(times), cells=spec["nx"] * spec["ny"] * spec["nz"], ranks=spec["ranks"],
                   openfoam_version=os.environ.get("WM_PROJECT_VERSION", "not established"), **reduce_case(case, spec))
        row["output_sha256"] = _file_digests(case, list((case / "postProcessing").rglob("*.dat")) + [case / "log.interFoam"])
        _json_write(result_path, row)
        state["result_sha256"] = hashlib.sha256(result_path.read_bytes()).hexdigest()
        _json_write(state_path, state)
        return row
    except Exception:
        state["failures"] += 1
        _json_write(state_path, state)
        _json_write(result_path, dict(status="failed", input_sha256=digest))
        raise


def main(argv=None):
    """Generate or execute a selected case set with explicit rank limits."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=("generate", "run", "run-one"))
    parser.add_argument("--work-dir", type=Path, required=True)
    parser.add_argument("--set", choices=SETS, default="medium-3d")
    parser.add_argument("--direction", choices=("roll", "pitch", "both"), default="both")
    parser.add_argument("--kind", choices=("free-decay", "forced", "all"), default="all")
    parser.add_argument("--ratio", type=float)
    parser.add_argument("--case-name")
    parser.add_argument("--ranks", type=int, default=4)
    parser.add_argument("--t1-roll", type=float)
    parser.add_argument("--t1-pitch", type=float)
    parser.add_argument("--smoke", action="store_true", help="short separate cases; never production results")
    args = parser.parse_args(argv)
    if args.command == "run-one":
        if not args.case_name or args.case_name in {".", ".."} or Path(args.case_name).name != args.case_name:
            parser.error("run-one requires a case basename")
        row = run_case(args.work_dir / args.case_name)
        print(json.dumps(row, sort_keys=True, allow_nan=False))
        return 0
    specs = case_specs(t1_roll=args.t1_roll, t1_pitch=args.t1_pitch, ranks=args.ranks, smoke=args.smoke)
    selected = [s for s in specs if s["group"] == args.set
                and (args.direction == "both" or s["direction"] == args.direction)
                and (args.kind == "all" or s["kind"] == args.kind)
                and (args.ratio is None or s["ratio"] == args.ratio)]
    if not selected:
        parser.error("selection contains no cases")
    for spec in selected:
        case = build_case(args.work_dir, spec)
        if args.command == "run":
            print(json.dumps(run_case(case), sort_keys=True, allow_nan=False), flush=True)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
