"""Research-only stress extrapolation; see the adjacent surface-crack spike note.

No standard-derived acceptance calculation. All distances are mm, stress MPa,
and K MPa*sqrt(m). Solver files are retained only in the supplied scratch area.
"""
import argparse
import hashlib
import importlib.util
import sys
from functools import lru_cache
import json
import math
import os
import resource
import re
import signal
import shutil
import subprocess
from pathlib import Path


# Benchmark dimensions and stress are chosen for this public, synthetic study.
# The depth, half-width and stress have the same value as inputs in
# test_crack_fad.py; thickness and semicircular shape are study choices.
# No client geometry or values from a standard are used.
BENCHMARK = dict(a_mm=5.0, c_mm=5.0, t_mm=2*5.0,
                 sigma_m_mpa=100.0, width_mm=25.0)
# Material constants are chosen for this public, synthetic study; each has the
# same value as the STEEL fixture in test_fem_chain.py, not a derived value.
# No client material data or values from a standard are used.
E_MPA, NU = 210000.0, 0.3
HALF_LENGTH_MM = BENCHMARK["width_mm"]
# Refinement levels are chosen for this public, synthetic study, with successive
# doubling; the initial level has the same value as the DOF count in
# test_fem_chain.py. No client geometry or values from a standard are used.
LEVELS = (6, 2*6, 2*2*6)
# Existing plate-validation test's relative tolerance; a research comparator.
COMPARISON_TOLERANCE = 0.05
# Roylance, p. 7, Eq. (4) discussion: leading field for r <= 0.1*a.
NEAR_FIELD_FRACTION = 0.1


@lru_cache(maxsize=None)
def repo_module(relative_path):
    # Load the unchanged file itself without eager unrelated package imports.
    root = Path(__file__).resolve().parents[2]
    name = "_spike_" + Path(relative_path).stem
    spec = importlib.util.spec_from_file_location(name, root/relative_path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


def reference_k(phi_deg):
    module = repo_module("src/digitalmodel/asset_integrity/assessment/crack_fad.py")
    return module.newman_raju_k(**BENCHMARK, phi_deg=phi_deg)


def extrapolate_stress_k(radii_mm, sigma_yy_mpa):
    """Fit sigma_yy*sqrt(2*pi*r) = K0 + slope*r on theta=0 rays.

    Roylance (MIT OCW), Introduction to Fracture Mechanics, p. 7, Eq. (4).
    The linear-in-r fit is a numerical spike choice, checked through sample
    spread; the cited textbook establishes the leading field only. No fitted
    FE value is used as input.
    """
    r, s = list(radii_mm), list(sigma_yy_mpa)
    if len(r) < 2 or len(r) != len(s):
        raise ValueError("paired samples at distinct positive distances required")
    if any(not math.isfinite(v) for v in r+s) or min(r) <= 0:
        raise ValueError("finite stress and positive finite radius required")
    xbar = sum(r)/len(r)
    apparent = [v*math.sqrt(2*math.pi*x/1000.0) for x, v in zip(r, s)]
    ybar = sum(apparent)/len(apparent)
    denom = sum((x-xbar)**2 for x in r)
    if denom == 0:
        raise ValueError("distinct radii required")
    slope = sum((x-xbar)*(y-ybar) for x, y in zip(r, apparent))/denom
    return dict(k_mpa_sqrt_m=ybar-slope*xbar, apparent_k=apparent,
                radii_mm=r, sigma_yy_mpa=s,
                sampling_spread_mpa_sqrt_m=max(apparent)-min(apparent))


def _resource_limit():
    # Owner resource ceiling; one sequential process, not an engineering input.
    cap = 16*1024**3
    resource.setrlimit(resource.RLIMIT_AS, (cap, cap))


def solve(work_dir, binary):
    """Run one bounded solver, retaining logs and time-tool peak RSS."""
    env = dict(os.environ, OMP_NUM_THREADS="4", OPENBLAS_NUM_THREADS="4",
               MKL_NUM_THREADS="4", CCX_NPROC_RESULTS="4")
    timer = shutil.which("time")
    command = [str(binary), "-i", "crack"]
    if timer:
        command = [timer, "-f", "%e %M", "-o", "resources.txt"] + command
    import time
    start = time.monotonic()
    with (Path(work_dir)/"solver.log").open("w") as output:
        try:
            proc = subprocess.Popen(command, cwd=work_dir, env=env,
                                    stdout=output, stderr=subprocess.STDOUT,
                                    start_new_session=True, preexec_fn=_resource_limit)
            rc = proc.wait(timeout=15*60)
        except subprocess.TimeoutExpired:
            os.killpg(proc.pid, signal.SIGKILL)
            proc.wait()
            rc = "timeout"
    seconds, rss = time.monotonic()-start, None
    resource_file = Path(work_dir)/"resources.txt"
    if timer and resource_file.exists():
        values = resource_file.read_text().splitlines()[-1].split()
        if len(values) == 2:
            seconds, rss = float(values[0]), int(values[1])
    return dict(returncode=rc, wall_seconds=seconds, peak_rss_kib=rss)


def read_fields(work_dir):
    module = repo_module("src/digitalmodel/solvers/calculix/result_parser.py")
    return module.CalculiXResultParser("crack", Path(work_dir)).parse_frd_file()


def reaction_y(work_dir):
    text = (Path(work_dir)/"crack.dat").read_text()
    values = re.findall(r"total force[^\n]*\n\s*([-+0-9.Ee]+)\s+([-+0-9.Ee]+)\s+([-+0-9.Ee]+)", text)
    if not values:
        raise ValueError("native reaction total absent")
    return float(values[-1][1])


def front_results(model, fields):
    rows = []
    for sample in model["samples"]:
        stress = [fields["stresses"][node]["syy"] for node in sample["nodes"]]
        result = extrapolate_stress_k(sample["radii_mm"], stress)
        reference = reference_k(sample["phi_deg"])
        rows.append(dict(phi_deg=sample["phi_deg"], reference_k=reference,
                         relative_difference=(result["k_mpa_sqrt_m"]-reference)/reference,
                         all_samples_near_field=max(sample["radii_mm"]) <=
                         NEAR_FIELD_FRACTION*BENCHMARK["a_mm"], **result))
    return rows


def retained_manifest(scratch):
    """Relative locators, sizes and digests only; no machine identity."""
    rows = []
    for directory in [scratch/"sources"] + [scratch/f"mesh_{n}" for n in LEVELS]:
        for path in sorted(directory.glob("*")):
            if path.is_file():
                rows.append(dict(path=path.relative_to(scratch).as_posix(),
                                 bytes=path.stat().st_size,
                                 sha256=hashlib.sha256(path.read_bytes()).hexdigest()))
    return rows


def compile_results(scratch):
    from surface_crack_mesh import build_mesh
    runs = []
    for n in LEVELS:
        directory = scratch/f"mesh_{n}"
        model = build_mesh(n)
        receipt = json.loads((directory/"receipt.json").read_text())
        row = dict(refinement=n, element_types="C3D8+C3D6",
                   elements=len(model["hexes"])+len(model["wedges"]),
                   nodes=len(model["nodes"]), front_segments=len(model["angles"])-1,
                   **receipt)
        if receipt["returncode"] == 0:
            row["reaction_y_n"] = reaction_y(directory)
            row["applied_force_n"] = BENCHMARK["sigma_m_mpa"]*BENCHMARK["width_mm"]*BENCHMARK["t_mm"]
            row["equilibrium_relative_residual"] = abs(row["reaction_y_n"]+row["applied_force_n"])/row["applied_force_n"]
            row["front"] = front_results(model, read_fields(directory))
            row["deepest_difference"] = row["front"][-1]["relative_difference"]
            row["surface_difference"] = row["front"][0]["relative_difference"]
            row["max_abs_front_difference"] = max(
                abs(p["relative_difference"]) for p in row["front"])
        runs.append(row)
    return dict(benchmark=BENCHMARK, material=dict(e_mpa=E_MPA, nu=NU),
                half_length_mm=HALF_LENGTH_MM, solver="CalculiX 2.21",
                solver_version_source="as installed on the build machine; not read from the run",
                comparison_tolerance=COMPARISON_TOLERANCE,
                qualification="research only; two-solver verdict remains open",
                runs=runs, source_revision="f21a12af5349c1656137a7b9daa091ec8f968738",
                source_files=source_files(), retained_files=retained_manifest(scratch))


def source_files():
    root = Path(__file__).resolve().parents[2]
    paths = ["src/digitalmodel/asset_integrity/assessment/crack_fad.py",
             "tests/asset_integrity/test_crack_fad.py",
             "tests/solvers/calculix/test_fem_chain.py", "LICENSE"]
    return [dict(path=p, sha256=hashlib.sha256((root/p).read_bytes()).hexdigest(),
                 terms="MIT License", retrieved_utc="2026-10-11") for p in paths]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--run", action="store_true")
    args = parser.parse_args()
    from surface_crack_mesh import build_mesh, write_deck
    if args.run:
        binary = shutil.which("ccx")
        if binary is None:
            raise SystemExit("CalculiX binary absent")
        for n in LEVELS:
            directory = args.scratch/f"mesh_{n}"
            directory.mkdir(parents=True, exist_ok=True)
            if (directory/"receipt.json").exists():
                raise SystemExit("existing run retained; choose a fresh scratch area")
            write_deck(directory/"crack.inp", build_mesh(n))
            receipt = solve(directory, binary)
            (directory/"receipt.json").write_text(json.dumps(receipt, indent=2)+"\n")
            print(f"refinement {n}: {receipt}", flush=True)
    args.output.write_text(json.dumps(compile_results(args.scratch),
                                     sort_keys=True, allow_nan=False,
                                     separators=(",", ":"))+"\n")


if __name__ == "__main__":
    main()
