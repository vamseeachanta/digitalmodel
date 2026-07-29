"""Regenerate committed ``docs/api/**`` HTML in isolation and detect drift.

Run from the repository root:

    .venv/bin/python scripts/check_generated_html.py --check

Generators currently write to fixed repository paths, so this check builds a
temporary shadow repository. It never rewrites the developer's working tree.
Every discovered generator must be registered or carry an explicit reason for
being a discovery false-positive; generated pages themselves are never skipped.
"""

from __future__ import annotations

import argparse
import difflib
import os
import shutil
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from pathlib import Path


REPO = Path(__file__).resolve().parents[1]


@dataclass(frozen=True)
class Generator:
    script: str
    outputs: tuple[str, ...] = ()
    output_glob: str | None = None
    onepagers: bool = False
    redirect_module_outputs: bool = False


GENERATORS = (
    Generator(
        "scripts/capabilities/build_anchor_holding_explorer.py",
        ("docs/api/structural/anchor-holding-explorer.html",),
    ),
    Generator(
        "scripts/capabilities/build_casing_design_explorer.py",
        ("docs/api/well/casing-design-explorer.html",),
    ),
    Generator(
        "scripts/capabilities/build_cathodic_protection_explorer.py",
        ("docs/api/structural/cathodic-protection-explorer.html",),
    ),
    Generator(
        "scripts/capabilities/build_cfd_runtime_estimator.py",
        ("docs/api/cfd/cfd-runtime-estimator.html",),
    ),
    Generator(
        "scripts/capabilities/build_dynacard_troubleshooting.py",
        ("docs/api/artificial-lift/dynacard-troubleshooting.html",),
    ),
    Generator(
        "scripts/capabilities/build_field_economics_explorer.py",
        ("docs/api/structural/field-economics-explorer.html",),
    ),
    Generator(
        "scripts/capabilities/build_ipr_explorer.py",
        ("docs/api/structural/ipr-explorer.html",),
    ),
    Generator(
        "scripts/capabilities/build_onepagers.py",
        output_glob="docs/api/capabilities/api/*.html",
        onepagers=True,
    ),
    Generator(
        "scripts/capabilities/build_pore_pressure_explorer.py",
        ("docs/api/structural/pore-pressure-explorer.html",),
    ),
    Generator(
        "scripts/capabilities/build_ship_resistance_explorer.py",
        ("docs/api/structural/ship-resistance-explorer.html",),
    ),
    Generator(
        "scripts/capabilities/build_sloshing_cfd_showcase.py",
        ("docs/api/cfd/sloshing-cfd-study.html",),
    ),
    Generator(
        "scripts/capabilities/build_sloshing_explorer.py",
        ("docs/api/structural/sloshing-explorer.html",),
    ),
    Generator(
        "scripts/capabilities/build_viv_explorer.py",
        ("docs/api/structural/viv-explorer.html",),
    ),
    Generator(
        "scripts/capabilities/build_wall_thickness_explorer.py",
        ("docs/api/structural/wall-thickness-explorer.html",),
    ),
    Generator(
        "scripts/corrosion/build_galvanic_explorer.py",
        ("docs/api/corrosion/galvanic-compatibility-explorer.html",),
    ),
    Generator(
        "scripts/drilling_riser/build_operability_explorer.py",
        ("docs/api/drilling/drilling-riser-operability-explorer.html",),
        redirect_module_outputs=True,
    ),
    Generator(
        "scripts/drilling_riser/build_operability_monitor.py",
        ("docs/api/drilling/operability-monitor.html",),
        redirect_module_outputs=True,
    ),
    Generator(
        "scripts/ffs/build_riser_joint_explorer.py",
        ("docs/api/ffs/riser-joint-acceptance-explorer.html",),
    ),
    Generator(
        "scripts/production_chemistry/build_scale_si_explorer.py",
        ("docs/api/production/scale-si-explorer.html",),
    ),
)

# The discovery heuristic intentionally errs toward false positives. Each
# non-generator needs a reason so a new real generator cannot disappear into a
# silent skip.
DISCOVERY_FALSE_POSITIVES = {
    "scripts/benchmark/validate_owd_vs_spec.py": (
        "writes validation reports outside docs/api; docs/api HTML is input only"
    ),
    "scripts/capabilities/build_capabilities_inventory.py": (
        "writes JSON and Markdown; it only discovers existing docs/api HTML"
    ),
    "scripts/check_generated_html.py": (
        "executes registered generators; it does not emit a docs/api HTML page"
    ),
}

# Generated-looking committed pages without an active, repository-owned
# ``scripts/** -> docs/api/**`` route. Keeping them explicit prevents this check
# from overstating its coverage. Each should be removed from this map when its
# producer becomes deterministic and targets the committed page.
PAGE_EXCLUSIONS = {
    "docs/api/hydro/ocimf-coefficient-explorer.html": (
        "producer uses an external absolute-path workbook and random Plotly div ids"
    ),
    "docs/api/hydro/unit-box-benchmark/benchmark_amplitude.html": (
        "no active producer targets this committed copy; Plotly div id is random"
    ),
    "docs/api/hydro/unit-box-benchmark/benchmark_combined.html": (
        "no active producer targets this committed copy; Plotly div id is random"
    ),
    "docs/api/hydro/unit-box-benchmark/benchmark_heatmap.html": (
        "no active producer targets this committed copy; Plotly div id is random"
    ),
    "docs/api/hydro/unit-box-benchmark/benchmark_phase.html": (
        "no active producer targets this committed copy; Plotly div id is random"
    ),
    "docs/api/hydro/unit-box-benchmark/benchmark_report.html": (
        "no active producer targets this committed copy; output embeds a timestamp"
    ),
    "docs/api/hydro/rao-comparison/index.html": (
        "producer targets another docs path and embeds datetime.now()"
    ),
    "docs/api/hydro/passing-ship-benchmark.html": (
        "producer targets another docs path and embeds datetime.now()"
    ),
    "docs/api/orcaflex/riser-mesh-sensitivity.html": (
        "licensed OrcFxAPI benchmark output embeds a timestamp"
    ),
    "docs/api/orcaflex/riser-validation-report.html": (
        "licensed OrcFxAPI benchmark output embeds a timestamp"
    ),
}

ONEPAGER_DRIVER = r"""
import importlib.util
import sys

script = sys.argv[1]
spec = importlib.util.spec_from_file_location("generated_html_onepagers", script)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
module._CHROME = "drift-check-no-browser"
module._to_pdf = lambda *_args, **_kwargs: None
sys.argv = [script]
module.main()
"""

REDIRECT_DRIVER = r"""
import importlib.util
import sys
from pathlib import Path

script, shadow = sys.argv[1], Path(sys.argv[2])
spec = importlib.util.spec_from_file_location("generated_html_redirected", script)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
module.REPO_ROOT = shadow
if hasattr(module, "oa"):
    module.oa.REPO_ROOT = shadow
out_dir = shadow / "docs" / "api" / "drilling"
module._OUT_DIR = out_dir
module._HTML = out_dir / module._HTML.name
module._JSON = out_dir / module._JSON.name
module.main()
"""


def discover_candidate_generators(repo: Path) -> set[str]:
    """Conservative source scan used to fail closed on unregistered builders."""
    found = set()
    for path in (repo / "scripts").rglob("*.py"):
        text = path.read_text(encoding="utf-8", errors="ignore")
        markers = ("docs", "api", ".html", "write_text")
        if all(marker in text for marker in markers):
            found.add(path.relative_to(repo).as_posix())
    return found


def validate_registry(repo: Path) -> list[str]:
    registered = {entry.script for entry in GENERATORS}
    candidates = discover_candidate_generators(repo)
    explained = set(DISCOVERY_FALSE_POSITIVES)
    errors = []
    if missing := sorted(candidates - registered - explained):
        errors.append(f"unregistered candidate generator(s): {', '.join(missing)}")
    if stale := sorted((registered | explained) - candidates):
        errors.append(f"stale generator registry entry/entries: {', '.join(stale)}")
    for path, reason in DISCOVERY_FALSE_POSITIVES.items():
        if not reason.strip():
            errors.append(f"discovery false-positive lacks a reason: {path}")
    for path, reason in PAGE_EXCLUSIONS.items():
        if not (repo / path).is_file():
            errors.append(f"stale generated-page exclusion: {path}")
        if not reason.strip():
            errors.append(f"generated-page exclusion lacks a reason: {path}")
    return errors


def prepare_shadow(repo: Path, shadow: Path) -> None:
    shutil.copytree(repo / "scripts", shadow / "scripts")
    shutil.copytree(repo / "docs" / "api", shadow / "docs" / "api")
    for name in ("assets", "atlases", "data", "src", "tests"):
        source = repo / name
        if source.exists():
            (shadow / name).symlink_to(source, target_is_directory=True)


def output_paths(root: Path, entry: Generator) -> set[Path]:
    if entry.output_glob:
        return {path for path in root.glob(entry.output_glob)}
    return {root / path for path in entry.outputs}


def clear_generated_html(shadow: Path, entry: Generator) -> None:
    for path in output_paths(shadow, entry):
        path.unlink()


def run_generator(shadow: Path, entry: Generator) -> str | None:
    script = shadow / entry.script
    command = [sys.executable, str(script)]
    if entry.onepagers:
        command = [sys.executable, "-c", ONEPAGER_DRIVER, str(script)]
    elif entry.redirect_module_outputs:
        command = [
            sys.executable,
            "-c",
            REDIRECT_DRIVER,
            str(script),
            str(shadow),
        ]
    env = os.environ.copy()
    env["PYTHONPATH"] = str(shadow / "src")
    result = subprocess.run(
        command,
        cwd=shadow,
        env=env,
        capture_output=True,
        text=True,
        timeout=180,
    )
    if result.returncode:
        detail = result.stderr.strip() or result.stdout.strip()
        return f"{entry.script} failed ({result.returncode}): {detail}"
    produced = output_paths(shadow, entry)
    if not produced or any(not path.is_file() for path in produced):
        return f"{entry.script} produced no registered HTML output"
    return None


def regenerate(shadow: Path) -> list[str]:
    for entry in GENERATORS:
        clear_generated_html(shadow, entry)
    with ThreadPoolExecutor(max_workers=4) as pool:
        results = pool.map(lambda entry: run_generator(shadow, entry), GENERATORS)
        return [error for error in results if error]


def all_registered_outputs(root: Path) -> set[Path]:
    paths = set()
    for entry in GENERATORS:
        paths.update(output_paths(root, entry))
    return paths


def short_diff(committed: Path, generated: Path) -> list[str]:
    old = committed.read_text(encoding="utf-8").splitlines()
    new = generated.read_text(encoding="utf-8").splitlines()
    diff = difflib.unified_diff(
        old,
        new,
        fromfile=f"committed/{committed.name}",
        tofile=f"generated/{generated.name}",
        lineterm="",
    )
    lines = list(diff)[:24]
    return [line if len(line) <= 280 else f"{line[:277]}..." for line in lines]


def compare_outputs(repo: Path, shadow: Path) -> list[tuple[str, list[str]]]:
    committed = {path.relative_to(repo) for path in all_registered_outputs(repo)}
    fresh = {path.relative_to(shadow) for path in all_registered_outputs(shadow)}
    drift = []
    for relative in sorted(committed | fresh):
        old, new = repo / relative, shadow / relative
        if not old.exists():
            drift.append((relative.as_posix(), ["generated file is not committed"]))
        elif not new.exists():
            drift.append((relative.as_posix(), ["committed file is no longer generated"]))
        elif old.read_bytes() != new.read_bytes():
            drift.append((relative.as_posix(), short_diff(old, new)))
    return drift


def check(repo: Path = REPO) -> int:
    if errors := validate_registry(repo):
        print("GENERATED HTML CHECK FAILED — registry coverage error")
        for error in errors:
            print(f"  x {error}")
        return 1
    with tempfile.TemporaryDirectory(prefix="digitalmodel-generated-html-") as tmp:
        shadow = Path(tmp)
        prepare_shadow(repo, shadow)
        if errors := regenerate(shadow):
            print("GENERATED HTML CHECK FAILED — regeneration error")
            for error in errors:
                print(f"  x {error}")
            return 1
        drift = compare_outputs(repo, shadow)
    if drift:
        print("GENERATED HTML CHECK FAILED — committed pages differ from generators")
        for path, diff in drift:
            print(f"\n  x {path}")
            for line in diff:
                print(f"    {line}")
        print(f"\n{len(drift)} generated HTML page(s) drifted.")
        return 1
    pages = len(all_registered_outputs(repo))
    print(
        f"generated HTML check OK — {pages} page(s) from "
        f"{len(GENERATORS)} generator(s); "
        f"{len(PAGE_EXCLUSIONS)} explicit exclusion(s)"
    )
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--check",
        action="store_true",
        help="regenerate in a temporary shadow repo and fail on any diff",
    )
    args = parser.parse_args()
    if not args.check:
        parser.error("--check is required; this command never rewrites tracked pages")
    return check()


if __name__ == "__main__":
    raise SystemExit(main())
