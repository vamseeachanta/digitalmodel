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

if __package__:
    from .generated_html_ownership import (
        DISCOVERY_FALSE_POSITIVES,
        EXCLUDED_GENERATORS,
        MANUAL_PAGES,
        PAGE_EXCLUSIONS,
    )
else:
    from generated_html_ownership import (
        DISCOVERY_FALSE_POSITIVES,
        EXCLUDED_GENERATORS,
        MANUAL_PAGES,
        PAGE_EXCLUSIONS,
    )


REPO = Path(__file__).resolve().parents[1]


@dataclass(frozen=True)
class Generator:
    script: str
    outputs: tuple[str, ...] = ()
    arguments: tuple[str, ...] = ()
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
        "docs/api/cfd/report_build/build_maccamy_fuchs.py",
        ("docs/api/cfd/maccamy-fuchs-cylinder-verification.html",),
        ("docs/api/cfd/cases/maccamy_fuchs/mf_results.json", "."),
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
    """Defense-in-depth scan for likely unregistered builders."""
    found = set()
    for root in (repo / "scripts", repo / "docs" / "api"):
        for path in root.rglob("*.py"):
            text = path.read_text(encoding="utf-8", errors="ignore").casefold()
            # Avoid write API names: producers use helpers and exporters too.
            if all(marker in text for marker in ("docs", "api", "html")):
                found.add(path.relative_to(repo).as_posix())
    return found


def docs_api_pages(repo: Path) -> set[str]:
    pages = set()
    for path in (repo / "docs" / "api").rglob("*"):
        if path.is_file() and path.suffix.casefold() == ".html":
            pages.add(path.relative_to(repo).as_posix())
    return pages


def validate_page_census(repo: Path) -> list[str]:
    owned = {
        path.relative_to(repo).as_posix()
        for path in all_registered_outputs(repo)
    }
    pages = docs_api_pages(repo)
    classified = owned | set(PAGE_EXCLUSIONS) | set(MANUAL_PAGES)
    errors = []
    if unclassified := sorted(pages - classified):
        errors.append(f"unclassified docs/api HTML page(s): {', '.join(unclassified)}")
    if stale := sorted(set(MANUAL_PAGES) - pages):
        errors.append(f"stale manual-page classification(s): {', '.join(stale)}")
    overlaps = (
        (owned & set(PAGE_EXCLUSIONS))
        | (owned & set(MANUAL_PAGES))
        | (set(PAGE_EXCLUSIONS) & set(MANUAL_PAGES))
    )
    if overlaps:
        errors.append(
            f"page has conflicting classifications: {', '.join(sorted(overlaps))}"
        )
    for path, reason in MANUAL_PAGES.items():
        if not reason.strip():
            errors.append(f"manual-page classification lacks a reason: {path}")
    return errors


def validate_registry(repo: Path) -> list[str]:
    registered = {entry.script for entry in GENERATORS}
    candidates = discover_candidate_generators(repo)
    explained = set(DISCOVERY_FALSE_POSITIVES) | set(EXCLUDED_GENERATORS)
    errors = []
    if missing := sorted(candidates - registered - explained):
        errors.append(f"unregistered candidate generator(s): {', '.join(missing)}")
    if stale := sorted((registered | explained) - candidates):
        errors.append(f"stale generator registry entry/entries: {', '.join(stale)}")
    for path, reason in DISCOVERY_FALSE_POSITIVES.items():
        if not reason.strip():
            errors.append(f"discovery false-positive lacks a reason: {path}")
    for path, reason in EXCLUDED_GENERATORS.items():
        if not reason.strip():
            errors.append(f"excluded generator lacks a reason: {path}")
    for path, reason in PAGE_EXCLUSIONS.items():
        if not (repo / path).is_file():
            errors.append(f"stale generated-page exclusion: {path}")
        if not reason.strip():
            errors.append(f"generated-page exclusion lacks a reason: {path}")
    errors.extend(validate_page_census(repo))
    return errors


def prepare_shadow(repo: Path, shadow: Path) -> None:
    shutil.copytree(repo / "scripts", shadow / "scripts")
    shutil.copytree(repo / "docs" / "api", shadow / "docs" / "api")
    for name in ("assets", "atlases", "data", "src"):
        source = repo / name
        if source.exists():
            shutil.copytree(source, shadow / name)
    for relative in (
        "tests/asset_integrity/test_data/real_inspection",
        "tests/drilling_riser/fixtures",
    ):
        source = repo / relative
        if source.exists():
            shutil.copytree(source, shadow / relative)


def output_paths(root: Path, entry: Generator) -> set[Path]:
    if entry.output_glob:
        return {path for path in root.glob(entry.output_glob)}
    return {root / path for path in entry.outputs}


def clear_generated_html(shadow: Path, entry: Generator) -> None:
    for path in output_paths(shadow, entry):
        path.unlink()


def run_generator(shadow: Path, entry: Generator) -> str | None:
    script = shadow / entry.script
    command = [
        sys.executable,
        str(script),
        *(str(shadow / argument) for argument in entry.arguments),
    ]
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
    env["PYTHONDONTWRITEBYTECODE"] = "1"
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
        f"{len(EXCLUDED_GENERATORS)} excluded generator(s); "
        f"{len(PAGE_EXCLUSIONS)} explicit exclusion(s); "
        f"{len(MANUAL_PAGES)} manual page classification(s)"
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
