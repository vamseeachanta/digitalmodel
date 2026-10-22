# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Regenerate committed ``docs/api/**`` HTML in isolation and detect drift.

Run from the repository root, inside the project environment:
    uv run python scripts/check_generated_html.py --check

The checker itself is stdlib-only, but the generators it drives are not: they
reach numpy, scipy, pandas, matplotlib, pydantic and yaml through
src/digitalmodel. A bare interpreter that happens to have those installed
system-wide makes that indistinguishable from having no dependencies at all --
run it in the project environment so a missing package fails here rather than
in CI.
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
        build_output_pages,
        BUILD_OUTPUT_GENERATORS,
        DISCOVERY_FALSE_POSITIVES,
        EXCLUDED_GENERATORS,
        MANUAL_PAGES,
        PAGE_EXCLUSIONS,
    )
else:
    from generated_html_ownership import (
        build_output_pages,
        BUILD_OUTPUT_GENERATORS,
        DISCOVERY_FALSE_POSITIVES,
        EXCLUDED_GENERATORS,
        MANUAL_PAGES,
        PAGE_EXCLUSIONS,
    )


REPO = Path(__file__).resolve().parents[1]


@dataclass(frozen=True)
class Generator:
    """Describe deterministic committed outputs and their required inputs."""
    script: str
    outputs: tuple[str, ...] = ()
    arguments: tuple[str, ...] = ()
    output_glob: str | None = None
    redirect_module_outputs: bool = False
    # Scripts producing a gitignored intermediate this generator reads. They run
    # first, in order, inside the shadow tree. A page whose input is not
    # committed is still reproducible as long as that input is itself generated
    # deterministically from the repository.
    prerequisites: tuple[str, ...] = ()
    inputs: tuple[str, ...] = ()


GENERATORS = (
    Generator(
        "scripts/reporting/generate_reservoir_report.py",
        ("docs/reports/modules/reservoir/index.html",
         "docs/reports/modules/reservoir/changes.json"),
        inputs=(
            "pyproject.toml",
            "docs/collide_pe/briefs/pe-2026-05-21.html",
            "docs/reports/modules/reservoir/validation.json",
            "docs/reports/pipeline-ffs-assessment.html",
            "src/digitalmodel/reservoir/__init__.py",
            "src/digitalmodel/reservoir/volumetrics.py",
            "src/digitalmodel/reservoir/stratigraphic.py",
            "tests/reservoir/test_reservoir.py",
            "tests/reservoir/test_stratigraphic.py",
            "tests/reservoir/test_volumetrics.py",
            "tests/test_reservoir_framework.py",
        ),
    ),
    # The synthetic assessment and its digest manifest form one reproducible
    # deliverable. Explicit inputs keep the shadow build independent of cwd.
    Generator(
        "scripts/reporting/build_pipeline_ffs_report.py",
        ("docs/api/ffs/pipeline-ffs-assessment.html",
         "docs/api/ffs/pipeline-ffs-assessment.manifest.json",
         "docs/reports/pipeline-ffs-assessment.html",
         "docs/reports/pipeline-ffs-assessment.manifest.json"),
        inputs=(
            "pyproject.toml",
            "examples/workflows/pipeline-ffs-assessment/input.yml",
            "examples/workflows/pipeline-ffs-assessment/assumptions.yml",
            "examples/workflows/pipeline-corroded-defect-screen/synthetic.yml",
            "examples/workflows/ffs-metal-loss/input.yml",
        ),
    ),
    Generator(
        "scripts/capabilities/build_installation_heading_explorer.py",
        ("docs/api/installation/installation-heading-explorer.html",),
        inputs=(
            "scripts/capabilities/installation_heading/limits.json",
            "scripts/capabilities/installation_heading/revisions.json",
            "scripts/capabilities/installation_heading/page.html.template",
        ),
    ),
    Generator(
        "scripts/reporting/build_riser_pipe_comparison.py",
        ("docs/reports/2026-10-09-compute-program/riser-pipe-comparison/riser-pipe-comparison.html",),
        inputs=("scripts/reporting/riser_pipe_comparison.yml",
                "scripts/reporting/riser_pipe_comparison.criteria.json",
                "scripts/reporting/riser_pipe_comparison.revisions.json"),
    ),
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
        "scripts/capabilities/build_wall_thickness_3d_page.py",
        ("docs/api/structural/wall-thickness-3d-explorer.html",),
        prerequisites=("scripts/capabilities/build_wall_thickness_3d.py",),
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
    Generator(
        "scripts/studies/surface_crack_report.py",
        ("docs/domains/asset-integrity/surface-crack-calculix-spike-2026-10-11.html",),
        ("--results", "docs/domains/asset-integrity/surface-crack-calculix-results.json",
         "--output", "docs/domains/asset-integrity/surface-crack-calculix-spike-2026-10-11.html"),
        inputs=("docs/domains/asset-integrity/surface-crack-calculix-results.json",),
    ),
)

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
    """Enumerate HTML pages currently present in the documentation tree."""
    pages = set()
    for path in (repo / "docs" / "api").rglob("*"):
        if path.is_file() and path.suffix.casefold() == ".html":
            pages.add(path.relative_to(repo).as_posix())
    return pages


def validate_page_census(repo: Path) -> list[str]:
    """Reject pages without ownership or with conflicting classifications."""
    owned = {
        path.relative_to(repo).as_posix()
        for path in all_registered_outputs(repo)
    }
    pages = docs_api_pages(repo)
    errors = []
    enumeration_succeeded = True
    try:
        build_output = build_output_pages(repo)
    except (ValueError, SyntaxError) as error:
        errors.append(
            f"{error.filename}:{error.lineno}: {error.msg}"
            if isinstance(error, SyntaxError) else str(error)
        )
        enumeration_succeeded = False
        build_output = set()
    classified = owned | set(PAGE_EXCLUSIONS) | set(MANUAL_PAGES) | build_output
    if enumeration_succeeded and (unclassified := sorted(pages - classified)):
        errors.append(f"unclassified docs/api HTML page(s): {', '.join(unclassified)}")
    if stale := sorted(set(MANUAL_PAGES) - pages):
        errors.append(f"stale manual-page classification(s): {', '.join(stale)}")
    overlaps = (
        (owned & set(PAGE_EXCLUSIONS))
        | (owned & set(MANUAL_PAGES))
        | (set(PAGE_EXCLUSIONS) & set(MANUAL_PAGES))
        | (build_output & (owned | set(PAGE_EXCLUSIONS) | set(MANUAL_PAGES)))
    )
    if overlaps:
        errors.append(
            f"page has conflicting classifications: {', '.join(sorted(overlaps))}"
        )
    for path, reason in MANUAL_PAGES.items():
        if not reason.strip():
            errors.append(f"manual-page classification lacks a reason: {path}")
    return errors


def validate_build_output_generators(repo: Path) -> list[str]:
    """Validate build-output scripts, required fields and enumeration patterns."""
    errors = []
    placeholder = "<specification-id>"
    for script, entry in BUILD_OUTPUT_GENERATORS.items():
        path = Path(script)
        if (path.is_absolute() or ".." in path.parts
                or path.suffix != ".py" or not (repo / path).is_file()):
            errors.append(f"build-output generator missing script: {script}")
        for field in ("command", "output_pattern", "reason"):
            value = entry.get(field)
            if not isinstance(value, str) or not value.strip():
                errors.append(
                    f"build-output generator {field.replace('_', ' ')} is empty: {script}"
                )
        pattern = entry.get("output_pattern", "")
        if not isinstance(pattern, str) or not pattern.strip():
            continue
        remainder = pattern.replace(placeholder, "")
        if (not pattern.startswith("docs/api/") or not pattern.endswith(".html")
                or ".." in Path(pattern).parts or "\\" in pattern
                or pattern.count(placeholder) != 1
                or any(character in remainder for character in "<>")):
            errors.append(
                f"build-output generator output pattern must lie under docs/api/, "
                f"end in .html and contain exactly {placeholder}: {script}"
            )
    return errors


def validate_registry(repo: Path) -> list[str]:
    """Validate generator discovery, reasons, and page ownership."""
    registered = {entry.script for entry in GENERATORS}
    candidates = discover_candidate_generators(repo)
    explained = set(DISCOVERY_FALSE_POSITIVES) | set(EXCLUDED_GENERATORS)
    build_output = set(BUILD_OUTPUT_GENERATORS)
    errors = []
    if conflicts := sorted(build_output & (registered | explained)):
        errors.append(f"generator has conflicting classifications: {', '.join(conflicts)}")
    explained |= build_output
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
    build_errors = validate_build_output_generators(repo)
    errors.extend(build_errors)
    if not build_errors:
        errors.extend(validate_page_census(repo))
    return errors


def prepare_shadow(repo: Path, shadow: Path) -> None:
    """Copy repository inputs into an isolated regeneration tree."""
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
    for entry in GENERATORS:
        for relative in (*entry.inputs, *entry.outputs):
            source, target = repo / relative, shadow / relative
            if source.is_file():
                target.parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(source, target)


def output_paths(root: Path, entry: Generator) -> set[Path]:
    """Resolve the explicit or globbed outputs of a registered generator."""
    if entry.output_glob:
        return {path for path in root.glob(entry.output_glob)}
    return {root / path for path in entry.outputs}


def clear_generated_html(shadow: Path, entry: Generator) -> None:
    """Remove registered outputs from the isolated shadow tree."""
    for path in output_paths(shadow, entry):
        path.unlink()


def run_script(shadow: Path, command: list[str]) -> subprocess.CompletedProcess[str]:
    """Execute a generator with captured output in the shadow tree."""
    env = os.environ.copy()
    env["PYTHONPATH"] = str(shadow / "src")
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    return subprocess.run(
        command,
        cwd=shadow,
        env=env,
        capture_output=True,
        text=True,
        timeout=600,
    )


def run_generator(shadow: Path, entry: Generator) -> str | None:
    """Run prerequisites and a generator, then verify its outputs."""
    for prerequisite in entry.prerequisites:
        result = run_script(shadow, [sys.executable, str(shadow / prerequisite)])
        if result.returncode:
            detail = result.stderr.strip() or result.stdout.strip()
            return (
                f"{entry.script} prerequisite {prerequisite} failed "
                f"({result.returncode}): {detail}"
            )
    script = shadow / entry.script
    command = [
        sys.executable,
        str(script),
        *(argument if argument.startswith("--") else str(shadow / argument)
          for argument in entry.arguments),
    ]
    if entry.redirect_module_outputs:
        command = [
            sys.executable,
            "-c",
            REDIRECT_DRIVER,
            str(script),
            str(shadow),
        ]
    result = run_script(shadow, command)
    if result.returncode:
        detail = result.stderr.strip() or result.stdout.strip()
        return f"{entry.script} failed ({result.returncode}): {detail}"
    produced = output_paths(shadow, entry)
    if not produced or any(not path.is_file() for path in produced):
        return f"{entry.script} produced no registered HTML output"
    return None


def regenerate(shadow: Path) -> list[str]:
    """Regenerate all committed outputs in the isolated shadow tree."""
    outputs = {entry: {path.relative_to(shadow).as_posix()
                       for path in output_paths(shadow, entry)}
               for entry in GENERATORS}
    for entry in GENERATORS:
        clear_generated_html(shadow, entry)
    pending = list(GENERATORS)
    with ThreadPoolExecutor(max_workers=4) as pool:
        while pending:
            ready = [entry for entry in pending if not set(entry.inputs) & {
                path for other in pending if other != entry for path in outputs[other]
            }]
            if not ready:
                return ["cyclic generated HTML input dependencies"]
            results = pool.map(lambda entry: run_generator(shadow, entry), ready)
            if errors := [error for error in results if error]:
                return errors
            pending = [entry for entry in pending if entry not in ready]
    return []


def all_registered_outputs(root: Path) -> set[Path]:
    """Collect paths owned by registered deterministic generators."""
    paths = set()
    for entry in GENERATORS:
        paths.update(output_paths(root, entry))
    return paths


def short_diff(committed: Path, generated: Path) -> list[str]:
    """Return a bounded text diff for a generated page."""
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
    """Find differences between committed and regenerated pages."""
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
    """Check registry coverage and byte-identical regeneration."""
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
    """Parse the read-only freshness-check command."""
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
