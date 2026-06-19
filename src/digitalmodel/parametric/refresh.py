"""Atlas refresh automation (#799, epic #794).

An atlas is a cache of a workflow's response surface. It becomes *stale* when
the basis it was built from changes: the parametric spec, the input template,
the response-relevant source code, or a cited standard edition. This module
fingerprints that basis so staleness is detectable (the query path escalates a
stale atlas) and regenerable (a CI / drift-sentinel job runs ``--check`` and,
on drift, ``--apply``).

The fingerprint deliberately excludes the repo git sha: an atlas is not stale
just because an unrelated commit landed — only when its own basis moves.
"""

from __future__ import annotations

import hashlib
import json
import subprocess
import sys
from pathlib import Path
from typing import Any

import yaml

REPO_ROOT = Path(__file__).resolve().parents[3]
REGISTRY = REPO_ROOT / "docs" / "registry" / "workflows.yaml"
DEFAULT_ATLAS_ROOT = REPO_ROOT / "atlases"

# Cited standard editions per basename — the documentary source of the curves
# / formulae an atlas encodes. Bumping an edition invalidates the atlas.
STANDARDS: dict[str, list[dict[str, str]]] = {
    "mooring_fatigue": [{"id": "DNV-RP-C203", "edition": "2021-09"}],
    "synthetic_rope_mooring_fatigue": [{"id": "DNV-OS-E301", "edition": "2018-07"}],
    "code_check": [{"id": "API-RP-2RD", "edition": "2013"}],
    "rao_tabulation": [],
    "free_span": [{"id": "DNV-RP-F105", "edition": "2017-06"}],
    "pile_capacity": [{"id": "API-RP-2GEO", "edition": "2011"}],
    "anchor_capacity": [{"id": "DNV-RP-E303", "edition": "2005"}],
    "spectral_fatigue": [{"id": "DNV-RP-C203", "edition": "2021-09"},
                         {"id": "DNV-RP-C205", "edition": "2021-09"}],
    "fpso_mooring_full": [{"id": "DNV-OS-E301", "edition": "2018-07"}],
    "viv_analysis": [{"id": "DNV-RP-F105", "edition": "2017-06"}],
}

# Response-relevant source files per basename: the code whose change would
# alter the computed response (NOT the whole package — excludes generate.py so
# adding an unrelated workflow doesn't invalidate every atlas).
SOURCE_FILES: dict[str, list[str]] = {
    "mooring_fatigue": [
        "src/digitalmodel/fatigue/sn_curves.py",
        "src/digitalmodel/fatigue/damage.py",
    ],
    # response is a replicated T-N formula; track the workflow so divergence
    # from the source flags the atlas stale.
    "synthetic_rope_mooring_fatigue": [
        "src/digitalmodel/synthetic_rope_mooring_fatigue/workflow.py",
    ],
    "code_check": [
        "src/digitalmodel/orcaflex/code_check_engine.py",
    ],
    "rao_tabulation": [
        "src/digitalmodel/hydrodynamics/interpolator.py",
        "src/digitalmodel/hydrodynamics/models.py",
    ],
    "free_span": [
        "src/digitalmodel/subsea/pipeline/free_span/__init__.py",
        "src/digitalmodel/subsea/pipeline/free_span/models.py",
        "src/digitalmodel/subsea/pipeline/free_span/span_allowable_length.py",
        "src/digitalmodel/subsea/pipeline/free_span/span_natural_frequency.py",
        "src/digitalmodel/subsea/pipeline/free_span/span_onset_screening.py",
        "src/digitalmodel/subsea/pipeline/free_span/span_viv_response.py",
        "src/digitalmodel/subsea/pipeline/free_span/span_fatigue_damage.py",
    ],
    "pile_capacity": ["src/digitalmodel/geotechnical/pile_capacity.py"],
    "anchor_capacity": ["src/digitalmodel/geotechnical/anchors.py"],
    "spectral_fatigue": [
        "src/digitalmodel/fatigue/spectral_fatigue.py",
        "src/digitalmodel/hydrodynamics/wave_spectra.py",
    ],
    "fpso_mooring_full": [
        "src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py",
    ],
    "viv_analysis": [
        "src/digitalmodel/subsea/viv_analysis/viv_analysis.py",
        "src/digitalmodel/subsea/viv_analysis/viv_tubular_members.py",
    ],
}

# Expected state of each licensed-solver library (#801/#831). A library is NOT
# code-computable, so its staleness is judged against what the operator declares
# as current here — not a re-run. Stale = the committed library's recorded
# solver name/version or covered case set no longer matches this expectation
# (e.g. bump `version` once real OrcaWave runs replace the stub, and the stub
# library immediately reads stale -> escalate -> prompts a real run).
LIBRARY_EXPECTATIONS: dict[str, dict[str, Any]] = {
    "diffraction_library": {
        "solver_name": "OrcaWave/AQWA",
        "solver_version": "STUB",
        "cases": ["fpso-design-draft", "fpso-ballast-draft", "semisub-operating"],
    },
}


def _registry_rows() -> list[dict[str, Any]]:
    return yaml.safe_load(REGISTRY.read_text())["workflows"]


def _row(workflow_id: str) -> dict[str, Any]:
    for row in _registry_rows():
        if row["id"] == workflow_id:
            return row
    raise KeyError(f"workflow id not in registry: {workflow_id}")


def parametric_workflow_ids() -> list[str]:
    """Registry ids that declare a parametric: block (i.e. own an atlas)."""
    return [row["id"] for row in _registry_rows() if row.get("parametric")]


def _sha_files(paths: list[str], repo_root: Path) -> str:
    h = hashlib.sha256()
    for rel in paths:
        h.update(rel.encode())
        h.update((repo_root / rel).read_bytes())
    return h.hexdigest()


def content_fingerprint(workflow_id: str, repo_root: Path = REPO_ROOT) -> str:
    """Hash of everything an atlas's correctness depends on. Stable across
    unrelated commits; changes iff spec / template / source / standard move."""
    row = _row(workflow_id)
    block = row["parametric"]
    basename = row["basename"]
    basis = {
        "spec": {
            "physics": block["physics"],
            "response": block["response"],
            "axes": block["axes"],
        },
        "response_kwargs": block.get("response_kwargs", {}),
        "standards": STANDARDS.get(basename, []),
        "input_sha256": hashlib.sha256(
            (repo_root / row["input"]).read_bytes()
        ).hexdigest(),
        "source_sha256": _sha_files(SOURCE_FILES.get(basename, []), repo_root),
    }
    return hashlib.sha256(json.dumps(basis, sort_keys=True).encode()).hexdigest()


def refresh_status(
    workflow_id: str, atlas_root: Path = DEFAULT_ATLAS_ROOT
) -> dict[str, Any]:
    """Compare the live atlas's stored fingerprint with the current basis."""
    from digitalmodel.parametric.atlas import Atlas

    row = _row(workflow_id)
    basename = row["basename"]
    current = content_fingerprint(workflow_id)
    try:
        atlas = Atlas.load(atlas_root, basename)
    except FileNotFoundError:
        return {"workflow_id": workflow_id, "stale": True, "reason": "no atlas built",
                "current": current, "stored": None}
    stored = atlas.provenance.get("content_fingerprint")
    stale = stored != current
    return {
        "workflow_id": workflow_id,
        "basename": basename,
        "stale": stale,
        "reason": "basis changed since build" if stale else "current",
        "current": current,
        "stored": stored,
        "atlas_id": atlas.atlas_id,
    }


def library_drift(atlas: Any) -> list[str]:
    """Reasons the given library atlas no longer matches its declared
    expectation (solver name/version, covered case set), or [] if current."""
    expected = LIBRARY_EXPECTATIONS.get(atlas.basename)
    if expected is None:
        return []
    solver = atlas.provenance.get("solver", {})
    cases = (atlas.provenance.get("coverage", {}) or {}).get("covered_cases", [])
    reasons = []
    if solver.get("name") != expected["solver_name"]:
        reasons.append(f"solver name {solver.get('name')!r} != {expected['solver_name']!r}")
    if solver.get("version") != expected["solver_version"]:
        reasons.append(
            f"solver version {solver.get('version')!r} != {expected['solver_version']!r}")
    if set(cases) != set(expected["cases"]):
        reasons.append(f"covered cases {sorted(cases)} != {sorted(expected['cases'])}")
    return reasons


def library_status(
    basename: str, atlas_root: Path = DEFAULT_ATLAS_ROOT
) -> dict[str, Any]:
    """Staleness of a licensed-solver library vs LIBRARY_EXPECTATIONS. Not
    code-recomputable, so a stale library is reported for an operator run, never
    auto-regenerated."""
    from digitalmodel.parametric.atlas import Atlas

    try:
        atlas = Atlas.load(atlas_root, basename)
    except FileNotFoundError:
        return {"basename": basename, "kind": "library", "stale": True,
                "reason": "no library built"}
    reasons = library_drift(atlas)
    stub = atlas.provenance.get("solver", {}).get("licensed") is False
    return {
        "basename": basename,
        "kind": "library",
        "stale": bool(reasons),
        "reason": "; ".join(reasons) if reasons else ("current (STUB)" if stub else "current"),
        "atlas_id": atlas.atlas_id,
    }


def _git_sha(repo_root: Path) -> str:
    try:
        return subprocess.check_output(
            ["git", "-C", str(repo_root), "rev-parse", "--short", "HEAD"], text=True
        ).strip()
    except Exception:  # pragma: no cover
        return "unknown"


def main(argv: list[str]) -> int:
    """CI drift gate (#833). Exit non-zero when a COMPUTED atlas is stale — a
    code/standard/template change that wasn't regenerated. Libraries (#801) are
    reported but do NOT fail the build (CI can't run a licensed solve) unless
    --strict-libraries is given. --apply regenerates stale computed atlases."""
    apply = "--apply" in argv
    strict_libraries = "--strict-libraries" in argv

    stale_ids = []
    for wid in parametric_workflow_ids():
        status = refresh_status(wid)
        print(f"[{'STALE' if status['stale'] else 'ok':5}] {wid}  ({status['reason']})")
        if status["stale"]:
            stale_ids.append(wid)

    stale_libs = []
    for basename in LIBRARY_EXPECTATIONS:
        status = library_status(basename)
        print(f"[{'STALE' if status['stale'] else 'ok':5}] {basename} (library)  ({status['reason']})")
        if status["stale"]:
            stale_libs.append(basename)

    if stale_libs:
        print(f"\n{len(stale_libs)} stale library(ies) — need a licensed run "
              f"(operator); not auto-regenerated: {stale_libs}")

    if apply and stale_ids:
        from digitalmodel.parametric.build import build_atlas_from_registry
        for wid in stale_ids:
            print(f"refreshing {wid} ...")
            atlas = build_atlas_from_registry(wid)
            if not atlas.validation["passes"]:
                print(f"  WARNING: {wid} regenerated but failed validation gate")
        stale_ids = []  # regenerated; commit the result

    if not stale_ids and not stale_libs:
        print("all atlases current")
    fail = bool(stale_ids) or (strict_libraries and bool(stale_libs))
    if stale_ids and not apply:
        print(f"\n{len(stale_ids)} stale computed atlas(es); run with --apply to regenerate")
    return 1 if fail else 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
