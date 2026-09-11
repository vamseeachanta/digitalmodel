"""Capture the BaseFile goldens from ``template_generator._generate_reference``.

The comparator is an independent in-tree implementation of the OrcaFlex
variation-model reference format.  See ``PROVENANCE.md`` beside this script for
the comparator class, what the goldens do and do not cover, and the divergence
between the reference form emitted here and the inline form emitted by
``modular_generator.writers.basefile``.

Usage (from the repository root, with the repo virtualenv)::

    .venv/Scripts/python.exe tests/solvers/orcaflex/modular_generator/goldens/basefile/capture.py

Re-capture only when ``template_generator._generate_reference`` changes.
"""
from __future__ import annotations

import subprocess
import sys
from datetime import date
from pathlib import Path

import yaml

GOLDENS = Path(__file__).resolve().parent
CASES = GOLDENS / "cases"
FIX = GOLDENS / "fixtures"
# goldens/basefile -> goldens -> modular_generator -> orcaflex -> solvers -> tests -> repo
REPO = GOLDENS.parents[5]
TPL = REPO / "docs/domains/orcaflex/templates"

sys.path.insert(0, str(REPO / "src"))

from digitalmodel.solvers.orcaflex.template_generator import (  # noqa: E402
    TemplateGenerator,
)

# case_id -> (base_file, variation_file, output_file)
SPECS = {
    "calm_buoy_deep_water": (
        TPL / "mooring_systems/calm_buoy_hybrid/base/calm_buoy_base.yml",
        TPL / "mooring_systems/calm_buoy_hybrid/variations/deep_water_200m.yml",
        TPL / "mooring_systems/calm_buoy_hybrid/cases/case_deep_water_golden.yml",
    ),
    "spread_mooring_twelve_leg": (
        TPL / "mooring_systems/spread_mooring_hybrid/base/spread_mooring_base.yml",
        TPL / "mooring_systems/spread_mooring_hybrid/variations/twelve_leg.yml",
        TPL / "mooring_systems/spread_mooring_hybrid/cases/case_twelve_leg_golden.yml",
    ),
    "salm_wire_rope": (
        TPL / "mooring_systems/salm_hybrid/base/salm_base.yml",
        TPL / "mooring_systems/salm_hybrid/variations/wire_rope_leg.yml",
        TPL / "mooring_systems/salm_hybrid/cases/case_wire_rope_golden.yml",
    ),
    "turret_external": (
        TPL / "mooring_systems/turret_mooring_hybrid/base/turret_mooring_base.yml",
        TPL / "mooring_systems/turret_mooring_hybrid/variations/external_turret.yml",
        TPL / "mooring_systems/turret_mooring_hybrid/cases/case_external_turret_golden.yml",
    ),
    # Synthetic: a binary .dat base, sibling to the output directory.
    "dat_base_sibling": (
        FIX / "base_model.dat",
        FIX / "variation.yml",
        FIX / "out/case_dat_base.yml",
    ),
    # Synthetic: base nested below the output directory, which is the only shape
    # that reaches the comparator's Path.relative_to branch.
    "base_below_output": (
        FIX / "sub/base_model.yml",
        FIX / "variation.yml",
        FIX / "case_base_below.yml",
    ),
}


def _rel(path: Path) -> str:
    return path.resolve().relative_to(REPO).as_posix()


def main() -> int:
    sha = subprocess.run(
        ["git", "-C", str(REPO), "rev-parse", "HEAD"],
        capture_output=True, text=True, check=True,
    ).stdout.strip()

    generator = TemplateGenerator()
    CASES.mkdir(parents=True, exist_ok=True)

    manifest_cases = []
    for case_id, (base, variation, out) in SPECS.items():
        out.parent.mkdir(parents=True, exist_ok=True)
        result = generator.generate(base, variation, out, as_reference=True)
        if not result.get("success"):
            print(f"FAIL {case_id}: {result}")
            return 1

        raw = out.read_text(encoding="utf-8")
        golden = CASES / f"{case_id}.yml"
        golden.write_text(raw, encoding="utf-8", newline="\n")
        parsed = yaml.safe_load(raw)
        manifest_cases.append({
            "case_id": case_id,
            "base_file": _rel(base),
            "variation_file": _rel(variation),
            "output_file": _rel(out),
            "golden": _rel(golden),
            "captured_base_file_value": parsed["BaseFile"],
            "captured_include_file_value": parsed["IncludeFile"],
        })

        # Capture artifacts are not left inside the committed template sets, nor
        # duplicated beside the fixtures they were generated from.
        out.unlink()
        print(f"OK {case_id}: BaseFile={parsed['BaseFile']!r}")

    manifest = {
        "schema": "basefile-goldens/1",
        "captured_on": date.today().isoformat(),
        "producer": {
            "module": "digitalmodel.solvers.orcaflex.template_generator",
            "callable": "TemplateGenerator.generate(..., as_reference=True)"
                        " -> TemplateGenerator._generate_reference",
            "source_lines": "src/digitalmodel/solvers/orcaflex/template_generator.py:380-418",
            "repo_commit": sha,
        },
        "comparator_class": "cross-solver",
        "covers": ["BaseFile"],
        "does_not_cover": ["override sections emitted inline by the writer"],
        "cases": manifest_cases,
    }
    (GOLDENS / "manifest.yml").write_text(
        yaml.dump(manifest, sort_keys=False, default_flow_style=False),
        encoding="utf-8", newline="\n",
    )
    print(f"\nmanifest written; repo_commit={sha}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
