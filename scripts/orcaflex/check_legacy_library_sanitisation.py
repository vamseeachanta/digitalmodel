# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Inspect legacy model inputs before preparing comparison or native runs.

Uses the repository identifier gate without accepting opaque content through
its uninspectable-file baseline. A digest authenticates bytes, not inspection.
Reports remain outside the repository, including when inspection fails. No
source model is changed, and no native solver interface is imported.
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import re
from hashlib import sha256
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
LIBRARY_GROUPS = ("jumper", "mooring", "installation", "training")
GATE_PATH = ROOT / "scripts/legal/check_identifiers.py"
_spec = importlib.util.spec_from_file_location("legacy_identifier_gate", GATE_PATH)
gate = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(gate)


def model_type(path: Path) -> str:
    """Describe the file's role without inferring an engineering configuration."""
    if path.suffix.lower() == ".dat":
        return "native_binary"
    if path.name in ("spec.yml", "spec.yaml"):
        return "specification"
    if path.name in ("master.yml", "master.yaml"):
        return "modular_master"
    if "includes" in path.parts:
        return "include_fragment"
    if "inputs" in path.parts:
        return "parameters"
    return "yaml_model"


def inventory(root: Path) -> list[dict]:
    """Enumerate model-related files in the four retained library subsets."""
    paths = []
    for group in LIBRARY_GROUPS:
        library = root / "docs/domains/orcaflex" / group
        for path in library.rglob("*"):
            if path.suffix.lower() not in (".yml", ".yaml", ".dat"):
                continue
            if path.is_symlink():
                raise ValueError("symbolic input refused")
            if path.is_file():
                paths.append(path)
    return [
        {
            "path": path.relative_to(root).as_posix(),
            "sha256": sha256(path.read_bytes()).hexdigest(),
            "size_bytes": path.stat().st_size,
            "model_type": model_type(path),
        }
        for path in sorted(paths)
    ]


def inspect_model(path: Path, relative: str, rules: dict) -> dict:
    """Require an actual content inspection; retain rule names, never excerpts."""
    findings, scanned, media, uninspectable = gate.check([str(path)], rules)
    rule_ids = set()
    for finding in findings:
        match = re.search(r"\[([^\]]+)\]", finding)
        rule_ids.add(match.group(1) if match else "identifier-finding")
    if uninspectable:
        rule_ids.add("content-uninspectable")
    if media:
        rule_ids.add("content-not-inspected")
    if scanned != 1 and not rule_ids:
        rule_ids.add("content-not-inspected")
    path_withheld = any(":path:" in finding for finding in findings)
    return {
        "path": "[identifier in path]" if path_withheld else relative,
        "status": "fail" if rule_ids else "pass",
        "inspected_files": scanned,
        "rules": sorted(rule_ids),
    }


def audit(root: Path = ROOT) -> dict:
    """Produce deterministic per-file evidence and a fail-closed summary."""
    rules = gate.load_rules()
    models = []
    for item in inventory(root):
        path = root / item["path"]
        inspection = inspect_model(path, item["path"], rules)
        if sha256(path.read_bytes()).hexdigest() != item["sha256"]:
            raise ValueError("source bytes changed during inspection")
        models.append({**item, **inspection})
    passed = sum(item["status"] == "pass" for item in models)
    failed = len(models) - passed
    return {
        "schema": "legacy-library-sanitisation-1",
        "criterion": "Every model input is inspected with no identifier findings.",
        "identifier_gate": "scripts/legal/check_identifiers.py",
        "identifier_gate_sha256": sha256(GATE_PATH.read_bytes()).hexdigest(),
        "public_rules_sha256": sha256(Path(gate.RULES).read_bytes()).hexdigest(),
        "binary_baseline_accepted": False,
        "native_solver_used": False,
        "publication_ready": bool(models) and failed == 0,
        "summary": {"total": len(models), "pass": passed, "fail": failed},
        "models": models,
    }


def publication_records(report: dict, root: Path = ROOT) -> dict:
    """Publish complete inspected models; disclose exclusions only as counts.

    A model is a standalone input or modular master. Includes and parameter
    files belong to their referring model. BaseFile and include dependencies
    are followed transitively; an opaque dependency excludes the whole model.
    The original audit remains unchanged and still fails on opaque inputs.
    """
    import yaml

    items = {item["path"]: item for item in report["models"]}
    dependencies = {}
    for relative, item in items.items():
        path = root / relative
        if sha256(path.read_bytes()).hexdigest() != item["sha256"]:
            raise ValueError("source digest mismatch")
        references = set()

        def visit(value):
            if isinstance(value, dict):
                for key, child in value.items():
                    if str(key).lower() in ("basefile", "includefile", "includefiles"):
                        for name in child if isinstance(child, list) else [child]:
                            if isinstance(name, str):
                                resolved = (path.parent / name.replace("\\", "/")).resolve()
                                if resolved.is_relative_to(root.resolve()):
                                    references.add(resolved.relative_to(root.resolve()).as_posix())
                                else:
                                    references.add("[outside-library]")
                    else:
                        visit(child)
            elif isinstance(value, list):
                for child in value:
                    visit(child)

        if item["status"] == "pass":
            try:
                visit(yaml.load(path.read_text(encoding="utf-8-sig"),
                                Loader=getattr(yaml, "CSafeLoader", yaml.SafeLoader)))
            except (yaml.YAMLError, UnicodeError, RecursionError):
                # Parse failures receive validator records, never a parse pass.
                pass
        if item["model_type"] == "modular_master":
            references.update(
                name for name in items
                if any(name.startswith((path.parent / folder).relative_to(root).as_posix() + "/")
                       for folder in ("includes", "inputs"))
            )
        dependencies[relative] = references

    def closure(relative):
        found, pending = set(), [relative]
        while pending:
            name = pending.pop()
            if name in found:
                continue
            found.add(name)
            pending.extend(dependencies.get(name, set()) - found)
        return found

    models, retained, excluded = [], set(), 0
    failed_models, missing_models = 0, 0
    for relative, item in items.items():
        if item["model_type"] in ("include_fragment", "parameters"):
            continue
        inputs = closure(relative)
        failed = any(name in items and items[name]["status"] != "pass" for name in inputs)
        missing = bool(inputs - items.keys())
        failed_models += int(failed)
        missing_models += int(missing)
        if failed or missing:
            excluded += 1
            continue
        recorded = sorted(inputs & items.keys())
        retained.update(recorded)
        models.append({"path": relative, "model_type": item["model_type"],
                       "inputs": recorded,
                       "missing_dependency_count": len(inputs - items.keys())})
    return {
        "schema": "legacy-library-publication-1",
        "native_solver_used": False,
        "counts": {
            "library_inputs": len(items), "published_inputs": len(retained),
            "excluded_inputs": len(items) - len(retained),
            "failed_inspections": report["summary"]["fail"],
            "excluded_models": excluded, "published_models": len(models),
            "models_with_failed_inputs": failed_models,
            "models_with_missing_inputs": missing_models,
        },
        "inputs": [items[name] for name in sorted(retained)],
        "models": models,
    }


def write_report(output: Path, report: dict, root: Path = ROOT) -> None:
    """Keep failure evidence out of the public source tree."""
    if output.resolve().is_relative_to(root.resolve()):
        raise ValueError("report must remain outside the repository")
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(
        json.dumps(report, indent=2, sort_keys=True, allow_nan=False) + "\n",
        encoding="utf-8",
    )


def main() -> int:
    """Write the external record and return failure for any uninspected input."""
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    try:
        report = audit()
        write_report(args.output, report)
    except Exception as error:
        # Exception messages can quote source content or a private locator.
        print(f"Library inspection failed ({type(error).__name__}); no detail emitted.")
        return 2
    print(json.dumps(report["summary"], sort_keys=True))
    return 0 if report["publication_ready"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
