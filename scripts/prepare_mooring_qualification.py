"""Prepare the reviewed mooring bundle offline; never invoke a native solver."""
from __future__ import annotations

import argparse
import contextlib
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import uuid

import yaml

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.smoke.model_manifest import (
    OUTPUTS, digest, load_contract, verify_manifest,
)

SOURCE = ROOT / "docs/domains/orcaflex/library/templates/mooring_buoy/spec.yml"
REFERENCE = ROOT / "docs/domains/orcaflex/examples/yml/C07/C07 Metocean buoy in deep water.yml"


def _merge(target, incoming):
    for key, value in incoming.items():
        if isinstance(value, dict) and isinstance(target.get(key), dict):
            _merge(target[key], value)
        else:
            target[key] = value


def _generated_sections(bundle):
    master = yaml.safe_load((bundle / "master.yml").read_text(encoding="utf-8"))
    result = {}
    for item in master:
        path = (bundle / item["includefile"]).resolve()
        if not path.is_relative_to(bundle.resolve()):
            raise ValueError("generated include escapes bundle")
        _merge(result, yaml.safe_load(path.read_text(encoding="utf-8")))
    return result


def _value_hash(value):
    encoded = json.dumps(value, sort_keys=True, default=str).encode("utf-8")
    return hashlib.sha256(encoded).hexdigest()


def _differences(reference, candidate, path=""):
    if isinstance(reference, dict) and isinstance(candidate, dict):
        for key in sorted(reference.keys() | candidate.keys(), key=str):
            child = path + "/" + str(key).replace("~", "~0").replace("/", "~1")
            if key not in reference or key not in candidate:
                yield {"path": child, "kind": "missing_key",
                       "reference_present": key in reference, "candidate_present": key in candidate,
                       "reference_value_sha256": _value_hash(reference.get(key)),
                       "candidate_value_sha256": _value_hash(candidate.get(key))}
            else:
                yield from _differences(reference[key], candidate[key], child)
    elif isinstance(reference, list) and isinstance(candidate, list):
        for index in range(max(len(reference), len(candidate))):
            if index >= min(len(reference), len(candidate)):
                yield {"path": f"{path}/{index}", "kind": "missing_item",
                       "reference_present": index < len(reference), "candidate_present": index < len(candidate),
                       "reference_value_sha256": _value_hash(reference[index] if index < len(reference) else None),
                       "candidate_value_sha256": _value_hash(candidate[index] if index < len(candidate) else None)}
            else:
                yield from _differences(reference[index], candidate[index], f"{path}/{index}")
    elif type(reference) is not type(candidate) or reference != candidate:
        yield {"path": path, "kind": "value_or_type",
               "reference_value_sha256": _value_hash(reference),
               "candidate_value_sha256": _value_hash(candidate)}


def _reference_report(root, manifest):
    reference = yaml.safe_load(REFERENCE.read_text(encoding="utf-8-sig"))
    generated = _generated_sections(root / "bundle")
    differences = list(_differences(reference, generated))
    report = {"reference_sha256": digest(REFERENCE), "source_sha256": manifest["source"]["sha256"],
              "input_files": manifest["files"], "reference_compatible": not differences,
              "difference_count": len(differences), "differences": differences,
              "comparison": "Recursive mapping merge in generated include order; lists compared in order; no metadata exclusions.",
              "native_verified": False, "engineering_parity": False}
    (root / "reference-differences.json").write_text(json.dumps(report, indent=2), encoding="utf-8")


def prepare_bundle(output_root: Path) -> Path:
    if os.environ.get("PYTHONHASHSEED") != "0":
        raise ValueError("Python hash seed 0 required at process startup")
    root = Path(output_root).resolve()
    root.mkdir(parents=True, exist_ok=False)
    try:
        source = root / "source/spec.yml"
        source.parent.mkdir()
        shutil.copyfile(SOURCE, source)
        with contextlib.redirect_stdout(sys.stderr):
            ModularModelGenerator(source).generate(root / "bundle")
        contract = load_contract()
        files = [{"path": p.relative_to(root).as_posix(), "sha256": digest(p)}
                 for p in sorted(root.rglob("*")) if p.is_file()]
        revision = subprocess.check_output(["git", "-C", str(ROOT), "rev-parse", "HEAD"], text=True).strip()
        manifest = {"schema_version": 1, "case_id": contract["case_id"], "run_id": uuid.uuid4().hex,
                    "source_revision": revision, "source": {"path": "source/spec.yml", "sha256": digest(source)},
                    "master": "bundle/master.yml", "files": files, "contract": contract,
                    "python_hash_seed": "0", "outputs": dict(OUTPUTS)}
        path = root / "manifest.json"
        path.write_text(json.dumps(manifest, indent=2), encoding="utf-8")
        verify_manifest(path)
        _reference_report(root, manifest)
        return path
    except Exception:
        # Only this call's fresh, resolved output root is owned by this cleanup.
        shutil.rmtree(root)
        raise


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    print(prepare_bundle(args.output))


if __name__ == "__main__":
    main()
