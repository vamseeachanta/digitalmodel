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
    OUTPUTS, digest, load_contract, read_yaml, verify_manifest,
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
                present = candidate[key] if key in candidate else reference[key]
                if isinstance(present, dict) and present:
                    yield from _differences(reference.get(key, {}), candidate.get(key, {}), child)
                    continue
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
    reference = read_yaml(REFERENCE)
    generated = _generated_sections(root / "bundle")
    differences = list(_differences(reference, generated))
    report = {"reference_sha256": digest(REFERENCE), "source_sha256": manifest["source"]["sha256"],
              "input_files": manifest["files"], "reference_compatible": not differences,
              "difference_count": len(differences), "differences": differences,
              "comparison": "Recursive mapping merge in generated include order; lists compared in order; no metadata exclusions.",
              "native_verified": False, "engineering_parity": False}
    report["contract_source_role"] = "governing_template"
    (root / "reference-differences.json").write_text(json.dumps(report, indent=2), encoding="utf-8")


def _selected_source(source_path, *, template_path=None, governing_template=None):
    template, selected = read_yaml(template_path or SOURCE), read_yaml(source_path)
    changes = sorted(item['path'] for item in _differences(template, selected))
    if governing_template is None:
        governing_template = Path(source_path).resolve() == SOURCE.resolve()
    if governing_template:
        if changes:
            raise ValueError('governing template changed during selection')
        return changes
    allowed = ['/environment/raw_properties/VerticalWindVariationFactor',
               '/environment/raw_properties/WindType']
    if changes != allowed:
        raise ValueError('unreviewed source difference outside exact two-key allow-set')
    raw = selected['environment']['raw_properties']
    if raw['WindType'] != 'Constant' or raw['VerticalWindVariationFactor'] is not None:
        raise ValueError('derived Constant wind/null intent differs')
    return changes


def _files(root):
    return [{"path": p.relative_to(root).as_posix(), "sha256": digest(p)}
            for p in sorted(root.rglob("*")) if p.is_file()]


def prepare_bundle(output_root: Path, *, source_path=SOURCE) -> Path:
    if os.environ.get("PYTHONHASHSEED") != "0":
        raise ValueError("Python hash seed 0 required at process startup")
    source_path = Path(source_path)
    applied_keys = _selected_source(source_path)
    input_hashes = dict(template_sha256=digest(SOURCE), reference_sha256=digest(REFERENCE),
                        source_sha256=digest(source_path))
    root = Path(output_root).resolve()
    root.mkdir(parents=True, exist_ok=False)
    try:
        source = root / "source/spec.yml"
        source.parent.mkdir()
        shutil.copyfile(source_path, source)
        shutil.copyfile(SOURCE, root / 'source/template.yml')
        if (digest(source) != input_hashes['source_sha256']
                or digest(root / 'source/template.yml') != input_hashes['template_sha256']):
            raise ValueError('source bytes changed during retention')
        if _selected_source(source, template_path=root / 'source/template.yml',
                governing_template=source_path.resolve() == SOURCE.resolve()) != applied_keys:
            raise ValueError('retained source differences changed')
        derivation = dict(input_hashes, applied_keys=applied_keys, contract_source_role='governing_template')
        (root / 'source/derivation.json').write_text(json.dumps(derivation, indent=2), encoding='utf-8')
        with contextlib.redirect_stdout(sys.stderr):
            ModularModelGenerator(source).generate(root / "bundle")
        contract = load_contract()
        files = _files(root)
        revision = subprocess.check_output(["git", "-C", str(ROOT), "rev-parse", "HEAD"], text=True).strip()
        manifest = {"schema_version": 1, "case_id": contract["case_id"], "run_id": uuid.uuid4().hex,
                    "source_revision": revision, "source": {"path": "source/spec.yml", "sha256": digest(source)},
                    "master": "bundle/master.yml", "files": files, "contract": contract,
                    "python_hash_seed": "0", "outputs": dict(OUTPUTS)}
        _reference_report(root, manifest)
        if digest(REFERENCE) != input_hashes['reference_sha256']:
            raise ValueError('reference bytes changed during comparison')
        manifest['files'] = _files(root)
        path = root / "manifest.json"
        path.write_text(json.dumps(manifest, indent=2), encoding="utf-8")
        verify_manifest(path)
        return path
    except Exception:
        # Retain partial inputs and outputs as failed preparation evidence.
        raise


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--source", type=Path, default=SOURCE)
    args = parser.parse_args()
    print(prepare_bundle(args.output, source_path=args.source))


if __name__ == "__main__":
    main()
