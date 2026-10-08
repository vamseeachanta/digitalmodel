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

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.smoke.model_manifest import (
    OUTPUTS, _relative, digest, load_contract, read_yaml, verify_manifest,
)

SOURCE = ROOT / "docs/domains/orcaflex/library/templates/mooring_buoy/spec.yml"
REFERENCE = ROOT / "docs/domains/orcaflex/examples/yml/C07/C07 Metocean buoy in deep water.yml"
ABSENT_VALUE_BYTES = b"\x00digitalmodel.absent.v1"


def _merge(target, incoming):
    for key, value in incoming.items():
        if isinstance(value, dict) and isinstance(target.get(key), dict):
            _merge(target[key], value)
        else:
            target[key] = value


def _generated_sections(bundle):
    master = read_yaml(bundle / "master.yml")
    if not isinstance(master, list):
        raise ValueError('generated master requires ordered includes')
    result, owners, updates = {}, {}, []
    for item in master:
        if not isinstance(item, dict) or set(item) != {'includefile'}:
            raise ValueError('generated master requires explicit include records')
        name = item['includefile']
        if not isinstance(name, str):
            raise ValueError('generated include requires filename')
        path = _relative(bundle.resolve(), name)
        incoming = read_yaml(path)
        if not isinstance(incoming, dict) or 'includefile' in incoming:
            raise ValueError('generated section must be a direct section mapping')
        for section, value in incoming.items():
            if section in result:
                if section == 'General':
                    raise ValueError('duplicate General ownership in generated includes')
                updates.append(dict(section=section, previous_include=owners[section],
                    current_include=name, previous_value_sha256=_value_hash(result[section]),
                    current_value_sha256=_value_hash(value)))
            owners[section] = name
        _merge(result, incoming)
    return result, updates


def _value_hash(value, *, present=True):
    if not present:
        return hashlib.sha256(ABSENT_VALUE_BYTES).hexdigest()
    encoded = json.dumps(
        value, sort_keys=True, default=str, ensure_ascii=True, allow_nan=True,
        skipkeys=False, check_circular=True, indent=None, separators=(", ", ": "),
    ).encode("utf-8")
    return hashlib.sha256(encoded).hexdigest()


def _missing_member_hash_rule():
    return {
        "id": "digitalmodel.missing-member.sha256.v1",
        "algorithm": "sha256",
        "absent_preimage_hex": ABSENT_VALUE_BYTES.hex(),
        "absent_sha256": hashlib.sha256(ABSENT_VALUE_BYTES).hexdigest(),
        "present_encoder": {
            "function": "json.dumps", "sort_keys": True, "default": "str",
            "ensure_ascii": True, "allow_nan": True, "skipkeys": False,
            "check_circular": True, "indent": None, "separators": [", ", ": "],
            "text_encoding": "utf-8",
        },
        "present_value_digest_uniqueness": False,
        "container_limit": {
            "id": "recursive-leaf-only-v2",
            "mapping_key_missing_nonempty_mapping_action": "recurse_with_empty_mapping",
            "mapping_key_missing_nonempty_mapping_container_row_emitted": False,
            "mapping_key_missing_nonempty_mapping_distinguishes_absent_from_empty": False,
            "mapping_key_missing_other_value_action": "hash_whole_value",
            "list_missing_member_action": "hash_whole_member",
            "list_missing_member_row_emitted": True,
        },
    }


def _differences(reference, candidate, path=""):
    if isinstance(reference, dict) and isinstance(candidate, dict):
        for key in sorted(reference.keys() | candidate.keys(), key=str):
            child = path + "/" + str(key).replace("~", "~0").replace("/", "~1")
            if key not in reference or key not in candidate:
                reference_present = key in reference
                candidate_present = key in candidate
                present = candidate[key] if candidate_present else reference[key]
                if isinstance(present, dict) and present:
                    yield from _differences(reference.get(key, {}), candidate.get(key, {}), child)
                    continue
                yield {"path": child, "kind": "missing_key",
                       "reference_present": reference_present,
                       "candidate_present": candidate_present,
                       "reference_value_sha256": _value_hash(
                           reference.get(key), present=reference_present),
                       "candidate_value_sha256": _value_hash(
                           candidate.get(key), present=candidate_present)}
            else:
                yield from _differences(reference[key], candidate[key], child)
    elif isinstance(reference, list) and isinstance(candidate, list):
        for index in range(max(len(reference), len(candidate))):
            if index >= min(len(reference), len(candidate)):
                reference_present = index < len(reference)
                candidate_present = index < len(candidate)
                yield {"path": f"{path}/{index}", "kind": "missing_item",
                       "reference_present": reference_present,
                       "candidate_present": candidate_present,
                       "reference_value_sha256": _value_hash(
                           reference[index] if reference_present else None,
                           present=reference_present),
                       "candidate_value_sha256": _value_hash(
                           candidate[index] if candidate_present else None,
                           present=candidate_present)}
            else:
                yield from _differences(reference[index], candidate[index], f"{path}/{index}")
    elif type(reference) is not type(candidate) or reference != candidate:
        yield {"path": path, "kind": "value_or_type",
               "reference_value_sha256": _value_hash(reference),
               "candidate_value_sha256": _value_hash(candidate)}


def _reference_report(root, manifest):
    reference = read_yaml(REFERENCE)
    generated, updates = _generated_sections(root / "bundle")
    differences = list(_differences(reference, generated))
    report = {"schema_version": 2,
              "missing_member_hash_rule": _missing_member_hash_rule(),
              "reference_sha256": digest(REFERENCE), "source_sha256": manifest["source"]["sha256"],
              "input_files": manifest["files"], "reference_compatible": not differences and not updates,
              "ordered_updates": updates,
              "difference_count": len(differences), "differences": differences,
              "comparison": "Strict YAML with presence-aware missing-member hashes; under a mapping-key parent, a missing non-empty mapping recurses to descendant rows without a container row, so an absent container and a present empty mapping can yield identical rows; a missing list member hashes the whole member in one missing_item row; ordered non-General section updates recorded; recursive mapping merge, lists compared in order; no metadata exclusions.",
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
