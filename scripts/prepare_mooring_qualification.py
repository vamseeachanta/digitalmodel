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
# Preimage of the digest recorded for a member that is absent (not present-and-null).
ABSENT_VALUE_BYTES = b"\x00digitalmodel.absent.v1"


def _merge(target, incoming):
    for key, value in incoming.items():
        if isinstance(value, dict) and isinstance(target.get(key), dict):
            _merge(target[key], value)
        else:
            target[key] = value


def _generated_sections(bundle):
    """Merge the generated includes in master order. Strict YAML (duplicate keys and
    aliases refused) so that no assignment is hidden from the comparison."""
    master = read_yaml(bundle / "master.yml")
    if not isinstance(master, list):
        raise ValueError("generated master requires ordered includes")
    result = {}
    for item in master:
        if not isinstance(item, dict) or set(item) != {"includefile"}:
            raise ValueError("generated master requires explicit include records")
        incoming = read_yaml(_relative(bundle.resolve(), item["includefile"]))
        if not isinstance(incoming, dict) or "includefile" in incoming:
            raise ValueError("generated section must be a direct section mapping")
        _merge(result, incoming)
    return result


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
    generated = _generated_sections(root / "bundle")
    differences = list(_differences(reference, generated))
    report = {"schema_version": 2,
              "missing_member_hash_rule": _missing_member_hash_rule(),
              "reference_sha256": digest(REFERENCE), "source_sha256": manifest["source"]["sha256"],
              "input_files": manifest["files"], "reference_compatible": not differences,
              "difference_count": len(differences), "differences": differences,
              "comparison": "Strict YAML (duplicate keys and aliases refused) with presence-aware "
                            "missing-member hashes; under a mapping-key parent, a missing non-empty "
                            "mapping recurses to descendant rows without a container row, so an absent "
                            "container and a present empty mapping can yield identical rows; a missing "
                            "list member hashes the whole member in one missing_item row; recursive "
                            "mapping merge in generated include order; lists compared in order; no "
                            "metadata exclusions.",
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
