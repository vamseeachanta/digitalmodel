# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Audit generated frozen mesh bytes against a reviewed reference packet.

This reads serialized inputs only. It does not load or qualify a native model.
"""

import argparse
import copy
import hashlib
import io
import json
import math
from pathlib import Path
import subprocess
import tarfile
from typing import Any

import yaml

from .generate_inputs import CAMPAIGN, _json_bytes, _write, producing_runtime

COUNT_TABLE = "LineType, Length, NumberOfSegments"
TARGET_TABLE = "LineType, Length, TargetSegmentLength"
HISTORY_PACKET = "docs/verification/riser-mesh-review.json"
FIXTURES = CAMPAIGN.parents[1] / "tests/riser_campaign/fixtures"
REFERENCE_REVISION = "d81ccf84ddc12a2a3cc45340537bec47a7807cd5"
REFERENCE_DIGEST = "6d45a583a2265db6ecd1e8f47f2cc7c50893fe88f2fba19fa048f88e02aca657"
HISTORY_FIXTURES = {
    REFERENCE_REVISION: ("prior-packet.json", "5139c5e2e6ff7192976ff2305c3786bf27576445317a8ad81bc7ab473d2f294c"),
    "bb123d57e5a51bfc15020eeee3674cd5103043e4": (
        "reissue-packet.json", "89cf5e9bc9d721947702d61e980001480eb8fcfb6691e233288e617b991914a2"),
    "0b83ecb072ca59f410b14872d426f9e5699ed41e": (
        "original-packet.json", "6803055a86130c496c934dc172c1ca7258d4abe600c0e567be7a43003ba45a42"),
}


def _fixture_bytes(name: str, digest: str) -> bytes:
    payload = (FIXTURES / name).read_bytes()
    if hashlib.sha256(payload).hexdigest() != digest:
        raise ValueError("retained fixture digest mismatch")
    return payload


def reference_fixture() -> dict[str, bytes]:
    """Read the pinned reference without requiring an unmerged Git object."""
    payload = _fixture_bytes("reviewed-reference.tar.xz", REFERENCE_DIGEST)
    with tarfile.open(fileobj=io.BytesIO(payload), mode="r:xz") as archive:
        return {item.name: archive.extractfile(item).read()
                for item in archive.getmembers() if item.isfile()}


def _verify_reference_sources(reference: Path, sources: list[dict[str, Any]],
                              revision: str) -> None:
    """Verify raw snapshots, or exact source pins from the retained reference.

    The retained archive omits shared source text. Its authenticated index
    preserves each reviewed source digest and length without republishing it.
    Arbitrary references must still supply all raw source snapshots.
    """
    if not sources:
        raise ValueError("retained source pin coverage must not be empty")
    pins = None
    for source in sources:
        result = subprocess.run(
            ["git", "show", f"{revision}:{source['path'].removeprefix('source/')}"],
            cwd=CAMPAIGN.parents[1], check=False, capture_output=True,
        )
        snapshot_exists = (reference / source["path"]).is_file()
        if result.returncode or not snapshot_exists:
            if revision != REFERENCE_REVISION:
                raise ValueError("reference revision unavailable or snapshot omitted; no digest-pinned fixture exists")
            if pins is None:
                retained = reference_fixture()
                index = json.loads(retained["index.json"])
                expected = index["frozen_tranche"]["code_sources"]
                if sources != expected:
                    raise ValueError("reference differs from the retained source pin coverage")
                pins = {item["path"]: item for item in expected}
            if source != pins.get(source["path"]):
                raise ValueError("reference source differs from the retained source pin")
        if snapshot_exists:
            payload = _verified(reference, source)
            if not result.returncode and payload != result.stdout:
                raise ValueError("reference source does not match the reviewed revision")
        elif not result.returncode:
            if (hashlib.sha256(result.stdout).hexdigest() != source["sha256"]
                    or len(result.stdout) != source["bytes"]):
                raise ValueError("retained source pin does not match the reviewed revision")


def _history(path: Path, revision: str) -> tuple[dict[str, Any], list[dict[str, Any]]]:
    """Retain correction records only from the exact committed packet bytes."""
    repo = CAMPAIGN.parents[1]
    resolved = subprocess.run(
        ["git", "rev-parse", "--verify", "--end-of-options", f"{revision}^{{commit}}"],
        cwd=repo, check=False, capture_output=True, text=True, encoding="utf-8",
    )
    if resolved.returncode:
        if revision not in HISTORY_FIXTURES:
            raise ValueError("history revision unavailable and no digest-pinned fixture exists")
        recorded_revision = revision
        committed = _fixture_bytes(*HISTORY_FIXTURES[revision])
    else:
        recorded_revision = resolved.stdout.strip()
        result = subprocess.run(
            ["git", "show", f"{recorded_revision}:{HISTORY_PACKET}"],
            cwd=repo, check=False, capture_output=True,
        )
        if result.returncode:
            raise ValueError("history revision contains no review packet")
        committed = result.stdout
    payload = path.read_bytes()
    digest = hashlib.sha256(payload).hexdigest()
    if digest != hashlib.sha256(committed).hexdigest() or len(payload) != len(committed):
        raise ValueError("history packet digest mismatch against the recorded revision")
    packet = json.loads(payload)
    records = {key: packet[key] for key in (
        "coefficient_axis_correction", "original_coefficient_axis_correction") if key in packet}
    if "historical_corrections" in packet:
        history = packet["historical_corrections"]
    else:
        if not records:
            raise ValueError("history packet has no correction records")
        history = {"recorded_at_revision": recorded_revision,
                   "reference_revision": packet["reference_revision"], **records}
    if not isinstance(history, dict) or not history:
        raise ValueError("history packet correction records must be a nonempty object")
    sources = copy.deepcopy(packet.get("history", []))
    if not isinstance(sources, list) or any(
        not isinstance(item, dict) or not {"revision", "sha256", "bytes", "records"} <= item.keys()
        for item in sources
    ):
        raise ValueError("history entries must bind revisions, digests, bytes and records")
    if not sources and "history_source" in packet:
        sources.append({**packet["history_source"], "records": copy.deepcopy(history)})
    sources.append({"path": HISTORY_PACKET, "revision": recorded_revision,
                    "sha256": digest, "bytes": len(payload),
                    "records": copy.deepcopy(records or history)})
    return history, sources


def _changed_fields(before: Any, after: Any, path: str = "$") -> list[str]:
    """Compare types and values while ignoring mapping insertion order only."""
    if type(before) is not type(after):
        # YAML may spell the same physical number as an integer or a float.
        # Booleans must never compare equal to numeric zero or one.
        if (type(before) in (int, float) and type(after) in (int, float)
                and before == after):
            return []
        return [path]
    if isinstance(before, dict):
        changed = []
        for key in sorted(before.keys() | after.keys(), key=str):
            child = f"{path}.{key}"
            if key not in before or key not in after:
                changed.append(child)
            else:
                changed.extend(_changed_fields(before[key], after[key], child))
        return changed
    if isinstance(before, list):
        if len(before) != len(after):
            return [path]
        return [field for i, (a, b) in enumerate(zip(before, after))
                for field in _changed_fields(a, b, f"{path}[{i}]")]
    return [] if before == after else [path]


def _serialization_record(old: bytes, new: bytes, relative: str,
                          records: list[dict[str, Any]], old_model: Any, new_model: Any) -> None:
    fields = _changed_fields(old_model, new_model)
    # Parsed equality alone would also allow comment and scalar-style changes.
    # Require the exact same non-comment YAML lines for a key-order-only claim.
    old_lines = sorted(line.strip() for line in old.decode("utf-8").splitlines()
                       if line.strip() and not line.lstrip().startswith("#"))
    new_lines = sorted(line.strip() for line in new.decode("utf-8").splitlines()
                       if line.strip() and not line.lstrip().startswith("#"))
    kind = ("VALUE_CHANGE" if fields else "BYTE_IDENTICAL" if old == new else
            "KEY_ORDER_ONLY" if old_lines == new_lines else "SERIALIZATION_ONLY")
    records.append({"path": relative, "old_sha256": hashlib.sha256(old).hexdigest(),
                    "new_sha256": hashlib.sha256(new).hexdigest(),
                    "classification": kind, "changed_field_paths": fields})
    if fields:
        raise ValueError("undeclared model value change at " + ", ".join(fields))


def _correct_coefficient_axes(model: dict[str, Any], axial_drag: float = 0.008) -> None:
    """Normalize only the known positional defect for a strict comparison.

    LineTypeBuilder.DEFAULT_HYDRO_COEFFS names n as normal and z as axial.
    With IncludeTorsion=No, n applies equally to local x and y. No other
    value, torsion setting or model field is allowed to change.
    """
    if not model["LineTypes"] or not model["Lines"] or any(
        line["IncludeTorsion"] != "No" for line in model["Lines"]
    ):
        raise ValueError("coefficient correction requires torsion disabled")
    expected = {"Cdn": 1.2, "Cdz": axial_drag, "Can": 1.0, "Caz": 0}
    for line_type in model["LineTypes"]:
        if (line_type.get("Cd") != [1.2, axial_drag] or line_type.get("Ca") != [1.0, 0]
                or expected.keys() & line_type.keys()):
            raise ValueError("reference coefficients differ from the known positional defect")
        del line_type["Cd"], line_type["Ca"]
        line_type.update(expected)


def _compare_frozen_models(old_model: dict[str, Any], new_model: dict[str, Any],
                           coefficient_axes: bool) -> tuple[list, list]:
    """Compare physical fields and preserve an already counted reference mesh."""
    counted_reference = COUNT_TABLE in old_model["Lines"][0]
    if coefficient_axes and not counted_reference:
        raise ValueError("coefficient correction requires a counted reference mesh")
    original_sections = old_model["Lines"][0].pop(COUNT_TABLE if counted_reference else TARGET_TABLE)
    sections = new_model["Lines"][0].pop(COUNT_TABLE)
    if counted_reference and original_sections != sections:
        raise ValueError("counted reference mesh changed")
    if coefficient_axes:
        _correct_coefficient_axes(old_model)
    if old_model != new_model:
        raise ValueError("model field outside the permitted correction changed")
    return original_sections, sections


def _entry(root: Path, relative: str) -> dict[str, Any]:
    payload = (root / relative).read_bytes()
    return {"path": relative, "sha256": hashlib.sha256(payload).hexdigest(), "bytes": len(payload)}


def _verified(root: Path, entry: dict[str, Any]) -> bytes:
    relative = Path(entry["path"])
    if relative.is_absolute() or ".." in relative.parts:
        raise ValueError("packet path must be relative")
    payload = (root / relative).read_bytes()
    if hashlib.sha256(payload).hexdigest() != entry["sha256"] or len(payload) != entry["bytes"]:
        raise ValueError(f"packet digest mismatch: {entry['path']}")
    return payload


def _material_spans(sections: list[list[Any]]) -> list[list[Any]]:
    spans = []
    station = 0.0
    for kind, length, _ in sections:
        if spans and spans[-1][0] == kind:
            spans[-1][2] = station + length
        else:
            spans.append([kind, station, station + length])
        station += length
    return spans


def _compare_original_inputs(reference: Path, candidate: Path, before: list[dict[str, Any]],
                             after: list[dict[str, Any]], coefficient_axes: bool = False,
                             serialization_change: list[dict[str, Any]] | None = None) -> int:
    """Allow only the original matrix's named-axis replacement, when requested."""
    if len(before) != len(after) or (
        not coefficient_axes and serialization_change is None and before != after
    ):
        raise ValueError("original comparison index changed")
    count = 0
    for old_case, new_case in zip(before, after):
        if ({key: value for key, value in old_case.items() if key != "inputs"}
                != {key: value for key, value in new_case.items() if key != "inputs"}
                or len(old_case["inputs"]) != len(new_case["inputs"])):
            raise ValueError("original comparison case or coverage changed")
        if _verified(reference, old_case["geometry"]) != _verified(candidate, new_case["geometry"]):
            raise ValueError("original comparison geometry changed")
        for old_input, new_input in zip(old_case["inputs"], new_case["inputs"]):
            if ({key: value for key, value in old_input.items() if key not in ("sha256", "bytes")}
                    != {key: value for key, value in new_input.items() if key not in ("sha256", "bytes")}):
                raise ValueError("original comparison input metadata changed")
            old_bytes, new_bytes = _verified(reference, old_input), _verified(candidate, new_input)
            old_model, new_model = yaml.safe_load(old_bytes), yaml.safe_load(new_bytes)
            if coefficient_axes:
                _correct_coefficient_axes(old_model, axial_drag=0)
                if _changed_fields(old_model, new_model):
                    raise ValueError("original model field outside the coefficient correction changed")
            if serialization_change is not None:
                _serialization_record(old_bytes, new_bytes, old_input["path"],
                                      serialization_change, old_model, new_model)
            elif not coefficient_axes and old_bytes != new_bytes:
                raise ValueError("original comparison bytes changed")
            count += 1
    return count


def mesh_statistics(sections: list[list[Any]]) -> dict[str, Any]:
    """Check actual section boundary elements with an unrounded 2.0 limit."""
    if not sections:
        raise ValueError("mesh is empty")
    sizes = []
    for _, length, count in sections:
        if not math.isfinite(length) or length <= 0 or type(count) is not int or count <= 0:
            raise ValueError("mesh requires positive lengths and integer counts")
        sizes.append(length / count)
    # Within a section all cells have the same size, so only joins can govern.
    maximum = max([1.0, *[max(a / b, b / a) for a, b in zip(sizes, sizes[1:])]])
    if maximum > 2.0:
        raise ValueError("adjacent element ratio exceeds 2.0")
    return {"sections": len(sections), "elements": sum(row[2] for row in sections),
            "maximum_adjacent_ratio_bound": math.ceil(maximum * 10) / 10,
            "unrounded_adjacent_ratio_pass": True}


def audit(reference: Path, candidate: Path, reference_revision: str,
          coefficient_axes: bool = False, original_coefficient_axes: bool = False,
          history_from: Path | None = None, history_revision: str | None = None,
          serialization_change: bool = False) -> dict[str, Any]:
    """Verify source ancestry, serialized meshes and physical preservation."""
    if (history_from is None) != (history_revision is None):
        raise ValueError("history requires both a packet and its recorded revision")
    retained_history = (_history(history_from, history_revision)
                        if history_from is not None and history_revision is not None else None)
    repo = CAMPAIGN.parents[1]
    old = json.loads((reference / "index.json").read_bytes())
    new = json.loads((candidate / "index.json").read_bytes())
    before, after = old["frozen_tranche"], new["frozen_tranche"]
    runtime = json.loads(_verified(candidate, new["producing_runtime"]))
    if not all(runtime.get(key) for key in ("python", "implementation", "system", "architecture", "libraries")):
        raise ValueError("candidate producing runtime is incomplete")
    if not all(runtime["libraries"].get(name) for name in (
            "numpy", "scipy", "PyYAML", "pydantic", "pydantic_core", "jsonschema")):
        raise ValueError("candidate numerical library versions are incomplete")
    _verify_reference_sources(reference, before["code_sources"], reference_revision)
    for key in ("manifest", "schema", "basis"):
        if _verified(reference, old[key]) != _verified(candidate, new[key]):
            raise ValueError(f"physical snapshot changed: {key}")
    for key in ("scope_sha256", "criteria_sha256", "written_counts", "phase_a_required", "pending_governing_qa"):
        if before[key] != after[key]:
            raise ValueError(f"frozen scope or counts changed: {key}")
    for key in ("snapshots", "load_ledger"):
        left = before[key] if isinstance(before[key], list) else [before[key]]
        right = after[key] if isinstance(after[key], list) else [after[key]]
        if left != right:
            raise ValueError(f"frozen snapshot index changed: {key}")
        for a, b in zip(left, right):
            if _verified(reference, a) != _verified(candidate, b):
                raise ValueError(f"frozen snapshot bytes changed: {key}")
    serialization_records = [] if serialization_change else None
    original_inputs = _compare_original_inputs(
        reference, candidate, old["cases"], new["cases"], original_coefficient_axes,
        serialization_records)
    if [case["case_id"] for case in before["cases"]] != [case["case_id"] for case in after["cases"]]:
        raise ValueError("frozen case order changed")
    inputs, audits = [], []
    for a, b in zip(before["cases"], after["cases"]):
        for key in ("geometry", "section"):
            if _verified(reference, a[key]) != _verified(candidate, b[key]):
                raise ValueError(f"frozen physical bytes changed: {key}")
        if len(a["inputs"]) != len(b["inputs"]):
            raise ValueError("frozen load count changed")
        mesh = None
        for old_input, new_input in zip(a["inputs"], b["inputs"]):
            metadata = lambda item: {key: value for key, value in item.items() if key not in ("sha256", "bytes")}
            if metadata(old_input) != metadata(new_input):
                raise ValueError("frozen load metadata changed")
            old_bytes, new_bytes = _verified(reference, old_input), _verified(candidate, new_input)
            old_model, new_model = yaml.safe_load(old_bytes), yaml.safe_load(new_bytes)
            if serialization_records is not None:
                normalized = copy.deepcopy(old_model)
                if coefficient_axes:
                    _correct_coefficient_axes(normalized)
                _serialization_record(old_bytes, new_bytes, old_input["path"],
                                      serialization_records, normalized, new_model)
            original_sections, sections = _compare_frozen_models(old_model, new_model, coefficient_axes)
            statistics = mesh_statistics(sections)
            old_spans, new_spans = _material_spans(original_sections), _material_spans(sections)
            if len(old_spans) != len(new_spans) or any(
                a[0] != b[0] or any(abs(x - y) > 1e-9 for x, y in zip(a[1:], b[1:]))
                for a, b in zip(old_spans, new_spans)
            ):
                raise ValueError("installed length or buoyancy material arcs changed")
            if mesh is not None and mesh != sections:
                raise ValueError("mesh differs across loads of the same configuration")
            mesh = sections
            inputs.append({key: new_input[key] for key in (
                "id", "case_id", "path", "sha256", "bytes", "kind", "qualification")})
        audits.append({"case_id": b["case_id"], **statistics,
                       "physical_model_matches": len(b["inputs"]),
                       "geometry": b["geometry"], "section": b["section"]})
    if len(audits) != 12 or len(inputs) != 144 or original_inputs != 288:
        raise ValueError("campaign coverage differs from the frozen plan")
    source_paths = [source["path"].removeprefix("source/") for source in after["code_sources"]]
    source_paths += ["scripts/riser_campaign/audit_mesh.py", "tests/riser_campaign/test_mesh.py",
                     "tests/riser_campaign/test_frozen_scope.py", "tests/riser_campaign/test_checkout.py",
                     "examples/riser-campaign/README.md", ".gitattributes",
                     "src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py",
                     "src/digitalmodel/solvers/orcaflex/modular_generator/schema/riser.py",
                     "tests/riser_campaign/test_campaign.py"]
    source_paths += ["tests/riser_campaign/fixtures/" + name for name in (
        "prior-packet.json", "original-packet.json", "reissue-packet.json", "reviewed-reference.tar.xz")]
    for entry in after["code_sources"]:
        if _verified(candidate, entry) != (repo / entry["path"].removeprefix("source/")).read_bytes():
            raise ValueError("candidate source snapshot differs from the working source")
    packet = {"summary": "Solver-free serialized mesh audit; native qualification remains required.",
            "status": "SOLVER_FREE_ONLY", "native_qualification": "NOT_RUN",
            "reference_revision": reference_revision, "adjacent_element_ratio_limit": 2.0,
            "reference_index": _entry(reference, "index.json"), "candidate_index": _entry(candidate, "index.json"),
            "producing_runtime": {"record": new["producing_runtime"], "versions": runtime},
            "audit_runtime": producing_runtime(),
            "source_files": [_entry(repo, path) for path in sorted(set(source_paths))],
            "case_order": [case["case_id"] for case in after["cases"]],
            "counts": {"written": after["written_counts"], "planned": after["phase_a_required"],
                       "pending": after["pending_governing_qa"]},
            "preservation": {("original_physical_model_matches" if original_coefficient_axes or serialization_change
                              else "original_input_byte_matches"): original_inputs,
                             "frozen_physical_model_matches": len(inputs),
                             "geometry_and_section_byte_matches": len(audits),
                             "scope_criteria_load_ledger_and_manifest": "BYTE_IDENTICAL"},
            "mesh_audits": audits, "inputs": inputs,
            "review_status": "INDEPENDENT_REVIEW_REQUIRED; any later byte change invalidates the packet",
            "native_handoff": "examples/riser-campaign/README.md#solver-free-mesh-audit-and-native-handoff"}
    if retained_history is not None:
        packet["historical_corrections"], packet["history"] = retained_history
    if serialization_records is not None:
        packet["serialization_change"] = {
            "inputs": serialization_records,
            "declared_corrections": {"coefficient_axes": coefficient_axes,
                                     "original_coefficient_axes": original_coefficient_axes},
        }
    if coefficient_axes:
        packet["coefficient_axis_correction"] = {
            "removed_fields": {"Cd": [1.2, 0.008], "Ca": [1.0, 0]},
            "named_fields": {"Cdn": 1.2, "Cdz": 0.008, "Can": 1.0, "Caz": 0},
            "axis_values": {"drag": {"x": 1.2, "y": 1.2, "z": 0.008},
                            "added_mass": {"x": 1.0, "y": 1.0, "z": 0}},
            "include_torsion": "No",
            "counted_mesh": "IDENTICAL",
            "all_other_model_fields": "IDENTICAL",
            "convention": "LineTypeBuilder.DEFAULT_HYDRO_COEFFS: n normal (x/y), z axial; RiserLine.include_torsion defaults to false",
        }
    if original_coefficient_axes:
        packet["original_coefficient_axis_correction"] = {
            "removed_fields": {"Cd": [1.2, 0], "Ca": [1.0, 0]},
            "named_fields": {"Cdn": 1.2, "Cdz": 0, "Can": 1.0, "Caz": 0},
            "include_torsion": "No", "geometry_bytes": "IDENTICAL",
            "all_other_model_fields": "IDENTICAL", "inputs": original_inputs,
        }
    return packet


def main() -> None:
    """Write a digest-bound audit without altering generated model inputs."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--reference-dir", type=Path, required=True)
    parser.add_argument("--candidate-dir", type=Path, required=True)
    parser.add_argument("--reference-revision", required=True)
    parser.add_argument("--coefficient-axes", action="store_true",
                        help="Compare counted meshes and allow only the known named-axis coefficient correction")
    parser.add_argument("--original-coefficient-axes", action="store_true",
                        help="Allow only the original comparison's named-axis replacement with zero axial coefficients")
    parser.add_argument("--history-from", type=Path,
                        help="Retain correction history from an exact committed review packet")
    parser.add_argument("--history-revision",
                        help="Git revision containing the previous review packet")
    parser.add_argument("--serialization-change", action="store_true",
                        help="Compare parsed inputs and bind both digests; reject undeclared value changes")
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    packet = audit(args.reference_dir, args.candidate_dir, args.reference_revision,
                   args.coefficient_axes, args.original_coefficient_axes,
                   args.history_from, args.history_revision, args.serialization_change)
    _write(args.output.parent, args.output.name, _json_bytes(packet))
    print("Solver-free audit passed: 12 configurations, 144 counted inputs; native qualification NOT_RUN")


if __name__ == "__main__":
    main()
