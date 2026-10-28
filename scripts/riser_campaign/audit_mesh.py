# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Audit generated frozen mesh bytes against a reviewed reference packet.

This reads serialized inputs only. It does not load or qualify a native model.
"""

import argparse
import hashlib
import json
import math
from pathlib import Path
import subprocess
from typing import Any

import yaml

from .generate_inputs import CAMPAIGN, _json_bytes, _write, producing_runtime

COUNT_TABLE = "LineType, Length, NumberOfSegments"
TARGET_TABLE = "LineType, Length, TargetSegmentLength"


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


def audit(reference: Path, candidate: Path, reference_revision: str) -> dict[str, Any]:
    """Verify source ancestry, serialized meshes and physical preservation."""
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
    for source in before["code_sources"]:
        committed = subprocess.run(
            ["git", "show", f"{reference_revision}:{source['path'].removeprefix('source/')}"],
            cwd=repo, check=True, capture_output=True,
        ).stdout
        if _verified(reference, source) != committed:
            raise ValueError("reference source does not match the reviewed revision")
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
    if old["cases"] != new["cases"]:
        raise ValueError("original comparison index changed")
    original_inputs = 0
    for case in new["cases"]:
        for entry in [case["geometry"], *case["inputs"]]:
            if _verified(reference, entry) != _verified(candidate, entry):
                raise ValueError("original comparison bytes changed")
        original_inputs += len(case["inputs"])
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
            old_model = yaml.safe_load(_verified(reference, old_input))
            new_model = yaml.safe_load(_verified(candidate, new_input))
            original_sections = old_model["Lines"][0].pop(TARGET_TABLE)
            sections = new_model["Lines"][0].pop(COUNT_TABLE)
            if old_model != new_model:
                raise ValueError("model field outside the mesh changed")
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
                     "examples/riser-campaign/README.md", ".gitattributes"]
    for entry in after["code_sources"]:
        if _verified(candidate, entry) != (repo / entry["path"].removeprefix("source/")).read_bytes():
            raise ValueError("candidate source snapshot differs from the working source")
    return {"summary": "Solver-free serialized mesh audit; native qualification remains required.",
            "status": "SOLVER_FREE_ONLY", "native_qualification": "NOT_RUN",
            "reference_revision": reference_revision, "adjacent_element_ratio_limit": 2.0,
            "reference_index": _entry(reference, "index.json"), "candidate_index": _entry(candidate, "index.json"),
            "producing_runtime": {"record": new["producing_runtime"], "versions": runtime},
            "audit_runtime": producing_runtime(),
            "source_files": [_entry(repo, path) for path in sorted(set(source_paths))],
            "case_order": [case["case_id"] for case in after["cases"]],
            "counts": {"written": after["written_counts"], "planned": after["phase_a_required"],
                       "pending": after["pending_governing_qa"]},
            "preservation": {"original_input_byte_matches": original_inputs,
                             "frozen_physical_model_matches": len(inputs),
                             "geometry_and_section_byte_matches": len(audits),
                             "scope_criteria_load_ledger_and_manifest": "BYTE_IDENTICAL"},
            "mesh_audits": audits, "inputs": inputs,
            "review_status": "INDEPENDENT_REVIEW_REQUIRED; any later byte change invalidates the packet",
            "native_handoff": "examples/riser-campaign/README.md#solver-free-mesh-audit-and-native-handoff"}


def main() -> None:
    """Write a digest-bound audit without altering generated model inputs."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--reference-dir", type=Path, required=True)
    parser.add_argument("--candidate-dir", type=Path, required=True)
    parser.add_argument("--reference-revision", required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    packet = audit(args.reference_dir, args.candidate_dir, args.reference_revision)
    _write(args.output.parent, args.output.name, _json_bytes(packet))
    print("Solver-free audit passed: 12 configurations, 144 counted inputs; native qualification NOT_RUN")


if __name__ == "__main__":
    main()
