# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Inventory tracked riser YAML using repository evidence, without a solver."""
from __future__ import annotations

import argparse
from collections import Counter, defaultdict
import hashlib
import json
import os
from pathlib import Path
import subprocess

import yaml

ROOT = Path(__file__).resolve().parents[2]
BASE = "docs/domains/orcaflex/"
FOLDERS = ("production", "drilling", "completion", "intervention", "disconnect")
JSON_OUTPUT = BASE + "risers/provenance-evidence.json"
MD_OUTPUT = BASE + "risers/provenance-evidence.md"
LOADER = getattr(yaml, "CSafeLoader", yaml.SafeLoader)
WITHHELD = "withheld: unreviewed source text; locator and digest retained"


def digest(raw):
    """Return the SHA-256 of the exact supplied bytes."""
    return hashlib.sha256(raw).hexdigest()


def git(root, *args):
    """Read Git with argument arrays and no inherited repository overrides."""
    env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    return subprocess.run(["git", "-C", str(root), *args], check=True, env=env,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE).stdout


def paths(root, revision, prefixes):
    """Enumerate tracked YAML blobs at the requested revision, NUL-delimited."""
    entries = git(root, "ls-tree", "-r", "-z", revision, "--", *prefixes).split(b"\0")
    result = []
    for entry in filter(None, entries):
        metadata, path = entry.split(b"\t", 1)
        mode, kind, _ = metadata.split()
        if path.lower().endswith((b".yml", b".yaml")):
            if kind != b"blob" or mode not in (b"100644", b"100755"):
                raise ValueError("YAML candidate is not a regular tracked blob")
            result.append(path.decode("utf-8", "surrogateescape"))
    return sorted(result)


def normalize(raw):
    """Ignore comments and map order; retain tags, scalar values and list order.

    Reject duplicate or Python-colliding keys, cycles, complex keys, merge keys
    and unsupported tags. No include references or model code are evaluated.
    """
    loader = None
    active = set()

    def canonical(node):
        if id(node) in active:
            raise ValueError("alias cycle")
        active.add(id(node))
        try:
            if isinstance(node, yaml.ScalarNode):
                if node.tag.rsplit(":", 1)[-1] not in {
                    "str", "int", "float", "bool", "null", "timestamp"
                } or not node.tag.startswith("tag:yaml.org,2002:"):
                    raise ValueError("unsupported scalar tag")
                value = loader.construct_object(node)
                if isinstance(value, float):
                    value = value.hex()
                elif node.tag.endswith(":timestamp"):
                    value = value.isoformat()
                return [node.tag, value]
            if isinstance(node, yaml.SequenceNode) and node.tag == "tag:yaml.org,2002:seq":
                return [node.tag, [canonical(n) for n in node.value]]
            if isinstance(node, yaml.MappingNode) and node.tag == "tag:yaml.org,2002:map":
                pairs, keys, semantic_keys = [], set(), set()
                for key, value in node.value:
                    if not isinstance(key, yaml.ScalarNode):
                        raise ValueError("complex mapping key")
                    item = canonical(key)
                    token = json.dumps(item, separators=(",", ":"))
                    semantic = loader.construct_object(key)
                    if token in keys or semantic in semantic_keys:
                        raise ValueError("duplicate or colliding mapping key")
                    keys.add(token)
                    semantic_keys.add(semantic)
                    pairs.append([item, canonical(value)])
                return [node.tag, sorted(pairs, key=lambda p: json.dumps(p[0], separators=(",", ":"))) ]
            raise ValueError("unsupported YAML node")
        finally:
            active.remove(id(node))

    try:
        loader = LOADER(raw)
        node = loader.get_single_node()
        if node is None:
            raise ValueError("empty document")
        encoded = json.dumps(canonical(node), ensure_ascii=True, separators=(",", ":"))
        return {"status": "available", "sha256": digest(encoded.encode())}
    except (yaml.YAMLError, ValueError, TypeError, UnicodeError, RecursionError, OverflowError):
        return {"status": "unavailable: invalid or unsupported YAML", "sha256": None}
    finally:
        if loader is not None:
            loader.dispose()


def text_evidence(raw):
    """Locate source-bearing text without copying potentially identifying prose."""
    text = raw.decode("utf-8-sig", "replace")
    lines, evidence = text.splitlines(keepends=True), []
    start = None
    for index, line in enumerate(lines):
        if line.lstrip().startswith("#"):
            start = index if start is None else start
        elif not line.strip() or line.startswith(("%", "---")):
            if start is not None:
                break
        else:
            break
    if start is not None:
        last = index if not lines[index].lstrip().startswith("#") else index + 1
        evidence.append({"locator": f"lines {start + 1}-{last}", "status": WITHHELD,
                         "sha256": digest("".join(lines[start:last]).encode())})
    try:
        node = yaml.compose(text, Loader=LOADER)
        if isinstance(node, yaml.MappingNode):
            for key, value in node.value:
                if key.value in ("General", "Title"):
                    evidence.append({"locator": key.value, "status": WITHHELD,
                                     "sha256": digest(text[value.start_mark.index:value.end_mark.index].encode())})
        return evidence, "located and withheld" if evidence else "absent"
    except (yaml.YAMLError, ValueError, RecursionError):
        return evidence, "unavailable: YAML text locators could not be parsed"


def first_appearance(root, revision, path):
    """Record first reachable addition, without interpreting import as authorship."""
    history = git(root, "log", "--reverse", "--topo-order", "--diff-filter=A",
                  "--format=%H%x00%cI%x00%s", revision, "--", path).splitlines()
    if not history:
        raise ValueError("first reachable appearance is missing")
    commit, date, subject = history[0].split(b"\0", 2)
    return {"basis": "first reachable appearance", "commit": commit.decode("ascii"),
            "date": date.decode("ascii"), "subject": {"locator": "commit subject",
            "sha256": digest(subject), "status": WITHHELD}}


def build(root=ROOT, revision="HEAD"):
    """Build deterministic records from one pinned Git revision, without writes."""
    if git(root, "rev-parse", "--is-shallow-repository").strip() != b"false":
        raise ValueError("shallow history cannot establish first reachable appearance")
    revision = git(root, "rev-parse", "--verify", revision + "^{commit}").decode().strip()
    candidates = paths(root, revision, [BASE + "risers/" + name for name in FOLDERS])
    indexes, normalization_cache = {}, {}

    def normalization_for(raw):
        key = digest(raw)
        if key not in normalization_cache:
            normalization_cache[key] = normalize(raw)
        return normalization_cache[key]

    for group in ("examples", "library"):
        raw_index, normalized_index = defaultdict(list), defaultdict(list)
        unavailable = 0
        for path in paths(root, revision, [BASE + group]):
            raw = git(root, "show", revision + ":" + path)
            norm = normalization_for(raw)
            raw_index[digest(raw)].append(path)
            if norm["sha256"] is not None:
                normalized_index[norm["sha256"]].append(path)
            else:
                unavailable += 1
        indexes[group] = (raw_index, normalized_index, unavailable)
    records = []
    for path in candidates:
        raw = git(root, "show", revision + ":" + path)
        norm = normalization_for(raw)
        text, text_status = text_evidence(raw)
        row = {"path": path, "bytes": len(raw), "sha256": digest(raw),
               "normalized_sha256": norm["sha256"], "normalization_status": norm["status"],
               "first_appearance": first_appearance(root, revision, path),
               "source_text": text, "source_text_status": text_status}
        # Syntax is a file-kind hint, not an assertion of model completeness.
        row["file_kind"] = ("model YAML" if b"General:" in raw else
                            "analysis configuration" if b"basename:" in raw else "YAML fragment or configuration")
        for group, field in (("examples", "example_matches"), ("library", "library_matches")):
            raw_index, norm_index, _ = indexes[group]
            byte = set(raw_index.get(row["sha256"], []))
            normalized = set(norm_index.get(norm["sha256"], [])) if norm["sha256"] else set()
            row[field] = [{"path": p, "kind": "byte and normalized" if p in byte & normalized
                           else "byte" if p in byte else "normalized"} for p in sorted(byte | normalized)]
        row["proposed_class"] = "matches public example" if row["example_matches"] else "origin not established"
        row["missing_evidence"] = ("upstream authorship and permitted reuse are not established" if row["example_matches"]
                                   else "no public-example identity or positive repository-authorship evidence")
        records.append(row)
    return {"schema": "orcaflex-riser-provenance-evidence-v1", "records": records,
            "comparison_normalization_unavailable": {g: indexes[g][2] for g in indexes},
            "input_sha256": digest(json.dumps([(r["path"], r["sha256"]) for r in records],
                                                ensure_ascii=True, separators=(",", ":")).encode())}


def render(data):
    """Render compact machine records and the per-file evidence report."""
    records = data["records"]
    counts = Counter(r["proposed_class"] for r in records)
    failures = sum(r["normalized_sha256"] is None for r in records)
    body = ["# OrcaFlex riser provenance evidence", "", "## Summary", "",
            f"The tracked inventory contains {len(records)} YAML files. "
            f"Public-example identity matches: {counts['matches public example']}. "
            f"Origin not established: {counts['origin not established']}. "
            f"Normalization unavailable: {failures}.", "",
            "Identity is file evidence; upstream authorship and permitted reuse remain unestablished. "
            "The existing provenance declarations are unchanged.", "",
            "## Revision history", "", "| Revision | Date | Change and reason |",
            "| --- | --- | --- |", "| 1 | 2026-11-16 | Record repository evidence for origin review. |", "",
            "Table 1. Evidence report issue history.", "", "## Evidence basis", "",
            "Only tracked YAML blobs at the requested revision are compared. Earlier blob versions are not compared. "
            "First reachable addition commits describe repository appearance, not original creation. "
            "Shallow history and missing blobs stop the build. The JSON records contain full SHA-256 values, "
            "byte sizes, first-appearance dates and commits, source-text locators, and all matching paths.", "",
            "Normalization uses PyYAML scalar tags and constructed values, tagged sequences in order, "
            "and tagged mappings sorted by canonical keys. Compact ASCII JSON is hashed with SHA-256. "
            "Comments and mapping order are ignored; types and scalar values are retained. Floats use hexadecimal "
            "values and timestamps use ISO text. Duplicate or Python-colliding keys, alias cycles, complex keys, "
            "merge keys, unsupported tags and malformed documents deny normalized matching. Raw matching remains available.", "",
            "Library matches are duplication evidence only. No positive repository-authorship evidence was established; "
            "the generic-template class is therefore unused. The model/configuration distinction is a syntax hint. "
            "No include is followed, model code evaluated, geometry inferred or solver invoked.", "",
            "All source text and commit subjects are withheld as unreviewed free text; locators and SHA-256 "
            "digests distinguish withheld from absent evidence. Text digests use decoded UTF-8 with replacement "
            "and re-encoding; the raw blob digest remains the byte identity. No source statement is interpreted as permission.", "",
            "Comparison files without normalized evidence: " + ", ".join(
                f"{g}: {n}" for g, n in data["comparison_normalization_unavailable"].items()) + ".", "",
            "## Per-file evidence", "", "| File | Bytes | First appearance | Proposed class | Examples / library |",
            "| --- | ---: | --- | --- | ---: |"]
    for row in records:
        name = json.dumps(row["path"].removeprefix(BASE + "risers/"), ensure_ascii=True)[1:-1]
        name = name.replace("|", "&#124;").replace("`", "&#96;")
        first = row["first_appearance"]
        body.append(f"| `{name}` | {row['bytes']} | {first['date'][:10]} `{first['commit'][:12]}` | "
                    f"{row['proposed_class']} | {len(row['example_matches'])} / {len(row['library_matches'])} |")
    body += ["", "Table 2. One row per tracked YAML file; full reproducible evidence is in `provenance-evidence.json`.", "",
             "## Known limits and way forward", "",
             "A lack of a match does not establish independent authorship. Normalization failures lower evidence "
             "coverage; they do not establish inequality. Unreviewed statements require a separate origin review. "
             "The maintainers should obtain upstream origin and reuse terms before considering reuse. "
             "This inventory supplies no engineering result or physical-case elevation schematic.", "",
             "## Reproduce", "", "```sh",
             "uv run --no-project --with pytest --with pyyaml python -m pytest tests/riser_capability/test_orcaflex_riser_provenance_evidence.py tests/riser_capability/test_riser_inventory.py -q -p no:cacheprovider",
             "uv run --no-project --with pyyaml python scripts/riser_capability/build_orcaflex_riser_provenance_evidence.py --revision HEAD --check",
             "```", "", "Omit `--check` to regenerate both outputs; `--revision` selects a reachable commit. "
             "The input digest binds sorted candidate paths and raw digests without embedding a changing report commit.", "",
             "## Run-it-yourself prompt", "", "Rebuild the repository-only riser provenance inventory, reproduce every "
             "byte or normalized match from the recorded paths, and report missing evidence without changing models or origin declarations.", ""]
    compact = [json.dumps(r, ensure_ascii=True, separators=(",", ":")) for r in records]
    metadata = {k: v for k, v in data.items() if k != "records"}
    prefix = json.dumps(metadata, ensure_ascii=True, separators=(",", ":"))[:-1]
    return {JSON_OUTPUT: prefix + ',"records":[\n' + ",\n".join(compact) + "\n]}\n",
            MD_OUTPUT: "\n".join(body)}


def write_outputs(root, data, check=False):
    """Compare without writes in check mode, or write the deterministic outputs."""
    root = Path(root).resolve()
    outputs = render(data)
    for relative in outputs:
        path = root / relative
        if path.is_symlink() or not path.resolve().is_relative_to(root):
            raise ValueError("output path leaves the requested repository")
    valid = True
    for relative, text in outputs.items():
        path = root / relative
        expected = text.encode("utf-8")
        if check:
            valid = valid and path.is_file() and path.read_bytes() == expected
        else:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(expected)
    return valid


def main():
    """Build evidence at an explicit revision and fail on output drift."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--revision", default="HEAD")
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    data = build(args.root, args.revision)
    if not write_outputs(args.root, data, check=args.check):
        parser.exit(1, "provenance evidence outputs are missing or differ\n")
    print(f"provenance evidence: {len(data['records'])} records; outputs {'match' if args.check else 'written'}")


if __name__ == "__main__":
    main()
