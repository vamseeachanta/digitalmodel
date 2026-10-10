"""Exact postprocessing coverage audit and append-only local archive preparation."""

import csv
import gzip
import hashlib
import json
from pathlib import Path
import shutil
from datetime import datetime, timezone

import numpy as np


def sha256(path):
    digest = hashlib.sha256()
    with Path(path).open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _equal(left, right, context):
    a, b = np.asarray(left, dtype=np.float64), np.asarray(right, dtype=np.float64)
    if a.shape != b.shape or not np.isfinite(a).all() or not np.isfinite(b).all():
        raise ValueError(f"Nonfinite or mismatched shape: {context}")
    if not np.array_equal(a, b):
        raise ValueError(f"Numerical data differs: {context}")


def _csv(path):
    with path.open(newline="", encoding="utf-8-sig") as stream:
        rows = list(csv.reader(stream))
    if len(rows) < 2:
        raise ValueError(f"Empty CSV: {path}")
    return rows[0], np.asarray(rows[1:], dtype=np.float64)


def _identity(meta):
    parts = [meta.get("object"), meta.get("variable")]
    if meta.get("position") is not None:
        parts.append(meta["position"])
    return " / ".join(map(str, parts)), meta.get("units")


def _verify_time(item, context):
    label, units = item["label"], item["units"]
    matches = [key for key, meta in context["metadata"]["channels"].items()
               if _identity(meta) == (label, units)]
    if len(matches) != 1:
        raise ValueError(f"Missing/ambiguous time-series identity: {label} [{units}]")
    key = matches[0]
    if key not in context["arrays"]:
        raise ValueError(f"NPZ missing channel {key}")
    _equal(item["time"], context["arrays"]["time"], label + " time")
    _equal(item["values"], context["arrays"][key], label + " values")
    return key


def _verify_range(item, context):
    matches = [(i, graph) for i, graph in enumerate(context["extraction"]["range_graphs"])
               if (graph["label"], graph["units"]) == (item["label"], item["units"])]
    if len(matches) != 1:
        raise ValueError(f"Missing/ambiguous range identity: {item['label']}")
    index, graph = matches[0]
    for field in ["X", "Min", "Max", "Mean"]:
        _equal(item[field], graph[field], item["label"] + " " + field)
    return f"extracted/range_{index:03d}.csv"


def _compact(value, context):
    if isinstance(value, list):
        return [_compact(item, context) for item in value]
    if not isinstance(value, dict):
        return value
    if {"label", "units", "time", "values"} <= value.keys():
        key = _verify_time(value, context)
        context["json_time_series_count"] += 1
        return {**{k: v for k, v in value.items() if k not in {"time", "values"}},
                "retained_channel": key, "retained_file": "installation_traces/traces.npz",
                "retained_time_channel": "time"}
    if {"label", "units", "X", "Min", "Max", "Mean"} <= value.keys():
        path = _verify_range(value, context)
        context["json_range_series_count"] += 1
        return {**{k: v for k, v in value.items() if k not in {"X", "Min", "Max", "Mean"}},
                "retained_file": path}
    return {key: _compact(item, context) for key, item in value.items()}


def _verify_csvs(run, context):
    expected = {"results/cases.csv"}
    extraction = context["extraction"]
    for index, item in enumerate(extraction["time_histories"]):
        name = f"extracted/time_{index:03d}.csv"
        expected.add(name)
        headers, values = _csv(run / name)
        if headers != ["time_s", f"{item['label']} [{item['units']}]"]:
            raise ValueError(f"Time CSV identity/units differ: {name}")
        _equal(values[:, 0], item["time"], name + " time")
        _equal(values[:, 1], item["values"], name + " values")
        _verify_time(item, context)
    for index, item in enumerate(extraction["range_graphs"]):
        name = f"extracted/range_{index:03d}.csv"
        expected.add(name)
        headers, values = _csv(run / name)
        wanted = ["arc_length_m"] + [f"{key} [{item['units']}]" for key in ["Min", "Max", "Mean"]]
        if headers != wanted:
            raise ValueError(f"Range CSV identity/units differ: {name}")
        for col, key in enumerate(["X", "Min", "Max", "Mean"]):
            _equal(values[:, col], item[key], name + " " + key)
    actual = {p.relative_to(run).as_posix() for p in run.rglob("*.csv")}
    if actual - expected:
        raise ValueError(f"Unclassified CSV files: {sorted(actual - expected)}")


def audit_run(run_dir):
    """Verify every CSV and recognized JSON history/range against retained data."""
    run = Path(run_dir).resolve(strict=True)
    identity = run / "run.json"
    run_id = json.loads(identity.read_bytes()).get("run_id") if identity.is_file() else None
    if not isinstance(run_id, str) or not run_id.strip():
        raise ValueError("Missing run identity in run.json")
    for path in run.rglob("*"):
        if not path.resolve(strict=True).is_relative_to(run):
            raise ValueError(f"Inner source path escapes run: {path}")
    npz_path = run / "installation_traces/traces.npz"
    metadata = json.loads((run / "installation_traces/metadata.json").read_bytes())
    if metadata.get("trace_sha256") != sha256(npz_path):
        raise ValueError("Trace digest differs from metadata")
    with np.load(npz_path, allow_pickle=False) as stored:
        arrays = {key: stored[key] for key in stored.files}
    if "time" not in arrays or set(arrays) - {"time"} != set(metadata["channels"]):
        raise ValueError("NPZ and metadata channel inventories differ")
    if arrays["time"].ndim != 1 or len(arrays["time"]) < 2 or not np.all(np.diff(arrays["time"]) > 0):
        raise ValueError("Time samples must increase strictly")
    for key, values in arrays.items():
        _equal(values, values, key)
        if values.shape != arrays["time"].shape:
            raise ValueError(f"NPZ channel/time lengths differ: {key}")
    extraction = json.loads((run / "extracted/extraction.json").read_bytes())
    context = dict(metadata=metadata, arrays=arrays, extraction=extraction,
                   json_time_series_count=0, json_range_series_count=0)
    _verify_csvs(run, context)
    records, sources = {}, []
    for path in sorted(run.rglob("*.json")):
        relative = path.relative_to(run).as_posix()
        raw = path.read_bytes()
        records[relative] = _compact(json.loads(raw), context)
        sources.append(dict(path=relative, bytes=len(raw), sha256=hashlib.sha256(raw).hexdigest()))
    files = ["installation_traces/traces.npz", "installation_traces/metadata.json"]
    files += [f"extracted/range_{i:03d}.csv" for i in range(len(extraction["range_graphs"]))]
    files += [p.relative_to(run).as_posix() for p in (run / "results").glob("*") if p.suffix in {".json", ".csv"}]
    return dict(time_csv_count=len(extraction["time_histories"]), range_csv_count=len(extraction["range_graphs"]),
                json_time_series_count=context["json_time_series_count"], json_range_series_count=context["json_range_series_count"],
                precision="exact float64 equality", npz_channels=len(metadata["channels"]),
                samples=len(arrays["time"]), compact_records=records, source_json=sources,
                source_csv=[dict(path=p.relative_to(run).as_posix(), bytes=p.stat().st_size, sha256=sha256(p)) for p in sorted(run.rglob("*.csv"))],
                files=[dict(path=p, bytes=(run / p).stat().st_size, sha256=sha256(run / p)) for p in sorted(set(files))])


def _inside(root, relative):
    if Path(relative).is_absolute() or Path(relative).drive or ".." in Path(relative).parts:
        raise ValueError(f"Source path escapes owning root: {relative}")
    path = (root / relative).resolve(strict=True)
    if path == root or not path.is_relative_to(root):
        raise ValueError(f"Source path escapes owning root: {relative}")
    return path


def _copy_run(source, target, entry, audit):
    relative = entry["path"]
    run, destination = source / relative, target / relative
    files = []
    for item in audit["files"]:
        src, dst = run / item["path"], destination / item["path"]
        dst.parent.mkdir(parents=True, exist_ok=True)
        if sha256(src) != item["sha256"]:
            raise ValueError(f"Source changed after audit: {src}")
        with src.open("rb") as inp, dst.open("xb") as out:
            shutil.copyfileobj(inp, out)
        if sha256(dst) != item["sha256"]:
            raise ValueError(f"Archive readback differs: {dst}")
        files.append(dict(item, path=f"{relative}/{item['path']}", derivation="byte_copy"))
    for item in audit["source_json"] + audit["source_csv"]:
        if sha256(run / item["path"]) != item["sha256"]:
            raise ValueError("Source changed after audit")
    raw = json.dumps(audit["compact_records"], separators=(",", ":"), allow_nan=False).encode()
    compact = destination / "compact-source-records.json.gz"
    with compact.open("xb") as stream:
        stream.write(gzip.compress(raw, mtime=0))
    if gzip.decompress(compact.read_bytes()) != raw:
        raise ValueError("Compact metadata readback differs")
    files.append(dict(path=compact.relative_to(target).as_posix(), bytes=compact.stat().st_size,
                      sha256=sha256(compact), derivation="verified-array-reference-substitution"))
    return files


def create_archive(source_root, entries, target, allowed_parent):
    """Copy to a new immutable-version directory; no source deletion or overwrite."""
    source = Path(source_root).resolve(strict=True)
    target = Path(target).resolve()
    parent = Path(allowed_parent).resolve()
    if target.exists():
        raise FileExistsError(target)
    if target == parent or not target.is_relative_to(parent) or target.is_relative_to(source):
        raise ValueError("Archive target outside allowed archive parent or inside source")
    resolved = [_inside(source, entry["path"]) for entry in entries]
    if len({p.as_posix().casefold() for p in resolved}) != len(entries) or not entries:
        raise ValueError("Run inventory empty or duplicated")
    audits = [(entry, audit_run(path)) for entry, path in zip(entries, resolved)]
    target.mkdir(parents=True, exist_ok=False)
    manifest = dict(version="lean-r1", owner="private digitalmodel-data", created_utc=datetime.now(timezone.utc).isoformat(),
                    archive_root=str(target), source_root=str(source), backup_status="NOT_VERIFIED",
                    immutability="Version path refuses overwrite; integrity hashes verified. Not WORM storage.",
                    source_disposition="UNCHANGED; no native simulations or inputs removed", run_count=len(entries), runs=[], files=[])
    for entry, audit in audits:
        manifest["files"].extend(_copy_run(source, target, entry, audit))
        manifest["runs"].append(dict(entry, coverage={k: v for k, v in audit.items() if k not in {"files", "compact_records"}},
                                    run_id=audit["compact_records"]["run.json"]["run_id"]))
    manifest["bytes"] = sum(item["bytes"] for item in manifest["files"])
    path = target / "manifest.json"
    with path.open("x", encoding="utf-8") as stream:
        json.dump(manifest, stream, indent=2, allow_nan=False)
    if json.loads(path.read_text()) != manifest:
        raise ValueError("Manifest readback differs")
    return manifest
