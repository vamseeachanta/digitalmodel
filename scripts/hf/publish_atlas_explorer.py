#!/usr/bin/env python3
"""Validate and publish the existing parametric atlas projection atomically.

Use --dry-run for offline validation. Publication also verifies remote file
hashes and datasets-server rows; failed indexing never reports readiness.
"""

import argparse
import hashlib
import io
import itertools
import json
import math
from pathlib import Path
import time
from tempfile import TemporaryDirectory
from urllib.parse import urlencode
from urllib.request import urlopen

import pandas as pd
import yaml

REPO_ID = "aceengineer/digitalmodel-atlas-explorer"
LICENSE = "cc-by-4.0"
CONFIGS = [
    ("mooring_fatigue", "mooring_fatigue"),
    ("synthetic_rope_mooring_fatigue", "synthetic_rope_mooring_fatigue"),
    ("code_check", "code_check"),
]


def resolve_grid(atlases_root, basename):
    """Resolve a local pointer without allowing traversal or external symlinks."""
    root = Path(atlases_root).resolve()
    base = (root / basename).resolve()
    if not base.is_relative_to(root):
        raise ValueError("atlas pointer escapes root")
    atlas_id = (base / "default.txt").read_text(encoding="utf-8").strip()
    if not atlas_id or atlas_id in {".", ".."} or "/" in atlas_id or "\\" in atlas_id:
        raise ValueError("invalid atlas pointer")
    folder = (base / atlas_id).resolve()
    for filename in ("grid.parquet", "manifest.yaml"):
        if not (folder / filename).resolve().is_relative_to(base):
            raise ValueError("atlas pointer escapes root")
    manifest = yaml.safe_load((folder / "manifest.yaml").read_bytes())
    return folder / "grid.parquet", manifest


def validate_grid(frame, manifest):
    """Check the declared Cartesian grid and nonnegative finite responses.

    Zero is the mathematical lower bound for damage/utilisation, not a design
    acceptance limit. The interpolation tolerance is supplied by the manifest.
    This checks publication integrity, not independent engineering qualification.
    """
    validation = manifest.get("validation") or {}
    error, limit = validation.get("max_rel_error"), validation.get("threshold")
    if (
        validation.get("passes") is not True
        or validation.get("metric") != "max_rel_error"
        or any(
            isinstance(v, bool)
            or not isinstance(v, (int, float))
            or not math.isfinite(v)
            or v < 0
            for v in (error, limit)
        )
        or error > limit
    ):
        raise ValueError("atlas validation is missing or failed")
    response = manifest.get("response")
    if response not in frame or frame.empty:
        raise ValueError("grid response missing or empty")
    values = pd.to_numeric(frame[response], errors="coerce")
    if not values.map(lambda v: math.isfinite(v) and v >= 0).all():
        raise ValueError("response must be finite and nonnegative")
    axes = manifest.get("axes") or []
    names = [axis["name"] for axis in axes]
    if not names or len(set(names)) != len(names) or not set(names).issubset(frame):
        raise ValueError("grid axes missing or duplicated")
    declared = [axis.get("grid", axis.get("values", [])) for axis in axes]
    expected = set(itertools.product(*declared))
    actual = list(frame[names].itertuples(index=False, name=None))
    if len(actual) != len(expected) or set(actual) != expected:
        raise ValueError("grid must contain exactly the declared Cartesian product")
    for column in frame.columns:
        if (
            not frame[column]
            .map(
                lambda v: (
                    isinstance(v, (str, bool, int, float))
                    and (not isinstance(v, (int, float)) or math.isfinite(v))
                )
            )
            .all()
        ):
            raise ValueError("grid contains non-scalar, null or non-finite values")


def prepare_entry(atlases_root, config_name, basename):
    """Freeze validated bytes, so later source changes cannot change the upload."""
    parquet, _ = resolve_grid(atlases_root, basename)
    sources = {
        name: (parquet.parent / name).read_bytes()
        for name in ("grid.parquet", "manifest.yaml")
    }
    manifest = yaml.safe_load(sources["manifest.yaml"])
    if manifest.get("atlas_id") != parquet.parent.name:
        raise ValueError("atlas pointer and manifest identity differ")
    frame = pd.read_parquet(io.BytesIO(sources["grid.parquet"]))
    validate_grid(frame, manifest)
    return {
        "name": config_name,
        "frame": frame,
        "manifest": manifest,
        "sources": sources,
        "hashes": {
            name: hashlib.sha256(data).hexdigest() for name, data in sources.items()
        },
    }


def std_str(manifest):
    """Return the source manifest's standard identifiers without inventing clauses."""
    standards = (manifest.get("provenance") or {}).get("standards") or []
    return (
        ", ".join(f"{s.get('id')} ({s.get('edition')})" for s in standards)
        or "NOT_EVALUATED"
    )


def build_card(entries):
    """Build viewer configs with exact source hashes and explicit citation gaps."""
    front = {
        "license": LICENSE,
        "pretty_name": "digitalmodel Parametric Atlas Explorer",
        "tags": ["engineering", "fatigue", "parametric", "synthetic"],
        "configs": [
            {
                "config_name": e["name"],
                "data_files": [
                    {"split": "train", "path": f"{e['name']}/train.parquet"}
                ],
            }
            for e in entries
        ],
    }
    lines = [
        "---",
        yaml.safe_dump(front, sort_keys=False).strip(),
        "---",
        "",
        "# Parametric Atlas Explorer",
        "",
        "Synthetic screening outputs from the existing parametric atlases. "
        "Publication integrity checks do not establish engineering qualification.",
        "",
        "Publication history is maintained in the repository's "
        "`docs/hf-atlas-publishing.md`.",
        "",
        "## Data basis",
        "",
        "schema_version: 1",
        "",
        "The original grid and manifest bytes are preserved. Null or non-finite "
        "responses are rejected. No columns are withheld. Field units are encoded "
        "in axis names where available; cycles and damage are dimensionless. "
        "Unqualified field units require the source workflow documentation.",
        "",
    ]
    for entry in entries:
        lines.extend(card_section(entry))
    return "\n".join(lines)


def card_section(entry):
    """Render one configuration's preserved provenance and qualification limits."""
    name, frame, manifest = entry["name"], entry["frame"], entry["manifest"]
    provenance = manifest.get("provenance") or {}
    citations = provenance.get("citations")
    lines = [
        f"## {name}",
        "",
        f"Rows: {len(frame)}; response: `{manifest['response']}`.",
        f"Columns: {', '.join(frame.columns)}.",
        f"Standards declared by source: {std_str(manifest)}.",
        "Interpolation validation: PASS against the source manifest's tolerance "
        "(a surrogate check, not independent physical validation).",
        "",
    ]
    for filename, digest in entry["hashes"].items():
        lines.append(f"- Source SHA-256 `{name}/{filename}`: `{digest}`")
    lines += [
        "",
        "Citation sidecars: "
        + (
            json.dumps(citations, sort_keys=True)
            if citations
            else "NOT_EVALUATED — source manifest supplies edition-only provenance; "
            "complete Citation sidecars and clause-level qualification remain required."
        ),
        "",
    ]
    return lines


def publication_files(entries):
    """Return the frozen payload; no files or caches are written locally."""
    files = {"README.md": build_card(entries).encode()}
    for entry in entries:
        name = entry["name"]
        files[f"{name}/train.parquet"] = entry["sources"]["grid.parquet"]
        files[f"{name}/manifest.yaml"] = entry["sources"]["manifest.yaml"]
    return files


def publish_entries(api, repo_id, entries):
    """Publish in one commit and compare downloaded bytes at that exact revision."""
    from huggingface_hub import CommitOperationAdd

    files = publication_files(entries)
    api.create_repo(repo_id, repo_type="dataset", private=False, exist_ok=True)
    api.update_repo_settings(repo_id=repo_id, repo_type="dataset", private=False)
    info = api.repo_info(repo_id=repo_id, repo_type="dataset")
    if info.private:
        raise ValueError("remote visibility remains private")
    commit = api.create_commit(
        repo_id=repo_id,
        repo_type="dataset",
        parent_commit=info.sha,
        operations=[
            CommitOperationAdd(path_in_repo=name, path_or_fileobj=data)
            for name, data in files.items()
        ],
        commit_message="Add validated atlas projection with source provenance",
    )
    with TemporaryDirectory(prefix="hf-readback-") as cache:
        for name, expected in files.items():
            downloaded = api.hf_hub_download(
                repo_id=repo_id,
                repo_type="dataset",
                filename=name,
                revision=commit.oid,
                cache_dir=cache,
            )
            if (
                hashlib.sha256(Path(downloaded).read_bytes()).digest()
                != hashlib.sha256(expected).digest()
            ):
                raise ValueError(f"remote digest mismatch: {name}")
    return commit.oid


def validate_rows(entry, payload):
    """Compare the viewer sample with the frozen local grid, including row count."""
    frame = entry["frame"]
    rows = payload.get("rows") or []
    if payload.get("num_rows_total") != len(frame) or len(rows) != min(len(frame), 100):
        raise ValueError("rows count differs from source")
    try:
        actual = pd.DataFrame([row["row"] for row in rows])
        pd.testing.assert_frame_equal(
            actual[frame.columns],
            frame.iloc[: len(actual)].reset_index(drop=True),
            check_dtype=False,
            check_exact=True,
        )
    except (AssertionError, KeyError, TypeError) as exc:
        raise ValueError("rows values differ from source") from exc


def verify_viewer(repo_id, entries, attempts=3, delay=10):
    """Retry indexing, bounded by caller-supplied operational retry settings."""
    for entry in entries:
        query = urlencode(
            {
                "dataset": repo_id,
                "config": entry["name"],
                "split": "train",
                "offset": 0,
                "length": min(len(entry["frame"]), 100),
            }
        )
        for attempt in range(attempts):
            try:
                with urlopen(
                    "https://datasets-server.huggingface.co/rows?" + query, timeout=30
                ) as response:
                    validate_rows(entry, json.load(response))
                break
            except (OSError, ValueError) as exc:
                if attempt + 1 == attempts:
                    raise RuntimeError(f"viewer NOT_READY: {entry['name']}") from exc
                time.sleep(delay)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--atlases-root",
        type=Path,
        default=Path(__file__).resolve().parents[2] / "atlases",
    )
    parser.add_argument("--repo-id", default=REPO_ID)
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args()
    entries = [prepare_entry(args.atlases_root, name, base) for name, base in CONFIGS]
    for entry in entries:
        print(f"{entry['name']}: {len(entry['frame'])} validated rows")
    if args.dry_run:
        print(build_card(entries))
        return
    from huggingface_hub import HfApi

    revision = publish_entries(HfApi(), args.repo_id, entries)
    print(f"Uploaded revision {revision}; checking viewer readiness")
    verify_viewer(args.repo_id, entries)
    print(f"READY: {args.repo_id} at {revision}")


if __name__ == "__main__":
    main()
