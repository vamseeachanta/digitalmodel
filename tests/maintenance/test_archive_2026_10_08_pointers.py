"""The 2026-10-08 archive move (owner decision D01) leaves consistent pointers behind.

Files moved out of git live in a content-addressed store on the archive host
(`/mnt/ace/digitalmodel/blobs/sha256/<aa>/<sha256>.<ext>`). The repository keeps the
blob map (original path -> blob), the per-blob SHA-256 manifest, the verification
record written on the archive host, `data/inputs.yaml`, `DOCUMENT-MAP.md` and a
`.gitignore` section. These tests keep the moved paths out of git and keep the
pointer files in agreement with each other.
"""
import re
import subprocess
from pathlib import Path

import pytest
import yaml

REPO = Path(__file__).resolve().parents[2]
ARCHIVE_DIR = REPO / "data" / "archive" / "2026-10-08"
BLOB_MAP = ARCHIVE_DIR / "MANIFEST.blob-map-2026-10-08.tsv"
SHA_MANIFEST = ARCHIVE_DIR / "MANIFEST.blobs-2026-10-08.sha256.tsv"
VERIFY = ARCHIVE_DIR / "VERIFY-blobs-2026-10-08.tsv"
BLOB_PATH = re.compile(r"^blobs/sha256/([0-9a-f]{2})/([0-9a-f]{64})(\.[a-z0-9]+)?$")


def _tsv(path, comment="#"):
    lines = [l for l in path.read_text(encoding="utf-8").splitlines() if l and not l.startswith(comment)]
    header = lines[0].split("\t")
    return [dict(zip(header, l.split("\t"))) for l in lines[1:]]


@pytest.fixture(scope="module")
def blob_map():
    return _tsv(BLOB_MAP)


@pytest.fixture(scope="module")
def blobs():
    return {r["sha256"]: r for r in _tsv(SHA_MANIFEST)}


def _git(*args, inp=None):
    return subprocess.run(["git", "-c", "core.quotepath=false", "-C", str(REPO), *args],
                          input=inp, capture_output=True, check=False)


def test_blob_map_and_manifest_agree(blob_map, blobs):
    assert blob_map and blobs
    assert {r["sha256_blob"] for r in blob_map} == set(blobs)
    for r in blob_map:
        b = blobs[r["sha256_blob"]]
        assert r["blob_path"] == b["blob_path"] and r["bytes"] == b["bytes"], r["path"]
    for sha, b in blobs.items():
        m = BLOB_PATH.match(b["blob_path"])
        assert m and m.group(1) == sha[:2] and m.group(2) == sha, b["blob_path"]


def test_every_blob_verified_on_the_archive_host(blobs):
    rows = {r["sha256"]: r for r in _tsv(VERIFY)}
    assert set(rows) == set(blobs)
    for sha, r in rows.items():
        assert r["status"] == "OK" and r["sha256_recomputed"] == sha, sha


def test_moved_paths_are_not_tracked(blob_map):
    if _git("rev-parse", "--git-dir").returncode != 0:
        pytest.skip("not a git checkout")
    tracked = set(_git("ls-files", "-z").stdout.decode("utf-8").split("\0"))
    back = sorted(r["path"] for r in blob_map if r["path"] in tracked)
    assert back == [], f"{len(back)} archived paths are tracked again, e.g. {back[:3]}"


def test_moved_paths_are_ignored(blob_map):
    if _git("rev-parse", "--git-dir").returncode != 0:
        pytest.skip("not a git checkout")
    paths = [r["path"] for r in blob_map]
    out = _git("check-ignore", "--no-index", "-z", "--stdin", inp="\0".join(paths).encode() + b"\0")
    ignored = set(out.stdout.decode("utf-8").split("\0"))
    missing = [p for p in paths if p not in ignored]
    assert missing == [], f"{len(missing)} archived paths not ignored, e.g. {missing[:3]}"


def test_inputs_yaml_points_at_the_store(blob_map, blobs):
    data = yaml.safe_load((REPO / "data" / "inputs.yaml").read_text(encoding="utf-8"))
    move = next(m for m in data["archive_moves"] if m["date"] == "2026-10-08")
    assert move["root"] == "/mnt/ace/digitalmodel"
    assert move["layout"] == "blobs/sha256/<aa>/<sha256>.<ext>"
    for key in ("blob_map", "sha256_manifest", "verification"):
        assert (REPO / move[key]).is_file(), move[key]
    assert move["files"] == len(blob_map)
    assert move["unique_blobs"] == len(blobs)
    assert move["bytes_unique"] == sum(int(b["bytes"]) for b in blobs.values())
    assert "2026-10-08" in (REPO / "DOCUMENT-MAP.md").read_text(encoding="utf-8")
