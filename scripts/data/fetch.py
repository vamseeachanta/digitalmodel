# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Restore manifest-listed data from ACE_DATA_ROOT or ACE_DATA_REMOTE."""
import argparse
import csv
import fnmatch
import hashlib
import os
from pathlib import Path, PurePosixPath
import re
import shutil
import stat
import subprocess
import sys
import tempfile


class StoreUnavailable(FileNotFoundError):
    """The configured store cannot supply the requested blob."""


def validate_path(value):
    """Validate a canonical relative path before any filesystem traversal.

    Git repository-layout documentation defines both .git directories and
    gitfiles. Reject case and trailing-dot/space aliases for portable restores.
    """
    path = PurePosixPath(value)
    if (not path.parts or path.is_absolute() or ".." in path.parts
            or "\\" in value or ":" in value or str(path) != value
            or any(part.rstrip(" .").casefold() == ".git" for part in path.parts)):
        raise ValueError("Invalid manifest path or Git metadata destination")
    return path


def store_configured():
    """Return whether a mounted or remote store has been configured."""
    return bool(os.environ.get("ACE_DATA_ROOT") or os.environ.get("ACE_DATA_REMOTE"))


def read_manifest(root):
    """Read and validate paths, sizes, digests and content-store identifiers."""
    with (root / "data/MANIFEST.tsv").open(encoding="utf-8", newline="") as stream:
        reader = csv.DictReader(stream, delimiter="\t")
        if reader.fieldnames != ["path", "bytes", "sha256", "store_id"]:
            raise ValueError("Invalid manifest columns")
        rows = list(reader)
    seen = set()
    for row in rows:
        if set(row) != {"path", "bytes", "sha256", "store_id"} or any(
            value is None for value in row.values()
        ):
            raise ValueError("Invalid manifest row")
        validate_path(row["path"])
        if row["path"] in seen:
            raise ValueError("Invalid or duplicate manifest path")
        seen.add(row["path"])
        digest = row["sha256"]
        if not re.fullmatch(r"[0-9a-f]{64}", digest):
            raise ValueError("Invalid manifest digest")
        if not re.fullmatch(
            rf"blobs/sha256/{digest[:2]}/{digest}\.[a-z0-9]+", row["store_id"]
        ) or int(row["bytes"]) < 1:
            raise ValueError("Invalid manifest store identifier or size")
    return rows


def select_rows(rows, selector):
    """Select exact repository paths, path globs or content-store identifiers."""
    selected = [r for r in rows if r["store_id"] == selector
                or fnmatch.fnmatchcase(r["path"], selector)]
    if not selected:
        raise ValueError("No manifest entry matches the selector")
    return selected


def destination(root, row):
    """Reject symlinks and paths that escape the repository root."""
    path = validate_path(row["path"])
    root = Path(root).absolute()
    target = root / path
    current = target
    while current != root:
        if current.is_symlink():
            raise ValueError("Symlink destination is not permitted")
        current = current.parent
    if not target.resolve().is_relative_to(root.resolve()):
        raise ValueError("Destination escapes repository root")
    return target


def stage_blob(row, staged, transfer_timeout=None):
    """Copy a mounted blob or retrieve it with scp into temporary storage."""
    mounted = os.environ.get("ACE_DATA_ROOT")
    if mounted:
        base = Path(mounted) / "data/digitalmodel"
        source = base / row["store_id"]
        if not source.resolve().is_relative_to(base.resolve()):
            raise ValueError("Store blob escapes store root")
        if source.is_file():
            shutil.copyfile(source, staged)
            return
        if base.is_dir():
            raise FileNotFoundError(f"Store blob missing: {row['store_id']}")
    remote = os.environ.get("ACE_DATA_REMOTE")
    if remote:
        if (not re.fullmatch(r"[a-zA-Z0-9_.@-]+:[a-zA-Z0-9_./-]+", remote)
                or remote.startswith("-")):
            raise ValueError("Invalid ACE_DATA_REMOTE")
        source_arg = remote.rstrip("/") + "/data/digitalmodel/" + row["store_id"]
        try:
            subprocess.run(["scp", "-oBatchMode=yes", "-oConnectTimeout=15",
                            "--", source_arg, str(staged)],
                           check=True, capture_output=True, timeout=transfer_timeout,
                           env={**os.environ, "LC_ALL": "C", "LANG": "C"})
            return
        except subprocess.TimeoutExpired as exc:
            raise TimeoutError("Store transfer duration exceeded the requested limit") from exc
        except subprocess.CalledProcessError as exc:
            network_errors = (b"Connection timed out", b"Connection refused",
                              b"No route to host", b"Network is unreachable",
                              b"Could not resolve hostname")
            if any(message in (exc.stderr or b"") for message in network_errors):
                raise StoreUnavailable("Store not reachable") from exc
            raise ValueError("Remote store transfer failed; check access and blob availability") from exc
    if mounted:
        raise StoreUnavailable("Store not reachable")
    raise StoreUnavailable("Set ACE_DATA_ROOT or ACE_DATA_REMOTE, then run scripts/data/fetch.py again")


def verified_file(path, row):
    """Check size and SHA-256; filenames and timestamps alone prove nothing."""
    if not path.is_file() or path.stat().st_size != int(row["bytes"]):
        return False
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest() == row["sha256"]


def fetch_row(root, row, transfer_timeout=None):
    """Verify staged bytes before restoring a manifest-listed file."""
    target = destination(root, row)
    if verified_file(target, row):
        return target
    with tempfile.TemporaryDirectory(prefix="store-fetch-") as temp_dir:
        # Python 3.11 stat permission flags: restore creator access masked by
        # umask, without granting group/other access or executable payloads.
        Path(temp_dir).chmod(stat.S_IRWXU)
        staged = Path(temp_dir) / "blob"
        stage_blob(row, staged, transfer_timeout)
        staged.chmod(staged.stat().st_mode | stat.S_IRUSR | stat.S_IWUSR)
        if not verified_file(staged, row):
            raise ValueError("Store blob failed SHA-256 or size verification")
        target = destination(root, row)
        missing = []
        parent = target.parent
        while not parent.exists():
            missing.append(parent)
            parent = parent.parent
        for parent in reversed(missing):
            parent.mkdir()
            parent.chmod(parent.stat().st_mode | stat.S_IRWXU)
        descriptor, name = tempfile.mkstemp(prefix=".store-fetch-", dir=target.parent)
        pending = Path(name)
        try:
            with os.fdopen(descriptor, "wb") as output, staged.open("rb") as source:
                shutil.copyfileobj(source, output)
            read_write = (stat.S_IRUSR | stat.S_IWUSR | stat.S_IRGRP
                          | stat.S_IWGRP | stat.S_IROTH | stat.S_IWOTH)
            pending.chmod(staged.stat().st_mode & read_write)
            os.replace(pending, destination(root, row))
        finally:
            pending.unlink(missing_ok=True)
    return target


def main(argv=None, repo_root=None):
    """Fetch a path, path glob or store id; return a nonzero status on failure."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("selector", help="repository path, quoted glob or store id")
    parser.add_argument("--transfer-timeout", type=float,
                        help="positive total transfer limit in seconds; default: no total limit")
    args = parser.parse_args(argv)
    root = Path(repo_root) if repo_root is not None else Path(__file__).resolve().parents[2]
    try:
        if args.transfer_timeout is not None and (
                not 0 < args.transfer_timeout < float("inf")):
            raise ValueError("Transfer timeout must be finite and positive")
        rows = select_rows(read_manifest(root), args.selector)
        for row in rows:
            fetch_row(root, row, args.transfer_timeout)
            print(f"Fetched {row['path']} (SHA-256 verified)")
    except (OSError, ValueError) as exc:
        print(str(exc), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
