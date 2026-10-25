"""Offline completeness checks; callers own the trusted input declaration."""

from collections.abc import Mapping
import hashlib
import os
from pathlib import Path
import re
import stat


def _names(names):
    if not isinstance(names, (list, tuple)) or not names:
        raise ValueError("invalid_declaration")
    folded = set()
    for name in names:
        if (not isinstance(name, str) or not name
                or any(part in ("", ".", "..") for part in name.split("/"))
                or not re.fullmatch(r"[a-zA-Z0-9_./-]+", name)):
            raise ValueError("invalid_input_path")
        if name.casefold() in folded:
            raise ValueError("invalid_declaration")
        folded.add(name.casefold())
    return names


def _read_digest(root, name):
    root = Path(os.path.abspath(root))
    path = root / name
    try:
        for entry in (*reversed(root.parents), root, *[root / Path(*Path(name).parts[:i])
                       for i in range(1, len(Path(name).parts) + 1)]):
            info = entry.lstat()
            # Windows reparse-point flag, as used by verify_solver_evidence.py::_regular.
            if stat.S_ISLNK(info.st_mode) or getattr(info, "st_file_attributes", 0) & 0x400:
                raise ValueError("input_redirected")
            if entry != path and not stat.S_ISDIR(info.st_mode):
                raise ValueError("input_unreadable")
        if not stat.S_ISREG(info.st_mode):
            raise ValueError("input_unreadable")
        flags = (os.O_RDONLY | getattr(os, "O_NOFOLLOW", 0)
                 | getattr(os, "O_NONBLOCK", 0) | getattr(os, "O_BINARY", 0))
        with os.fdopen(os.open(path, flags), "rb") as stream:
            before = os.fstat(stream.fileno())
            digest = hashlib.file_digest(stream, "sha256").hexdigest()
            after = os.fstat(stream.fileno())
        final = path.lstat()
        keys = ("st_dev", "st_ino", "st_size", "st_mtime_ns", "st_ctime_ns")
        if any(getattr(info, key) != getattr(record, key)
               for key in keys for record in (before, after, final)):
            raise ValueError("input_changed_during_read")
        return digest, (before.st_dev, before.st_ino)
    except OSError as error:
        raise ValueError("input_unreadable") from error


def _capture_inputs(root, required):
    """Retain the identity from the same stable read as each mandatory digest."""
    if not isinstance(required, Mapping):
        raise ValueError("invalid_declaration")
    required = dict(required)
    names = _names(list(required))
    # SHA-256: NIST SHA-256 worked example, digest length 256 bits = 64 hex digits.
    if any(not isinstance(value, str) or not re.fullmatch(r"[0-9a-f]{64}", value)
           for value in required.values()):
        raise ValueError("invalid_declaration")
    observed, identities = {}, set()
    for name in sorted(names):
        observed[name], identity = _read_digest(root, name)
        identities.add(identity)
        if observed[name] != required[name]:
            raise ValueError("input_substituted")
    return observed, identities


def capture_inputs(root, required):
    """Read every mandatory input and compare its independently pinned digest."""
    return _capture_inputs(root, required)[0]


def _cleanup_confirmed(observation):
    # Issue 2085 contract: successful observation of the owned tree, exited root,
    # and zero active processes. A request to terminate is not an observation.
    return (isinstance(observation, Mapping)
            and observation.get("scope") == "owned_process_tree"
            and observation.get("observation_succeeded") is True
            and observation.get("root_exited") is True
            and type(observation.get("active_processes")) is int
            and observation["active_processes"] == 0)


def audit_proof(root, required_inputs, required_outputs, child, cleanup):
    """Audit completeness using parent-owned declarations and cleanup evidence.

    ``child`` is untrusted. ``cleanup`` must come from the parent's observer,
    after its final wait and whole-tree accounting. This function neither runs
    that observer nor authenticates its record. A complete proof does not
    establish numerical correctness, physical settings or engineering fitness.
    Optional diagnostics are outside both mandatory hash manifests.
    """
    failures, inputs, outputs, input_identities = [], {}, {}, set()
    try:
        inputs, input_identities = _capture_inputs(root, required_inputs)
    except ValueError as error:
        failures.append(str(error))
    if not isinstance(child, Mapping):
        child = {}
    if child.get("ok") is not True:
        failures.append("child_not_successful")
    if not inputs or child.get("input_hashes") != inputs:
        failures.append("input_hashes_mismatch")
    try:
        names = _names(required_outputs)
        if {name.casefold() for name in names} & {name.casefold() for name in inputs}:
            raise ValueError("input_output_overlap")
        for name in sorted(names):
            digest, identity = _read_digest(root, name)
            if identity in input_identities:
                raise ValueError("input_output_overlap")
            outputs[name] = digest
    except (ValueError, TypeError):
        failures.append("output_unreadable_or_invalid")
    if not outputs or child.get("output_hashes") != outputs:
        failures.append("output_hashes_mismatch")
    if not _cleanup_confirmed(cleanup):
        failures.append("cleanup_unconfirmed")
    return {"complete": not failures, "failures": failures,
            "input_hashes": inputs, "output_hashes": outputs}
