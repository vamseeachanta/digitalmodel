#!/usr/bin/env python3
"""Regenerate analytic compatibility fixtures from the pinned pre-refactor source.

Run with NumPy 2.4.4, scipy and pyyaml via uv run, with PYTHONPATH=src.
Only the named fixture is written; Git metadata is read with caller bindings cleared.
"""
import base64
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import pickle
import platform
import subprocess
import sys
import tempfile

import numpy as np

BASELINE = "4f7bfc0cb4d9f62d7485fe5b03fb00550b2320eb"
SOURCE = "src/digitalmodel/naval_architecture/mesh_hydrostatics.py"
SOURCE_SHA256 = "ad745569961e8dcb59fcfe1c931dad2655003fa886d702be1c1f7653ad00cc1b"
HULL_SOURCE = "src/digitalmodel/naval_architecture/hull_fixtures.py"
HULL_SHA256 = "d0ab75f251489f0e4052d84d30d2c7de7252d23c498c31b85aef1735614f7044"
PYTHON_VERSION = "3.11.14"


def _read_pinned_source(root, env, path, digest):
    source = subprocess.check_output(["git", "show", f"{BASELINE}:{path}"], cwd=root, env=env)
    if hashlib.sha256(source).hexdigest() != digest:
        raise ValueError(f"baseline source digest mismatch: {path}")
    return source


def _load_module(name, path, source):
    path.write_bytes(source)
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


def main():
    sys.dont_write_bytecode = True
    root = Path(__file__).resolve().parents[2]
    env = {k: v for k, v in os.environ.items()
           if k not in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE")}
    source = _read_pinned_source(root, env, SOURCE, SOURCE_SHA256)
    hull = _read_pinned_source(root, env, HULL_SOURCE, HULL_SHA256)
    if np.__version__ != "2.4.4":
        raise ValueError("fixture reproduction requires NumPy 2.4.4")
    if platform.python_version() != PYTHON_VERSION:
        raise ValueError(f"fixture reproduction requires Python {PYTHON_VERSION}")
    with tempfile.TemporaryDirectory(prefix="sr2314-pickle-") as scratch:
        _load_module("digitalmodel.naval_architecture.hull_fixtures",
                     Path(scratch) / "hull_fixtures.py", hull)
        name = "digitalmodel.naval_architecture.mesh_hydrostatics"
        module = _load_module(name, Path(scratch) / "baseline.py", source)
        mesh = module.TriMesh(*module.box_mesh(20, 4, 3), units="m", axes=module.CANONICAL_AXES)
        objects = {"mesh": mesh, "clipped": module.clip_at_waterline(mesh, 1.5),
                   "result": module.compute_hydrostatics(mesh, 1.5)}
        record = {"source_revision": BASELINE, "source_sha256": SOURCE_SHA256,
                  "numpy_version": np.__version__,
                  "description": "Analytic 20 x 4 x 3 m box at 1.5 m draft, original public classes",
                  "pickles": {k: base64.b64encode(pickle.dumps(v, protocol=4)).decode()
                              for k, v in objects.items()}}
    target = root / "tests/fixtures/test_vectors/naval_architecture/mesh_legacy_pickles.json"
    target.write_text(json.dumps(record, indent=2) + "\n")
    print(hashlib.sha256(target.read_bytes()).hexdigest())


if __name__ == "__main__":
    main()
