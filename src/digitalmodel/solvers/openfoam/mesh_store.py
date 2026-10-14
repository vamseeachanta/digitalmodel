"""Master mesh store: one serial ``constant/polyMesh`` per mesh *identity*.

Cases link to the store instead of carrying a private copy of a mesh that is
byte-identical to their siblings'. The identity is a content hash of every
input that determines the serial mesh, so a case whose meshing dictionaries
and surfaces match an entry can reuse it without rebuilding, and a case whose
inputs drifted cannot silently reuse a stale mesh.

This module is the library form of ``scripts/cfd/mesh_store.sh``, the bash
runtime that the solve-host chain driver calls. The two MUST agree on the
identity: ``tests/solvers/openfoam/test_mesh_store.py`` runs both on the same
fixture case and asserts equal hashes. Change the identity rule in both or in
neither.

What is shared and what is not
------------------------------
Only the serial ``constant/polyMesh`` is shared. ``processor*/constant/polyMesh``
is not: ``redistributePar -decompose`` was measured to be non-reproducible run
to run (sibling cases with byte-identical serial meshes and identical
decomposeParDicts came out with different processor-0 cell counts), and a
case's processor time directories are numbered in *its* processor cell order.
A decomposition therefore belongs to the results it holds and is rebuilt per
case (about 15 s) against 12-75 min for snappyHexMesh.

Identity
--------
sha256 over, in fixed order: the meshing dictionaries in ``system/``
(blockMesh, surfaceFeatureExtract, snappyHexMesh, meshQuality, refineMesh,
then ``topoSetDict.N`` in numeric order) with comments and whitespace
normalised, and the raw bytes of ``constant/triSurface/*.stl``. Solve
dictionaries (controlDict, fvSchemes, fvSolution, decomposeParDict), ``0.orig``
and the transport/turbulence properties are NOT inputs: they change the solve,
not the mesh.

The store entry is read-only on disk so any tool that writes through a case's
link (``snappyHexMesh -overwrite``, ``refineMesh -overwrite``, ``topoSet``,
serial ``renumberMesh -overwrite``) fails loudly instead of rewriting every
sibling. Links are relative so a campaign tree can be moved as a unit.
"""

from __future__ import annotations

import hashlib
import json
import os
import re
import shutil
import socket
import stat
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Iterable

#: Meshing dictionaries that define the serial mesh, in identity order.
MESH_DICTS: tuple[str, ...] = (
    "system/blockMeshDict",
    "system/surfaceFeatureExtractDict",
    "system/snappyHexMeshDict",
    "system/meshQualityDict",
    "system/refineMeshDict",
)

#: Build logs copied into the store beside the mesh; ``link`` copies them back
#: into a reusing case because downstream gates grep them (hull layer
#: coverage from log.snappyHexMesh, the checkMesh verdict from log.checkMesh).
BUILD_LOGS: tuple[str, ...] = (
    "log.blockMesh",
    "log.surfaceFeatureExtract",
    "log.snappyHexMesh",
    "log.checkMesh",
)

IDENTITY_LEN = 12

_LINE_COMMENT = re.compile(r"//.*$")
_BLOCK_COMMENT = re.compile(r"/\*[^*]*\*/")
_WS_RUN = re.compile(r"[ \t]+")
_TOPO = re.compile(r"^topoSetDict\.(\d+)$")


class MeshStoreError(RuntimeError):
    """A store operation refused to proceed; the message says why."""


# ---------------------------------------------------------------------------
# identity


def normalise_dict_text(text: str) -> bytes:
    """Strip ``//`` and single-line ``/* */`` comments, collapse runs of
    blanks to one space, trim, drop empty lines. Mirrors the sed/tr pipeline in
    ``mesh_store.sh`` byte for byte (each surviving line ends in ``\\n``)."""
    out: list[str] = []
    for line in text.split("\n"):
        line = _LINE_COMMENT.sub("", line)
        while True:
            new = _BLOCK_COMMENT.sub("", line)
            if new == line:
                break
            line = new
        line = _WS_RUN.sub(" ", line)
        if line.startswith(" "):
            line = line[1:]
        if line.endswith(" "):
            line = line[:-1]
        if line:
            out.append(line)
    return ("".join(l + "\n" for l in out)).encode()


def mesh_inputs(case: Path) -> list[str]:
    """The identity inputs present in *case*, relative paths, fixed order."""
    case = Path(case)
    found = [d for d in MESH_DICTS if (case / d).is_file()]
    topo = []
    for p in (case / "system").glob("topoSetDict.*"):
        m = _TOPO.match(p.name)
        if m:
            topo.append((int(m.group(1)), p.name))
    found += [f"system/{name}" for _, name in sorted(topo)]
    stls = sorted(p.name for p in (case / "constant" / "triSurface").glob("*.stl"))
    found += [f"constant/triSurface/{s}" for s in stls]
    return found


def input_shas(case: Path) -> list[tuple[str, str]]:
    """``[(sha256, relative path), ...]``; dictionaries normalised, STLs raw."""
    case = Path(case)
    rows = []
    for rel in mesh_inputs(case):
        p = case / rel
        if rel.endswith(".stl"):
            digest = hashlib.sha256(p.read_bytes()).hexdigest()
        else:
            digest = hashlib.sha256(normalise_dict_text(p.read_text())).hexdigest()
        rows.append((digest, rel))
    return rows


def mesh_identity(case: Path) -> str:
    """12-hex identity of the case's mesh inputs (``mesh_store.sh id``)."""
    rows = input_shas(case)
    if len(rows) < 3:
        raise MeshStoreError(f"{case} has only {len(rows)} mesh inputs; not a meshable case")
    listing = "".join(f"{sha}  {rel}\n" for sha, rel in rows).encode()
    return hashlib.sha256(listing).hexdigest()[:IDENTITY_LEN]


def mesh_cells(polymesh: Path) -> int | None:
    """Cell count from the ``nCells:`` note in ``owner``; None if unreadable."""
    owner = Path(polymesh) / "owner"
    if not owner.is_file():
        return None
    head = owner.read_bytes()[:4096]
    m = re.search(rb"nCells:(\d+)", head)
    return int(m.group(1)) if m else None


# ---------------------------------------------------------------------------
# store


@dataclass(frozen=True)
class StoreEntry:
    path: Path

    @property
    def identity(self) -> str:
        return self.path.name.split("-", 1)[0]

    @property
    def tag(self) -> str:
        return self.path.name.split("-", 1)[1] if "-" in self.path.name else ""

    @property
    def polymesh(self) -> Path:
        return self.path / "polyMesh"

    @property
    def provenance_path(self) -> Path:
        return self.path / "mesh_provenance.json"

    def provenance(self) -> dict:
        if not self.provenance_path.is_file():
            return {}
        return json.loads(self.provenance_path.read_text())

    @property
    def cells(self) -> int | None:
        return mesh_cells(self.polymesh)


def _chmod_tree(root: Path, writable: bool) -> None:
    for dirpath, dirnames, filenames in os.walk(root):
        for n in dirnames + filenames:
            p = Path(dirpath) / n
            mode = p.stat().st_mode
            new = (mode | stat.S_IWUSR) if writable else (mode & ~(stat.S_IWUSR | stat.S_IWGRP | stat.S_IWOTH))
            p.chmod(new)
    mode = root.stat().st_mode
    root.chmod((mode | stat.S_IWUSR) if writable else (mode & ~(stat.S_IWUSR | stat.S_IWGRP | stat.S_IWOTH)))


def _linked_entry(case: Path) -> Path | None:
    pm = Path(case) / "constant" / "polyMesh"
    if not pm.is_symlink():
        return None
    target = pm.resolve()
    return target.parent if target.is_dir() else None


def _checkmesh_verdict(log: Path) -> str:
    if not log.is_file():
        return "unknown"
    text = log.read_text(errors="replace")
    ok = re.search(r"^Mesh OK", text, re.M) and not re.search(r"Failed .* mesh checks", text)
    return "PASS" if ok else "FAIL"


def _layer_coverage(log: Path) -> float | None:
    """Last ``hull`` row's coverage column from the snappy layer table."""
    if not log.is_file():
        return None
    cov = None
    for line in log.read_text(errors="replace").splitlines():
        if line.startswith("hull "):
            parts = line.split()
            if len(parts) >= 6:
                try:
                    cov = float(parts[5])
                except ValueError:
                    pass
    return cov


class MeshStore:
    """Operations over ``<root>/meshes``; every method mirrors a ``mesh_store.sh``
    subcommand and refuses (raises :class:`MeshStoreError`) where the script
    dies."""

    def __init__(self, root: Path, store: Path | None = None, cases_dir: str = "cases"):
        self.root = Path(root)
        self.store = Path(store) if store else self.root / "meshes"
        self.cases_dir = self.root / cases_dir

    # -- resolution ---------------------------------------------------------
    def case_dir(self, case: str | Path) -> Path:
        p = Path(case)
        if p.is_dir():
            return p.resolve()
        q = self.cases_dir / str(case)
        if q.is_dir():
            return q.resolve()
        raise MeshStoreError(f"no case dir: {case}")

    def entry(self, ref: str | Path) -> StoreEntry:
        p = Path(ref)
        if p.is_dir():
            return StoreEntry(p.resolve())
        q = self.store / str(ref)
        if q.is_dir():
            return StoreEntry(q.resolve())
        matches = sorted(self.store.glob(f"{ref}-*"))
        if not matches:
            raise MeshStoreError(f"no store entry: {ref}")
        return StoreEntry(matches[0].resolve())

    def entries(self) -> list[StoreEntry]:
        if not self.store.is_dir():
            return []
        return [StoreEntry(p) for p in sorted(self.store.iterdir()) if (p / "polyMesh" / "owner").is_file()]

    # -- commands -------------------------------------------------------------
    def identity(self, case: str | Path) -> str:
        return mesh_identity(self.case_dir(case))

    def find(self, case: str | Path) -> StoreEntry | None:
        ident = self.identity(case)
        matches = sorted(self.store.glob(f"{ident}-*"))
        return StoreEntry(matches[0]) if matches else None

    def link(self, case: str | Path, ref: str | Path) -> Path:
        case_dir = self.case_dir(case)
        entry = self.entry(ref)
        if not (entry.polymesh / "owner").is_file():
            raise MeshStoreError(f"{entry.path} has no polyMesh/owner")
        pm = case_dir / "constant" / "polyMesh"
        if pm.exists() and not pm.is_symlink():
            raise MeshStoreError(f"{pm} is a real directory; promote or remove it before linking")
        pm.parent.mkdir(parents=True, exist_ok=True)
        if pm.is_symlink():
            pm.unlink()
        rel = os.path.relpath(entry.polymesh, pm.parent)
        pm.symlink_to(rel)
        logs = entry.path / "logs"
        if logs.is_dir():
            for log in logs.glob("log.*"):
                dest = case_dir / log.name
                if not dest.exists():
                    shutil.copy2(log, dest)
        return pm

    def promote(self, case: str | Path, tag: str) -> StoreEntry:
        case_dir = self.case_dir(case)
        pm = case_dir / "constant" / "polyMesh"
        if pm.is_symlink():
            raise MeshStoreError(f"{pm} is already a link ({os.readlink(pm)})")
        if not (pm / "owner").is_file():
            raise MeshStoreError(f"{pm} has no built mesh")
        ident = mesh_identity(case_dir)
        existing = sorted(self.store.glob(f"{ident}-*"))
        if existing:
            raise MeshStoreError(f"identity {ident} already in store: {existing[0]}. Link to it, or drop it first.")
        dest = self.store / f"{ident}-{tag}"
        (dest / "inputs").mkdir(parents=True)
        (dest / "logs").mkdir()
        shutil.move(str(pm), str(dest / "polyMesh"))
        for rel in mesh_inputs(case_dir):
            if rel.endswith(".stl"):
                continue
            target = dest / "inputs" / rel
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(case_dir / rel, target)
        for pattern in BUILD_LOGS + ("log.topoSet.*", "log.refineMesh.*"):
            for log in case_dir.glob(pattern):
                shutil.copy2(log, dest / "logs" / log.name)
        entry = StoreEntry(dest)
        provenance = {
            "identity": ident,
            "tag": tag,
            "built_from_case": case_dir.name,
            "promoted_at": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
            "host": socket.gethostname().split(".")[0],
            "openfoam": os.environ.get("WM_PROJECT_VERSION", "unknown"),
            "cells": entry.cells,
            "checkMesh": _checkmesh_verdict(case_dir / "log.checkMesh"),
            "hull_layer_coverage_pct": _layer_coverage(case_dir / "log.snappyHexMesh"),
            "inputs": {rel: sha for sha, rel in input_shas(case_dir)},
        }
        entry.provenance_path.write_text(json.dumps(provenance, indent=2) + "\n")
        _chmod_tree(dest / "polyMesh", writable=False)
        self.link(case_dir, dest)
        return entry

    def dedupe(self, case: str | Path, ref: str | Path) -> bool:
        """Replace a case's private polyMesh with a link, only if byte-identical
        to the entry (checkMesh's ``sets/`` output is diagnostic and ignored).
        Returns False when the case already links."""
        case_dir = self.case_dir(case)
        entry = self.entry(ref)
        pm = case_dir / "constant" / "polyMesh"
        if pm.is_symlink():
            return False
        if not (pm / "owner").is_file():
            raise MeshStoreError(f"{pm} has no mesh to dedupe")
        if not _trees_identical(entry.polymesh, pm, ignore={"sets"}):
            raise MeshStoreError(f"{case_dir.name} polyMesh differs from {entry.polymesh} -- not touched")
        shutil.rmtree(pm)
        self.link(case_dir, entry.path)
        return True

    def verify(self, case: str | Path) -> tuple[bool, str]:
        case_dir = self.case_dir(case)
        target = _linked_entry(case_dir)
        if target is None:
            raise MeshStoreError(f"{case_dir.name} does not link into the store")
        entry = StoreEntry(target)
        ident = mesh_identity(case_dir)
        if ident == entry.identity:
            return True, f"OK {case_dir.name} inputs {ident} == {entry.path.name}"
        return False, (
            f"MISMATCH {case_dir.name} inputs hash {ident} but links to {entry.path.name}; "
            f"diff the case's mesh dicts against {entry.path / 'inputs'}"
        )

    def status(self) -> list[dict]:
        cases = [p for p in self.cases_dir.iterdir() if p.is_dir()] if self.cases_dir.is_dir() else []
        rows = []
        for entry in self.entries():
            linked = sorted(c.name for c in cases if _linked_entry(c) == entry.path)
            rows.append(
                {
                    "master": entry.path.name,
                    "cells": entry.cells,
                    "checkMesh": entry.provenance().get("checkMesh"),
                    "linked_cases": linked,
                }
            )
        return rows

    def drop(self, ref: str | Path) -> Path:
        entry = self.entry(ref)
        if self.cases_dir.is_dir():
            for c in self.cases_dir.iterdir():
                if c.is_dir() and _linked_entry(c) == entry.path:
                    raise MeshStoreError(f"{c.name} still links to {entry.path}")
        _chmod_tree(entry.path, writable=True)
        shutil.rmtree(entry.path)
        return entry.path


def _trees_identical(a: Path, b: Path, ignore: Iterable[str] = ()) -> bool:
    ignore = set(ignore)

    def listing(root: Path) -> dict[str, Path]:
        out = {}
        for p in root.rglob("*"):
            rel = p.relative_to(root)
            if rel.parts and rel.parts[0] in ignore:
                continue
            if p.is_file():
                out[str(rel)] = p
        return out

    la, lb = listing(a), listing(b)
    if la.keys() != lb.keys():
        return False
    for k in la:
        if la[k].stat().st_size != lb[k].stat().st_size:
            return False
        if la[k].read_bytes() != lb[k].read_bytes():
            return False
    return True


# ---------------------------------------------------------------------------
# CLI


def main(argv: list[str] | None = None) -> int:
    import argparse

    ap = argparse.ArgumentParser(
        prog="mesh_store",
        description="Master mesh store (library twin of scripts/cfd/mesh_store.sh)",
    )
    ap.add_argument("--root", default=os.environ.get("DM_CFD_ROOT", "."), help="campaign root (DM_CFD_ROOT)")
    ap.add_argument("--store", default=os.environ.get("DM_CFD_MESH_STORE"), help="store dir (default <root>/meshes)")
    sub = ap.add_subparsers(dest="cmd", required=True)
    for name, nargs in (("id", 1), ("find", 1), ("promote", 2), ("link", 2), ("dedupe", 2), ("verify", 1), ("drop", 1)):
        sp = sub.add_parser(name)
        sp.add_argument("args", nargs=nargs)
    sub.add_parser("status")
    ns = ap.parse_args(argv)
    ms = MeshStore(Path(ns.root), Path(ns.store) if ns.store else None)
    try:
        if ns.cmd == "id":
            print(ms.identity(ns.args[0]))
        elif ns.cmd == "find":
            e = ms.find(ns.args[0])
            if e is None:
                print(f"no master for identity {ms.identity(ns.args[0])}", file=__import__("sys").stderr)
                return 1
            print(e.path)
        elif ns.cmd == "promote":
            e = ms.promote(ns.args[0], ns.args[1])
            print(f"promoted {ns.args[0]} -> {e.path} ({e.cells} cells)")
        elif ns.cmd == "link":
            print(ms.link(ns.args[0], ns.args[1]))
        elif ns.cmd == "dedupe":
            print("deduped" if ms.dedupe(ns.args[0], ns.args[1]) else "already linked")
        elif ns.cmd == "verify":
            ok, msg = ms.verify(ns.args[0])
            print(msg)
            return 0 if ok else 2
        elif ns.cmd == "status":
            print(f"{'MASTER':<32} {'CELLS':>10} {'CHECK':>7}  LINKED_CASES")
            for r in ms.status():
                print(f"{r['master']:<32} {str(r['cells']):>10} {str(r['checkMesh']):>7}  {' '.join(r['linked_cases']) or '(none)'}")
        elif ns.cmd == "drop":
            print(f"dropped {ms.drop(ns.args[0])}")
    except MeshStoreError as exc:
        print(f"mesh_store: FATAL: {exc}", file=__import__("sys").stderr)
        return 1
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
