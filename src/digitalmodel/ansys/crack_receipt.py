"""Provenance and artifact checks for FE crack-state receipts (#2157 P0).

A receipt (``fe_states/<state>.receipt.json``) is the committed evidence of one
licensed MAPDL solve. ``cint_parser.validate_receipt_schema`` checks its shape;
this module checks that it is *true* and *current*:

* **provenance**: ``run.producing_commit`` exists as a commit and is an ancestor
  of HEAD; the generator tree was clean when the solve ran; every generator and
  parser file listed in ``run.generator_files`` has the recorded git blob at the
  producing commit and the same blob in the checkout, unless a reviewed
  post-solve code delta is recorded in ``fe_states/code_deltas.json``;
* **artifacts**: the committed, host-free solver output (CINT table, reaction
  summary, other tables) matches the recorded SHA-256, and re-parsing it
  reproduces the receipt's reactions, front values and guard verdicts;
* **deck currency**: the current generator reproduces each level's deck hash.

Shallow clones: a producing commit that is not present locally is fetched by
SHA when the repository is shallow. A commit that still cannot be reached is
reported as ``unreachable_shallow``, distinct from ``absent`` (not a commit in a
full clone, e.g. a fake SHA).

Only ``git`` with fixed argument lists is invoked; nothing needs a licence.
"""

from __future__ import annotations

import hashlib
import json
import math
import subprocess  # nosec B404 - fixed git argv only
from collections.abc import Mapping
from pathlib import Path

from digitalmodel.ansys import cint_parser

# Guards that have no meaning for a model without a crack front.
NOT_APPLICABLE_GUARDS = {
    "weldolet_uncracked": ("c_contour", "d_complete", "g_j_mesh"),
    "limit_load": ("b_mesh_load", "c_contour", "d_complete", "g_j_mesh"),
}
_REL_TOL = 1e-12


# --------------------------------------------------------------------------- #
# git helpers
# --------------------------------------------------------------------------- #
def git(repo: Path, *args: str, check: bool = True) -> str:
    """Run ``git`` with a fixed argument list in ``repo``; return stdout."""
    proc = subprocess.run(  # nosec B603 B607 - fixed argv, no shell
        ["git", *args],
        cwd=str(repo),
        capture_output=True,
        text=True,
        check=False,
    )
    if check and proc.returncode != 0:
        raise RuntimeError(f"git {' '.join(args)} failed: {proc.stderr.strip()}")
    return proc.stdout.strip()


def _git_ok(repo: Path, *args: str) -> bool:
    proc = subprocess.run(  # nosec B603 B607 - fixed argv, no shell
        ["git", *args], cwd=str(repo), capture_output=True, text=True, check=False
    )
    return proc.returncode == 0


def is_shallow(repo: Path) -> bool:
    return git(repo, "rev-parse", "--is-shallow-repository", check=False) == "true"


def git_commit_status(sha: str, repo: Path) -> str:
    """``present`` | ``absent`` | ``unreachable_shallow`` for a commit SHA."""
    if _git_ok(repo, "cat-file", "-e", f"{sha}^{{commit}}"):
        return "present"
    if not is_shallow(repo):
        return "absent"
    _git_ok(repo, "fetch", "--quiet", "--no-tags", "origin", sha)
    if _git_ok(repo, "cat-file", "-e", f"{sha}^{{commit}}"):
        return "present"
    return "unreachable_shallow"


def blob_at(repo: Path, sha: str, path: str) -> str | None:
    out = git(repo, "rev-parse", f"{sha}:{path}", check=False)
    return out if len(out) == 40 else None


def worktree_blob(repo: Path, path: str) -> str | None:
    """Git blob id of the checked-out file (clean filters applied)."""
    if not (repo / path).is_file():
        return None
    return git(repo, "hash-object", "--", path, check=False) or None


def generator_blobs(repo: Path, sha: str, paths: list[str]) -> dict[str, str]:
    """Blob ids of ``paths`` at commit ``sha`` (for writing a receipt)."""
    out = {}
    for path in paths:
        blob = blob_at(repo, sha, path)
        if blob is None:
            raise RuntimeError(f"{path} is not tracked at {sha}")
        out[path] = blob
    return out


def generator_tree_clean(repo: Path, paths: list[str]) -> bool:
    """True when ``paths`` have no staged or unstaged change against HEAD."""
    return git(repo, "status", "--porcelain", "--", *paths) == ""


# --------------------------------------------------------------------------- #
# Provenance
# --------------------------------------------------------------------------- #
def load_code_deltas(path: Path) -> list[dict]:
    """Reviewed post-solve code deltas (empty when the record does not exist)."""
    if not Path(path).is_file():
        return []
    data = json.loads(Path(path).read_text(encoding="utf-8"))
    return list(data.get("deltas", []))


def _delta_covers(deltas: list[dict], state: str, path: str, old: str, new: str) -> bool:
    for d in deltas:
        if (
            d.get("state") in (state, "*")
            and d.get("path") == path
            and d.get("from_blob") == old
            and d.get("to_blob") == new
            and d.get("reviewed_by")
            and d.get("rationale")
        ):
            return True
    return False


def provenance_problems(
    receipt: Mapping,
    repo: Path,
    deltas: list[dict] | None = None,
    *,
    historical: bool = False,
) -> list[str]:
    """Why the receipt's provenance cannot be trusted (empty list = trusted).

    ``historical=True`` (superseded evidence, e.g. the stop-rule receipts):
    the producing commit and the recorded generator blobs at that commit are
    checked, but not that the files are unchanged in the checkout.
    """
    deltas = deltas or []
    run = receipt.get("run", {})
    state = receipt.get("state", "?")
    sha = str(run.get("producing_commit", ""))
    problems: list[str] = []
    if run.get("generator_tree_clean") is not True:
        problems.append(f"{state}: generator_tree_clean is not true")
    status = git_commit_status(sha, repo)
    if status == "absent":
        problems.append(f"{state}: producing commit {sha} is not a commit in this repository")
        return problems
    if status == "unreachable_shallow":
        problems.append(
            f"{state}: producing commit {sha} is unreachable in a shallow clone "
            "(fetch full history, e.g. actions/checkout fetch-depth: 0)"
        )
        return problems
    if not _git_ok(repo, "merge-base", "--is-ancestor", sha, "HEAD"):
        problems.append(f"{state}: producing commit {sha} is not an ancestor of HEAD")
    files = run.get("generator_files") or {}
    if not files:
        problems.append(f"{state}: no generator files recorded")
    for path, recorded in sorted(files.items()):
        at_sha = blob_at(repo, sha, path)
        if at_sha != recorded:
            problems.append(
                f"{state}: {path} recorded blob {recorded} but the producing commit "
                f"has {at_sha}"
            )
            continue
        if historical:
            continue
        current = worktree_blob(repo, path)
        if current != recorded and not _delta_covers(deltas, state, path, recorded, current):
            problems.append(
                f"{state}: {path} changed since the solve ({recorded} -> {current}) "
                "and no reviewed code delta is recorded"
            )
    return problems


# --------------------------------------------------------------------------- #
# Committed solver output
# --------------------------------------------------------------------------- #
def text_sha256(text: str) -> str:
    """SHA-256 of text with LF line endings (checkout-independent)."""
    norm = text.replace("\r\n", "\n")
    return hashlib.sha256(norm.encode("utf-8")).hexdigest()


def _read_text(path: Path) -> str:
    return path.read_bytes().decode("utf-8").replace("\r\n", "\n")


def _close(a, b) -> bool:
    if a is None or b is None:
        return a is b
    if isinstance(a, (int, float)) and isinstance(b, (int, float)):
        return math.isclose(float(a), float(b), rel_tol=_REL_TOL, abs_tol=1e-300)
    return a == b


def _compare(recorded, derived, where: str, problems: list[str]) -> None:
    if isinstance(recorded, dict) and isinstance(derived, dict):
        for key in sorted(set(recorded) | set(derived)):
            if key not in recorded or key not in derived:
                problems.append(f"{where}.{key}: present on one side only")
                continue
            _compare(recorded[key], derived[key], f"{where}.{key}", problems)
    elif isinstance(recorded, list) and isinstance(derived, list):
        if len(recorded) != len(derived):
            problems.append(f"{where}: length {len(recorded)} != {len(derived)}")
            return
        for i, (r, d) in enumerate(zip(recorded, derived, strict=True)):
            _compare(r, d, f"{where}[{i}]", problems)
    elif not _close(recorded, derived):
        problems.append(f"{where}: receipt {recorded!r} != artifact {derived!r}")


def artifact_problems(
    receipt: Mapping, base_dir: Path, *, historical: bool = False
) -> list[str]:
    """Check committed artifacts against the receipt (empty list = consistent).

    Digests first; then the receipt's reactions and front are re-derived by
    parsing the artifacts; then the guards are re-evaluated from the raw text
    and compared with the stored verdicts. ``historical=True`` (superseded
    evidence produced under earlier guard definitions) stops after the digests
    and the re-parsed reactions and front.
    """
    base_dir = Path(base_dir)
    state = receipt.get("state", "?")
    kind = receipt.get("kind")
    cracked = kind in cint_parser.CRACKED_KINDS
    geometry = receipt.get("front_geometry")
    problems: list[str] = []
    cint_texts: dict[int, str] = {}
    reac_texts: dict[int, str] = {}
    for mesh in receipt.get("meshes", []):
        lvl = mesh["level"]
        texts = {}
        for name, art in mesh.get("artifacts", {}).items():
            path = base_dir / art["path"]
            if not path.is_file():
                problems.append(f"{state} L{lvl}: artifact {art['path']} missing")
                continue
            text = _read_text(path)
            if text_sha256(text) != art["sha256"]:
                problems.append(f"{state} L{lvl}: artifact {art['path']} sha256 mismatch")
            texts[name] = text
        if "reac" not in texts or (cracked and "cint" not in texts):
            problems.append(f"{state} L{lvl}: required artifact missing")
            continue
        reac_texts[lvl] = texts["reac"]
        if cracked:
            cint_texts[lvl] = texts["cint"]
        derived = cint_parser.build_mesh_record(
            level=lvl,
            cint_text=texts.get("cint") if cracked else None,
            reac_text=texts["reac"],
            front_geometry=geometry,
        )
        _compare(mesh["reactions"], derived["reactions"], f"{state} L{lvl} reactions",
                 problems)
        _compare(mesh["front"], derived["front"], f"{state} L{lvl} front", problems)
        for key in ("declared_front_nodes", "declared_contours"):
            if mesh[key] != derived[key]:
                problems.append(f"{state} L{lvl}: {key} differs from the artifact")
        rev = cint_parser.parse_reaction_file(texts["reac"]).mapdl_rev or ""
        if mesh.get("run", {}).get("mapdl_version") != rev:
            problems.append(f"{state} L{lvl}: run.mapdl_version differs from the artifact")
        if kind in ("weldolet_uncracked", "weldolet_crack", "limit_load"):
            if not historical:
                problems.extend(_weldolet_derived_problems(receipt, mesh, texts))
    if problems or not reac_texts or historical:
        return problems
    guards = cint_parser.evaluate_guards(
        cint_texts, reac_texts, front_geometry=geometry, cracked=cracked
    )
    if kind == "limit_load" and len(reac_texts) == 1:
        guards["b_mesh_load"] = cint_parser.single_mesh_not_applicable()
    for name, g in guards.items():
        stored = receipt["guards"].get(name, {}).get("status")
        if stored != g.status:
            problems.append(
                f"{state}: guard {name} stored {stored!r}, artifacts give {g.status!r}"
            )
    return problems


def _weldolet_derived_problems(receipt: Mapping, mesh: Mapping, texts: dict) -> list[str]:
    """Re-derive the weldolet-specific receipt values from the artifacts."""
    from digitalmodel.ansys import weldolet_crack, weldolet_limit

    state, lvl = receipt.get("state", "?"), mesh["level"]
    problems: list[str] = []
    if receipt["kind"] == "limit_load":
        if "lpl" not in texts:
            return [f"{state} L{lvl}: load-deflection artifact missing"]
        lspec = weldolet_limit.spec_from_receipt(dict(receipt), lvl)
        derived = weldolet_limit.limit_load_result(lspec, texts["lpl"])
        _compare(mesh.get("limit_load"), derived, f"{state} L{lvl} limit_load", problems)
        if lvl == receipt["primary_level"]:
            _compare(receipt.get("limit_load"), derived, f"{state} limit_load", problems)
        return problems
    if receipt.get("plane") == "crotch":
        from digitalmodel.ansys import weldolet_crotch

        cspec = weldolet_crotch.spec_from_receipt(dict(receipt), lvl)
        ratios = [weldolet_crack.j_from_k_ratio(n, cspec.base) for n in mesh["front"]]
        _compare(mesh.get("j_from_k_ratio"), ratios, f"{state} L{lvl} j_from_k", problems)
        gov = weldolet_crack.governing_summary(mesh["front"], cspec.base)
        _compare(mesh.get("governing"), gov, f"{state} L{lvl} governing", problems)
        if lvl == receipt["primary_level"]:
            _compare(receipt.get("governing"), gov, f"{state} governing", problems)
        return problems
    spec = weldolet_crack.spec_from_receipt(dict(receipt), lvl)
    if receipt["kind"] == "weldolet_uncracked":
        if "path" not in texts or "hoop" not in texts:
            return [f"{state} L{lvl}: path/hoop artifacts missing"]
        derived = weldolet_crack.derive_uncracked(spec, texts["path"], texts["hoop"])
        _compare(mesh.get("derived"), derived, f"{state} L{lvl} derived", problems)
        if lvl == receipt["primary_level"]:
            _compare(receipt.get("sigma_ref"), derived["sigma_ref"],
                     f"{state} sigma_ref", problems)
            _compare(receipt.get("plausibility"), derived["plausibility"],
                     f"{state} plausibility", problems)
    else:
        ratios = [weldolet_crack.j_from_k_ratio(n, spec) for n in mesh["front"]]
        _compare(mesh.get("j_from_k_ratio"), ratios, f"{state} L{lvl} j_from_k", problems)
        if "sifs" not in texts:
            return [*problems, f"{state} L{lvl}: start-node SIFS audit artifact missing"]
        audit = weldolet_crack.start_node_audit(texts["sifs"])
        _compare(mesh.get("start_node_audit"), audit, f"{state} L{lvl} start-node audit",
                 problems)
        gov = weldolet_crack.governing_summary(mesh["front"], spec)
        _compare(mesh.get("governing"), gov, f"{state} L{lvl} governing", problems)
        if lvl == receipt["primary_level"]:
            _compare(receipt.get("governing"), gov, f"{state} governing", problems)
    return problems


def declared_state_problems(fe_states: Path) -> list[str]:
    """A declared state without a receipt is a failure, never a skip."""
    manifest = json.loads((Path(fe_states) / "declared_states.json").read_text("utf-8"))
    problems = []
    for entry in manifest["states"]:
        if not (Path(fe_states) / f"{entry['state']}.receipt.json").is_file():
            problems.append(f"declared state {entry['state']} has no receipt")
    return problems


# --------------------------------------------------------------------------- #
# Deck regeneration
# --------------------------------------------------------------------------- #
def regenerate_deck_sha256(receipt: Mapping, level: int) -> str:
    """Deck hash of ``level`` regenerated by the current generator."""
    kind = receipt["kind"]
    if kind == "verification":
        from digitalmodel.ansys.crack_verification import (
            CrackPlateSpec,
            deck_sha256,
            generate_crack_verification_apdl,
        )

        spec = CrackPlateSpec(**receipt["spec"], mesh_level=level)
        return deck_sha256(generate_crack_verification_apdl(spec))
    if kind == "limit_load":
        from digitalmodel.ansys import weldolet_limit

        return weldolet_limit.deck_sha256_for_receipt(dict(receipt), level)
    if receipt.get("plane") == "crotch":
        from digitalmodel.ansys import weldolet_crotch

        return weldolet_crotch.deck_sha256_for_receipt(dict(receipt), level)
    from digitalmodel.ansys import weldolet_crack

    return weldolet_crack.deck_sha256_for_receipt(dict(receipt), level)
