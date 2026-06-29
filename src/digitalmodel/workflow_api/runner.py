# ABOUTME: digitalmodel-bound run_workflow() — drives digitalmodel's #3307-embeddable
# ABOUTME: engine, reusing the SHARED assetutilities ResultEnvelope contract (#3285).
"""Deterministic, in-process workflow runner for digitalmodel.

``run_workflow(workflow_id, params=None, cfg=None, verify_reproducible=False)``
returns a typed :class:`assetutilities.workflow_api.ResultEnvelope`.

This is the per-repo runner the re-locked #3282 contract calls for: each adopting
repo provides its own ``run_workflow`` that drives ITS engine + resolves ITS
registry, while REUSING (never redefining) the shared ``ResultEnvelope`` +
``ResultLocator`` + ``extract_result`` + hashing/provenance helpers imported from
assetutilities. Two digitalmodel-specific bindings:

* it drives ``digitalmodel.engine.engine(cfg=..., embed=True, root_folder=, log_to_file=False)``
  (the workspace-hub#3307 embed path), and resolves a BARE single-registry id in
  digitalmodel's own ``docs/registry/workflows.yaml`` (cross-repo ``repo:id@version``
  resolution is #3284-owned and out of scope here);
* it stamps ``code_version("digitalmodel")`` and lifts any DNV/API ``citations``
  sidecar from an ``in_memory`` payload into ``provenance.standard_revisions``.

Side-effect-freeness comes entirely from the embed path: all result + log writes
land under a throwaway ``tempfile.mkdtemp()`` root which is ``rmtree``'d after the
content hash is taken, leaving the repo/example dirs byte-for-byte untouched.
"""

from __future__ import annotations

import copy
import tempfile
import shutil
from pathlib import Path

import yaml

from assetutilities.common.update_deep import update_deep_dictionary
from assetutilities.workflow_api import (
    ResultEnvelope,
    ResultLocator,
    extract_result,
)
from assetutilities.workflow_api.envelope import (
    compute_reproducible,
    input_hash,
    make_provenance,
    result_hash,
    utc_now_iso,
)

PACKAGE_NAME = "digitalmodel"


def _repo_root() -> Path:
    # runner.py -> workflow_api -> digitalmodel -> src -> <repo root>
    return Path(__file__).resolve().parents[3]


def registry_path() -> Path:
    return _repo_root() / "docs" / "registry" / "workflows.yaml"


def load_registry() -> dict:
    with open(registry_path()) as fh:
        return yaml.safe_load(fh)


def resolve_registry_row(workflow_id: str) -> dict:
    """Resolve a BARE in-repo id against digitalmodel's registry (fail-closed).

    Cross-repo ``repo:id@version`` resolution is #3284-owned and intentionally not
    handled here. When several rows share an id, prefer the one flagged
    ``latest: true``, else the first.
    """
    rows = [
        row
        for row in load_registry().get("workflows", [])
        if row.get("id") == workflow_id
    ]
    if not rows:
        raise KeyError(
            f"unknown workflow_id '{workflow_id}' (not in {registry_path()})"
        )
    for row in rows:
        if row.get("latest") is True:
            return row
    return rows[0]


def lookup_row_for_cfg(cfg: dict) -> dict | None:
    basename = cfg.get("basename")
    if not basename:
        return None
    for row in load_registry().get("workflows", []):
        if row.get("basename") == basename or row.get("id") == basename:
            return row
    return None


def resolve_example_path(rel_input: str) -> Path:
    return _repo_root() / rel_input


def build_cfg(row: dict, params: dict | None) -> dict:
    """Build the run cfg from a registry row + caller params (params win)."""
    cfg: dict = {"basename": row["basename"]}
    input_rel = row.get("input")
    if input_rel:
        with open(resolve_example_path(input_rel)) as fh:
            example_cfg = yaml.safe_load(fh) or {}
        cfg = update_deep_dictionary(cfg, example_cfg)
    if params:
        cfg = update_deep_dictionary(cfg, copy.deepcopy(params))
    return cfg


def _standard_revisions_from_payload(payload: dict) -> list:
    """Lift a DNV/API ``citations`` sidecar (in_memory payload) into provenance.

    The mooring-MBL route carries a DNV-OS-E301 ``citations`` list; surface each as
    a ``standard_revisions`` entry. Absent/empty (e.g. standalone mode without a
    wiki tree) yields ``[]`` -- never raises.
    """
    if not isinstance(payload, dict) or payload.get("kind") != "in_memory":
        return []
    value = payload.get("value")
    if not isinstance(value, dict):
        return []
    revisions = []
    for citation in value.get("citations", []) or []:
        if isinstance(citation, dict):
            revisions.append(
                {
                    "code_id": citation.get("code_id"),
                    "publisher": citation.get("publisher"),
                    "revision": citation.get("revision"),
                    "section": citation.get("section"),
                }
            )
    return revisions


def _run_once(cfg: dict, locator: ResultLocator):
    """One side-effect-free embed run. Returns ``(payload, warnings, result_hash)``."""
    # Imported lazily -- the digitalmodel engine import is heavy.
    from digitalmodel.engine import engine

    root = tempfile.mkdtemp(prefix="dmwf_")
    try:
        cfg_base = engine(
            cfg=copy.deepcopy(cfg),
            embed=True,
            root_folder=root,
            log_to_file=False,
        )
        payload, warns = extract_result(cfg_base, locator, root)
        return payload, warns, result_hash(payload)
    finally:
        shutil.rmtree(root, ignore_errors=True)


def run_workflow(
    workflow_id: str | None = None,
    params: dict | None = None,
    cfg: dict | None = None,
    verify_reproducible: bool = False,
) -> ResultEnvelope:
    """Run a digitalmodel registry workflow in-process; return a typed ResultEnvelope.

    Fail-closed: an unknown id or a router exception is returned as a
    ``status="error"`` envelope, never raised.
    """
    wid = workflow_id or "(inline-cfg)"
    try:
        if cfg is None:
            row = resolve_registry_row(workflow_id)
            cfg = build_cfg(row, params)
            locator = ResultLocator.from_row(row)
        else:
            cfg = copy.deepcopy(cfg)
            row = lookup_row_for_cfg(cfg)
            locator = (
                ResultLocator.from_row(row)
                if row
                else ResultLocator.default_for(cfg)
            )

        ihash = input_hash(cfg)
        payload, warns, rhash = _run_once(cfg, locator)
        repro = compute_reproducible(
            lambda: _run_once(cfg, locator)[2], rhash, verify_reproducible
        )
        return ResultEnvelope(
            workflow_id=wid,
            status="ok",
            result=payload,
            provenance=make_provenance(
                ihash,
                package_name=PACKAGE_NAME,
                standard_revisions=_standard_revisions_from_payload(payload),
                data_as_of=utc_now_iso(),
            ),
            determinism={"result_hash": rhash, "reproducible": repro},
            confidence=None,
            warnings=warns,
        )
    except Exception as exc:  # fail-closed -> error envelope, never a raw traceback
        return ResultEnvelope(
            workflow_id=wid,
            status="error",
            result={},
            provenance=make_provenance(None, package_name=PACKAGE_NAME),
            determinism={"result_hash": None, "reproducible": None},
            confidence=None,
            warnings=[str(exc)],
        )
