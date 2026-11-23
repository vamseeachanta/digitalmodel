# ABOUTME: golden_workflow_test template + volatile-field KEY-ALLOWLIST for the
# ABOUTME: determinism golden harness (workspace-hub#3283).
"""Determinism golden-test harness (workspace-hub#3283).

This is the consuming template the determinism epic (#3281) rests on. It runs a
workflow via the #3285-owned ``run_workflow`` (the REAL emission -- no fabricated
envelope), then asserts the emitted envelope's ``determinism.result_hash`` equals a
committed golden's. The PASS/FAIL verdict is **string equality of the #3282-owned
``result_hash``** -- never a value-delta heuristic. ``diff_results`` borrows the
``compare_tool`` keyed-numeric-delta + ``max_abs_delta`` shape only to DESCRIBE
what drifted in the failure message (human debugging), it is not the verdict.

The volatile-field spec is a **KEY-ALLOWLIST of dotted key-names**
(``GOLDEN_VOLATILE_KEYS``), applied by name only. A future volatile key is added by
NAME, never by a "looks like a date/path" value heuristic -- so a result value that
string-renders date-like or path-like is preserved. The result PAYLOAD is never
value-pruned; it rides the #3282 ``result_hash``, which already excludes the
``save_cfg`` dump and is content-based.
"""

from __future__ import annotations

import json
import os
from pathlib import Path

from digitalmodel.workflow_api.runner import run_workflow

# ── THE VOLATILE-FIELD SPEC = a KEY-ALLOWLIST of DOTTED KEY-NAMES ──────────────
# A key is volatile IFF its dotted name is in this set (or a per-call
# extra_volatile_keys). Values are NEVER inspected to decide inclusion.
GOLDEN_VOLATILE_KEYS = frozenset(
    {
        "provenance.code_version.git_sha",          # changes every commit
        "provenance.code_version.package_version",  # changes on release bump
        "provenance.data_as_of",                    # data-refresh dependent (record, don't pin)
        "determinism.reproducible",                 # a measured bool/None, not a fixed expectation
    }
)


def _prune_volatile(data: dict, volatile_keys=GOLDEN_VOLATILE_KEYS) -> dict:
    """Recursively drop dotted keys present in ``volatile_keys``.

    Matching is by DOTTED KEY-NAME only. Everything not listed is left
    byte-for-byte; list values are returned unchanged (the result payload is never
    value-pruned). Never inspects values to decide inclusion.
    """

    def _recurse(node, prefix: str):
        if not isinstance(node, dict):
            return node
        out = {}
        for key, value in node.items():
            dotted = f"{prefix}.{key}" if prefix else key
            if dotted in volatile_keys:
                continue
            out[key] = _recurse(value, dotted)
        return out

    return _recurse(data, "")


def _golden_result_hash(golden: dict) -> str:
    """Extract the pinned ``result_hash`` from a committed golden.

    Tolerates both the #3285 reference-golden shape (top-level ``result_hash``) and
    the ``capture_golden`` snapshot shape (also top-level ``result_hash``).
    """
    if "result_hash" in golden:
        return golden["result_hash"]
    determinism = golden.get("determinism") or golden.get("envelope_pruned", {}).get(
        "determinism", {}
    )
    return determinism.get("result_hash")


def _golden_result_payload(golden: dict):
    """Best-effort extraction of the golden's result payload (for the diff message)."""
    if "result" in golden:
        return golden["result"]
    return golden.get("envelope_pruned", {}).get("result")


def capture_golden(envelope, golden_path) -> dict:
    """Write a golden snapshot for ``envelope`` to ``golden_path``.

    The snapshot pins the #3282-owned ``result_hash`` (consumed, not recomputed),
    the result payload, and the volatile-pruned envelope. ``git_sha`` /
    ``package_version`` / ``data_as_of`` / ``reproducible`` are dropped by NAME so a
    commit or release bump does not spuriously break the golden.
    """
    snapshot = {
        "workflow_id": envelope.workflow_id,
        "status": envelope.status,
        "result_hash": envelope.determinism["result_hash"],
        "result": envelope.result,
        "envelope_pruned": _prune_volatile(envelope.to_dict()),
    }
    Path(golden_path).write_text(json.dumps(snapshot, indent=2, sort_keys=True))
    return snapshot


def diff_results(golden_result, current_result) -> str:
    """Describe what drifted between two result payloads (DEBUGGING AID ONLY).

    Reuses the ``compare_tool`` keyed-numeric-delta + ``max_abs_delta`` shape to name
    the drifted keys in a failure message. This is NEVER the verdict -- the verdict
    is ``result_hash`` string equality in :func:`golden_workflow_test`.
    """
    deltas: dict[str, str] = {}
    max_abs_delta = 0.0

    def _walk(golden_node, current_node, prefix: str):
        nonlocal max_abs_delta
        if isinstance(golden_node, dict) and isinstance(current_node, dict):
            for key in sorted(set(golden_node) | set(current_node)):
                _walk(golden_node.get(key), current_node.get(key),
                      f"{prefix}.{key}" if prefix else key)
        elif isinstance(golden_node, (int, float)) and isinstance(
            current_node, (int, float)
        ) and not isinstance(golden_node, bool) and not isinstance(current_node, bool):
            delta = current_node - golden_node
            if delta != 0:
                deltas[prefix] = f"{golden_node} -> {current_node} (Δ{delta:+})"
                max_abs_delta = max(max_abs_delta, abs(delta))
        else:
            if golden_node != current_node:
                deltas[prefix] = f"{golden_node!r} -> {current_node!r}"

    _walk(golden_result, current_result, "")
    if not deltas:
        return "result_hash drifted but no per-key value delta found (structural change)"
    lines = "\n".join(f"  {k}: {v}" for k, v in sorted(deltas.items()))
    return f"result_hash drifted; {len(deltas)} key(s) changed (max_abs_delta={max_abs_delta}):\n{lines}"


def golden_workflow_test(
    workflow_id,
    golden_path,
    *,
    params=None,
    cfg=None,
    pin_structural=False,
    extra_volatile_keys=(),
    runner=None,
):
    """Run ``workflow_id`` and assert determinism against a committed golden.

    The load-bearing assertion is ``env.determinism["result_hash"] ==
    golden_result_hash`` -- the #3282-owned content hash, consumed not recomputed.
    The envelope is the REAL return of ``run_workflow`` (no fabricated path).

    With ``REGEN_GOLDENS=1`` the golden is rewritten and the test is SKIPPED (the
    re-sanction gate -- a refreshed golden requires owner sign-off before commit; it
    must never silently pass). ``pin_structural`` additionally asserts the full
    volatile-pruned envelope equals the golden's ``envelope_pruned`` (volatile keys
    excluded BY NAME, plus any per-call ``extra_volatile_keys``).

    ``runner`` is an injection seam (defaults to digitalmodel's ``run_workflow``) so
    the template can be unit-tested over synthetic envelopes without the engine.
    """
    runner = runner or run_workflow
    env = runner(workflow_id, params=params, cfg=cfg)
    assert env.status == "ok", f"{workflow_id} errored: {env.warnings}"

    golden_path = Path(golden_path)
    if os.environ.get("REGEN_GOLDENS") == "1":
        import pytest

        capture_golden(env, golden_path)
        pytest.skip(
            f"golden {golden_path} refreshed -- re-sanction + owner sign-off "
            "required before commit"
        )

    golden = json.loads(golden_path.read_text())
    expected_hash = _golden_result_hash(golden)
    actual_hash = env.determinism["result_hash"]
    assert actual_hash == expected_hash, diff_results(
        _golden_result_payload(golden), env.result
    )

    if pin_structural:
        volatile = GOLDEN_VOLATILE_KEYS | frozenset(extra_volatile_keys)
        assert _prune_volatile(env.to_dict(), volatile) == golden["envelope_pruned"]

    return env
