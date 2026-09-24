"""UV-workflow router for the ``riser_stackup_drawing`` basename (#2152).

Input (paths relative to the input file)::

    basename: riser_stackup_drawing
    riser_stackup_drawing:
      spec: synthetic_spec.json   # StackupDrawingSpec JSON
      output_dir: results         # optional, default "results"

Writes ``<stem>_stackup.svg``, ``<stem>_stackup_spec.json`` (the spec as
rendered) and ``<stem>_reconcile.json`` into ``output_dir``, where ``<stem>``
is the input file's stem. Raises :class:`StackupReconcileError` after writing
all three when the reconciliation result is ``fail``, so the run exits
non-zero. A ``pass_with_open_items`` result completes; the open items are in
the report and the summary.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

from digitalmodel.drilling_riser.stackup_drawing.adapters import from_json, to_json
from digitalmodel.drilling_riser.stackup_drawing.reconcile import reconcile
from digitalmodel.drilling_riser.stackup_drawing.render import render

__all__ = ["StackupReconcileError", "router"]

_KEY = "riser_stackup_drawing"


class StackupReconcileError(RuntimeError):
    """The rendered drawing does not reconcile with its spec."""


def router(cfg: dict) -> dict:
    """Render the configured spec, reconcile it and record the outputs."""
    settings = cfg.get(_KEY) or {}
    spec_ref = settings.get("spec")
    if not spec_ref:
        raise ValueError(f"{_KEY}.spec (path to the spec JSON) is required")
    config_dir = _config_dir(cfg)
    spec_path = _resolve(config_dir, spec_ref)
    spec = from_json(spec_path.read_text(encoding="utf-8"))

    output_dir = _resolve(config_dir, settings.get("output_dir", "results"))
    output_dir.mkdir(parents=True, exist_ok=True)
    stem = _input_stem(cfg)
    svg_path = output_dir / f"{stem}_stackup.svg"
    spec_out = output_dir / f"{stem}_stackup_spec.json"
    report_path = output_dir / f"{stem}_reconcile.json"

    svg = render(spec)
    report = reconcile(spec, svg)
    _write(svg_path, svg)
    _write(spec_out, to_json(spec))
    _write(report_path, json.dumps(report, indent=2, ensure_ascii=False) + "\n")

    checks = {k: v["status"] for k, v in report["checks"].items()}
    cfg[_KEY] = {
        "spec": str(spec_path),
        "svg": str(svg_path),
        "spec_json": str(spec_out),
        "reconcile_report": str(report_path),
        "result": report["result"],
        "checks": checks,
    }
    # "pass_with_open_items" completes: the open items are in the report and
    # the summary; only a failed check stops the run.
    if report["result"] == "fail":
        failed = {
            k: v["failures"][:3]
            for k, v in report["checks"].items()
            if v["status"] == "fail"
        }
        raise StackupReconcileError(
            f"stack-up drawing does not reconcile ({report_path}): {failed}"
        )
    return cfg


def _write(path: Path, text: str) -> None:
    # newline="" keeps LF on every platform so outputs are byte-reproducible
    with path.open("w", encoding="utf-8", newline="") as stream:
        stream.write(text)


def _resolve(base: Path, ref: Any) -> Path:
    path = Path(str(ref))
    return path if path.is_absolute() else base / path


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return _KEY
