#!/usr/bin/env python3
"""Domain registration for the standard report engine.

ABOUTME: A domain declares ``@report_adapter("<basename>.<kind>")`` on a
function that turns its results dict into a :class:`ReportSpec`; the engine
hook :func:`maybe_render_report` renders when a routed config carries a
``report:`` mapping. Nothing here is wired into the router yet (#2212 PR2).

Config shape the hook understands::

    report:
      kind: anode_design            # adapter key is "<basename>.<kind>"
      document: {number: ..., revision: ..., title: ..., project: ..., client: ...}
      manifest: {...}               # optional report-layer manifest inputs
      pdf: auto                     # auto | off | require
      output_dir: results           # relative to the config file
      stem: my_report               # optional; defaults to the basename
"""

from __future__ import annotations

from pathlib import Path
from typing import Any, Callable, Mapping

from digitalmodel.reporting.engine import ReportArtifacts, write_report
from digitalmodel.reporting.pdf import PDF_MODES
from digitalmodel.reporting.spec import DocumentMeta, ReportSpec

AdapterFn = Callable[[dict[str, Any]], ReportSpec]

#: Registry: ``"<basename>.<kind>"`` -> adapter function.
ADAPTERS: dict[str, AdapterFn] = {}


class AdapterError(ValueError):
    """Raised for an unknown adapter key or a malformed ``report`` block."""


def report_adapter(key: str) -> Callable[[AdapterFn], AdapterFn]:
    """Register ``fn`` as the spec builder for ``key`` (``basename.kind``)."""
    if not isinstance(key, str) or key.count(".") != 1 or not all(key.split(".")):
        raise AdapterError(f"adapter key must be '<basename>.<kind>': got {key!r}")

    def _register(fn: AdapterFn) -> AdapterFn:
        if key in ADAPTERS and ADAPTERS[key] is not fn:
            raise AdapterError(f"report adapter {key!r} is already registered")
        ADAPTERS[key] = fn
        return fn

    return _register


def build_spec(key: str, results: dict[str, Any]) -> ReportSpec:
    """Run the registered adapter for ``key`` over ``results``."""
    try:
        adapter = ADAPTERS[key]
    except KeyError:
        known = ", ".join(sorted(ADAPTERS)) or "none"
        raise AdapterError(
            f"no report adapter registered for {key!r} (known: {known})"
        ) from None
    spec = adapter(results)
    if not isinstance(spec, ReportSpec):
        raise AdapterError(
            f"report adapter {key!r} returned {type(spec).__name__}, not ReportSpec"
        )
    return spec


def maybe_render_report(cfg: dict[str, Any], basename: str) -> ReportArtifacts | None:
    """Render a report when ``cfg["report"]`` is a mapping; otherwise no-op.

    The adapter builds the content from ``cfg[basename]`` (the domain's
    results block); ``report.document`` and ``report.manifest`` override the
    document control and report-layer manifest so YAML owns document
    numbering. Artifact names are recorded under ``cfg["report"]["artifacts"]``.
    """
    report = cfg.get("report")
    if not isinstance(report, Mapping):
        return None
    kind = report.get("kind")
    if not isinstance(kind, str) or not kind.strip():
        raise AdapterError("report.kind is required to select a report adapter")
    pdf = str(report.get("pdf", "auto")).strip().lower()
    if pdf not in PDF_MODES:
        raise AdapterError(f"report.pdf must be one of {PDF_MODES}: got {pdf!r}")

    results = cfg.get(basename)
    spec = build_spec(
        f"{basename}.{kind.strip()}",
        dict(results) if isinstance(results, Mapping) else {},
    )
    updates: dict[str, Any] = {}
    if report.get("document"):
        updates["document"] = DocumentMeta.model_validate(report["document"])
    if report.get("manifest"):
        updates["manifest"] = dict(report["manifest"])
    if updates:
        spec = ReportSpec.model_validate({**spec.model_dump(), **updates})

    out_dir = Path(str(report.get("output_dir", "results")))
    if not out_dir.is_absolute():
        base = cfg.get("_config_dir_path")
        if base:
            out_dir = Path(str(base)) / out_dir
    stem = str(report.get("stem") or basename)
    artifacts = write_report(spec, out_dir, stem, pdf=pdf)  # type: ignore[arg-type]
    cfg["report"] = {
        **report,
        "artifacts": {
            "html": artifacts.html_path.name,
            "pdf": artifacts.pdf_path.name if artifacts.pdf_path else None,
            "citations": artifacts.citations_json_path.name,
            "manifest": artifacts.manifest_path.name,
        },
        "output_dir": str(out_dir),
        "pdf_status": artifacts.pdf_status.message,
    }
    return artifacts


__all__ = [
    "ADAPTERS",
    "AdapterError",
    "AdapterFn",
    "build_spec",
    "maybe_render_report",
    "report_adapter",
]
