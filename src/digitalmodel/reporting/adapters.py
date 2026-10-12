#!/usr/bin/env python3
"""Domain registration for the standard report engine.

ABOUTME: A domain declares ``@report_adapter("<basename>.<kind>")`` on a
function that turns the routed cfg (inputs + results) into a
:class:`ReportSpec`; the engine hook :func:`maybe_render_report` renders when
a routed config carries a ``report:`` mapping with a ``kind``. The router
(``digitalmodel.engine``) calls the hook after every domain arm (#2212 PR2).

Config shape the hook understands::

    report:
      kind: anode_design            # adapter key is "<basename>.<kind>"
      document: {number: ..., revision: ..., title: ..., project: ..., client: ...}
      manifest: {...}               # optional report-layer manifest inputs
      pdf: auto                     # auto | off | require
      output_dir: results           # relative to the config file
      stem: my_report               # optional; defaults to the basename

Adapters are discovered lazily: when ``"<basename>.<kind>"`` is not yet
registered the hook imports ``digitalmodel.<basename>.report_adapters`` (the
convention for domain adapters) before looking again.
"""

from __future__ import annotations

import importlib
from pathlib import Path
from typing import Any, Callable, Mapping

from digitalmodel.reporting.engine import ReportArtifacts, write_report
from digitalmodel.reporting.pdf import PDF_MODES
from digitalmodel.reporting.spec import DocumentMeta, ReportSpec

AdapterFn = Callable[[dict[str, Any]], ReportSpec]

#: Registry: ``"<basename>.<kind>"`` -> adapter function.
ADAPTERS: dict[str, AdapterFn] = {}

#: Module a domain's adapters live in, formatted with the basename.
DOMAIN_ADAPTER_MODULE = "digitalmodel.{basename}.report_adapters"


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


def import_domain_adapters(basename: str) -> bool:
    """Import ``digitalmodel.<basename>.report_adapters`` if it exists.

    Returns True when the module was imported (registering its adapters as a
    side effect), False when no such module exists. Other import errors
    propagate: a broken adapter module must not be mistaken for a missing one.
    """
    name = DOMAIN_ADAPTER_MODULE.format(basename=basename)
    try:
        importlib.import_module(name)
    except ModuleNotFoundError as exc:
        if exc.name and (name == exc.name or name.startswith(exc.name + ".")):
            return False
        raise
    return True


def build_spec(key: str, cfg: dict[str, Any]) -> ReportSpec:
    """Run the registered adapter for ``key`` over the routed ``cfg``.

    The adapter sees the whole cfg (``inputs``, the domain's results block,
    ``report``, ``_config_file_path`` ...), so it can echo inputs, name the
    input file in its provenance and read document control itself.
    """
    if key not in ADAPTERS:
        import_domain_adapters(key.split(".", 1)[0])
    try:
        adapter = ADAPTERS[key]
    except KeyError:
        known = ", ".join(sorted(ADAPTERS)) or "none"
        raise AdapterError(
            f"no report adapter registered for {key!r} (known: {known})"
        ) from None
    spec = adapter(cfg)
    if not isinstance(spec, ReportSpec):
        raise AdapterError(
            f"report adapter {key!r} returned {type(spec).__name__}, not ReportSpec"
        )
    return spec


def maybe_render_report(cfg: dict[str, Any], basename: str) -> ReportArtifacts | None:
    """Render a report when ``cfg["report"]`` is a mapping; otherwise no-op.

    The adapter builds the content from the routed ``cfg``; ``report.document``
    and ``report.manifest`` override the document control and report-layer
    manifest so YAML owns document numbering. Artifact names are recorded
    under ``cfg["report"]["artifacts"]``.
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

    spec = build_spec(f"{basename}.{kind.strip()}", cfg)
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
    "DOMAIN_ADAPTER_MODULE",
    "AdapterError",
    "AdapterFn",
    "build_spec",
    "import_domain_adapters",
    "maybe_render_report",
    "report_adapter",
]
