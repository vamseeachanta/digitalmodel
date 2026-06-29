# ABOUTME: Public surface for digitalmodel's deterministic workflow API (workspace-hub#3285).
"""Deterministic, in-process workflow API for digitalmodel.

Re-exports the SHARED, assetutilities-owned :class:`ResultEnvelope` contract
(#3282) and exposes digitalmodel's own ``run_workflow`` built on the #3307
embeddable engine path. The envelope/locator/hashing/provenance helpers are
imported from assetutilities -- never redefined here.
"""

from assetutilities.workflow_api import ResultEnvelope

from digitalmodel.workflow_api.runner import (
    build_cfg,
    load_registry,
    registry_path,
    resolve_registry_row,
    run_workflow,
)

__all__ = [
    "ResultEnvelope",
    "run_workflow",
    "build_cfg",
    "load_registry",
    "registry_path",
    "resolve_registry_row",
]
