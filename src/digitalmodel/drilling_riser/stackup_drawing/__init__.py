"""Data-driven riser stack-up drawing with a reconciliation check (#2152, #2158).

Pipeline: adapter -> :class:`StackupDrawingSpec` -> :func:`render` (SVG text)
-> :func:`reconcile` (report). The render and reconcile layers use only the
standard library; the reconciler re-derives the elevation transform from the
SVG itself, so a drawing can be checked without trusting the renderer.

Example::

    from digitalmodel.drilling_riser.stackup_drawing import (
        from_json, reconcile, render,
    )

    spec = from_json(Path("spec.json").read_text(encoding="utf-8"))
    svg = render(spec)
    assert reconcile(spec, svg)["result"] == "pass"

Workflow basename: ``riser_stackup_drawing`` (see :mod:`.workflow`).
"""

from digitalmodel.drilling_riser.stackup_drawing.adapters import (
    from_json,
    from_schedule_assembly,
    to_json,
)
from digitalmodel.drilling_riser.stackup_drawing.reconcile import PX_TOL, reconcile
from digitalmodel.drilling_riser.stackup_drawing.render import Layout, render
from digitalmodel.drilling_riser.stackup_drawing.schema import (
    NOT_FOUND,
    SCHEMA_VERSION,
    ComponentType,
    Datums,
    DesignDataItem,
    Provenance,
    Reference,
    ReferenceValue,
    StackupComponent,
    StackupDrawingSpec,
    TensionerSystem,
    TitleBlock,
    not_found_fields,
)

__all__ = [
    "NOT_FOUND",
    "PX_TOL",
    "SCHEMA_VERSION",
    "ComponentType",
    "Datums",
    "DesignDataItem",
    "Layout",
    "Provenance",
    "Reference",
    "ReferenceValue",
    "StackupComponent",
    "StackupDrawingSpec",
    "TensionerSystem",
    "TitleBlock",
    "from_json",
    "from_schedule_assembly",
    "not_found_fields",
    "reconcile",
    "render",
    "to_json",
]
