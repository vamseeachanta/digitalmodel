"""Shared helpers for the OrcaFlex report sections.

The sections select model objects by OrcFxAPI type and render the same HTML
fragments. Those choices live here once, so a section states *what* it reports
and this module owns *how* objects are found and fragments are written.

``OrcFxAPI`` is imported when a selection helper is called, not when this
module is imported, so the package stays importable without an OrcaFlex
installation and tests can substitute a fake module in ``sys.modules``.
"""
from __future__ import annotations

import json
import warnings
from typing import Any, Iterable

__all__ = [
    "objects_of_type",
    "lines",
    "vessels",
    "buoys",
    "kv_table",
    "attr_text",
    "plotly_chart",
]


def objects_of_type(model: Any, type_name: str) -> list:
    """Return ``model.objects`` that are instances of ``OrcFxAPI.<type_name>``.

    Objects keep their model order. Raises ``ImportError`` when OrcFxAPI is not
    importable and ``AttributeError`` when it has no attribute ``type_name``.
    """
    import OrcFxAPI

    cls = getattr(OrcFxAPI, type_name)
    return [o for o in model.objects if isinstance(o, cls)]


def lines(model: Any) -> list:
    """Line objects of ``model``, in model order."""
    return objects_of_type(model, "Line")


def vessels(model: Any) -> list:
    """Vessel objects of ``model``, in model order."""
    return objects_of_type(model, "Vessel")


def buoys(model: Any, kind: str) -> list:
    """Buoy objects whose class name contains ``kind`` (``"3D"`` or ``"6D"``)."""
    return [o for o in objects_of_type(model, "Buoy") if kind in type(o).__name__]


def kv_table(rows: Iterable[tuple[str, str]]) -> str:
    """Two-column label/value table with the value in bold."""
    trs = "\n".join(
        f"    <tr><td>{label}</td><td><strong>{value}</strong></td></tr>"
        for label, value in rows
    )
    return f'<table class="ofx">\n{trs}\n</table>'


def attr_text(obj: Any, attr: str, default: str = "N/A") -> str:
    """``str(getattr(obj, attr))``, or ``default`` if reading it raises."""
    try:
        return str(getattr(obj, attr))
    except Exception:
        return default


def plotly_chart(div_id: str, traces: list, layout: dict) -> str:
    """A ``<div>`` and the inline script that draws ``traces`` into it."""
    return (
        f'<div id="{div_id}" style="width:100%;margin-bottom:1rem;"></div>\n'
        f'<script>Plotly.newPlot("{div_id}", '
        f"{json.dumps(traces)}, {json.dumps(layout)}, "
        f"{{responsive:true, displayModeBar:false}});</script>"
    )


def deprecated_alias_getattr(module_name: str, aliases: dict[str, str]):
    """Build a module ``__getattr__`` that serves old private helper names.

    ``aliases`` maps an old name to the name of its replacement in this module.
    Each access warns with ``DeprecationWarning`` and returns the replacement.
    """

    def __getattr__(name: str):
        if name in aliases:
            target = aliases[name]
            warnings.warn(
                f"{module_name}.{name} is deprecated; use "
                f"digitalmodel.orcaflex.reporting.sections._common.{target}",
                DeprecationWarning,
                stacklevel=2,
            )
            return globals()[target]
        raise AttributeError(f"module {module_name!r} has no attribute {name!r}")

    return __getattr__
