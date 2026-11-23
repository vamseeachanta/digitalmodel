"""Atlas catalog (#835, epic #794).

Summarize every committed atlas from its manifest: response, physics class,
axes + ranges, grid size, held-out error, provenance, and (for libraries)
coverage + stub/real. Gives an operator a single view of what's answerable
instantly, how accurate, against which standard, and what's still a stub.

    uv run python -m digitalmodel.parametric.catalog
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

from digitalmodel.parametric.atlas import Atlas

REPO_ROOT = Path(__file__).resolve().parents[3]
DEFAULT_ATLAS_ROOT = REPO_ROOT / "atlases"


def _axis_summary(ax: Any) -> str:
    if ax.is_categorical:
        return f"{ax.name}{{{len(ax.values)}}}"
    tag = "*" if getattr(ax, "derived", None) else ""  # derived axis marker
    return f"{ax.name}{tag}[{min(ax.grid):g}–{max(ax.grid):g}]"


def _summarize(atlas: Atlas) -> dict[str, Any]:
    prov = atlas.provenance
    kind = prov.get("kind", "computed")
    if kind == "library":
        solver = prov.get("solver", {})
        err = "library"
        provenance = f"{solver.get('name', '?')} {solver.get('version', '?')}"
        if solver.get("licensed") is False:
            provenance += " (STUB)"
    else:
        max_err = atlas.validation.get("max_rel_error")
        err = f"{max_err:.3f}" if max_err is not None else "n/a"
        standards = ",".join(s.get("id", "") for s in prov.get("standards", [])) or "—"
        provenance = f"{prov.get('code_version', '?')} · {standards}"
    return {
        "atlas": atlas.basename,
        "kind": kind,
        "response": atlas.response,
        "physics": atlas.physics,
        "axes": ", ".join(_axis_summary(a) for a in atlas.axes),
        "cells": len(atlas.grid),
        "max_err": err,
        "provenance": provenance,
    }


def build_catalog(atlas_root: Path = DEFAULT_ATLAS_ROOT) -> list[dict[str, Any]]:
    rows = []
    for default in sorted(Path(atlas_root).glob("*/default.txt")):
        basename = default.parent.name
        rows.append(_summarize(Atlas.load(atlas_root, basename)))
    return rows


def render_markdown(rows: list[dict[str, Any]]) -> str:
    head = ("| Atlas | Kind | Response | Physics | Axes (ranges; * = derived) | "
            "Cells | Held-out err | Provenance |\n"
            "|---|---|---|---|---|---|---|---|\n")
    body = "".join(
        f"| {r['atlas']} | {r['kind']} | {r['response']} | {r['physics']} | "
        f"{r['axes']} | {r['cells']} | {r['max_err']} | {r['provenance']} |\n"
        for r in rows
    )
    return (f"# Parametric atlas catalog\n\n{len(rows)} atlases.\n\n{head}{body}")


def main() -> None:
    print(render_markdown(build_catalog()))


if __name__ == "__main__":
    main()
