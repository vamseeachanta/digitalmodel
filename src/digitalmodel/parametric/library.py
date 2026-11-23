"""Sparse atlas *libraries* for licensed solvers (#801, epic #794).

Diffraction (AQWA/OrcaWave) and OrcaFlex cost a license + minutes per run, so
they cannot be densely gridded like the offline workflows. Instead an atlas is
built from a SPARSE set of canonical cases that were solved once on the licensed
machine, and a query either matches a covered case (interpolating only WITHIN
it) or escalates. The coverage model is "exact key + interpolate-within":

  * a categorical KEY axis (e.g. vessel/draft) is matched exactly — a value not
    in the enumerated coverage map escalates by default, never interpolates
    across cases;
  * continuous axes within a covered case (e.g. frequency, heading) interpolate
    between the solver's grid points.

This reuses the regular Atlas (one categorical axis + continuous axes already
gives exactly this behaviour). What differs is the SOURCE: the grid comes from
recorded solver output, not a response function, so it is built here rather than
via generate.RESPONSE_FUNCS, and the provenance records the licensed run (not a
code version). Populating a real library is an operator step on the licensed
machine; the committed library ships a clearly-marked STUB so the mechanism is
testable and wired end to end.
"""

from __future__ import annotations

import hashlib
import json
from typing import Any

import pandas as pd

from digitalmodel.parametric.atlas import Atlas, Axis


def build_library_atlas(
    *,
    basename: str,
    key_axis: str,
    key_values: list[str],
    continuous_axes: list[Axis],
    records: list[dict[str, Any]],
    response: str,
    solver: dict[str, Any],
    coverage_note: str,
) -> Atlas:
    """Assemble a library Atlas from recorded canonical-case solver output.

    ``records`` is the full cartesian product of (key_values x continuous grids)
    with the ``response`` column filled from the solver (or a stub). The key
    axis is categorical (exact-match coverage); the continuous axes interpolate
    within a case.
    """
    axes = [Axis(name=key_axis, values=list(key_values)), *continuous_axes]
    grid = pd.DataFrame(records)

    coverage = {
        "key_axis": key_axis,
        "covered_cases": list(key_values),
        "continuous_axes": {
            ax.name: {"scale": ax.scale, "min": min(ax.grid), "max": max(ax.grid)}
            for ax in continuous_axes
        },
        "note": coverage_note,
    }
    provenance = {
        "basename": basename,
        "kind": "library",
        "solver": solver,  # name, version, licensed, run_date, case provenance
        "coverage": coverage,
    }
    blob = json.dumps(
        {"axes": [a.to_dict() for a in axes], "solver": solver,
         "cases": list(key_values)},
        sort_keys=True,
    )
    atlas_id = hashlib.sha256(blob.encode()).hexdigest()[:12]

    return Atlas(
        basename=basename,
        atlas_id=atlas_id,
        physics="linear",  # interpolate the recorded value within a case
        response=response,
        axes=axes,
        grid=grid,
        provenance=provenance,
        validation={
            "kind": "library",
            "note": (
                "Within-case interpolation between solver grid points; accuracy "
                "is bounded by the solver grid density. No held-out recompute "
                "gate — the licensed solver is not re-run here."
            ),
        },
    )
