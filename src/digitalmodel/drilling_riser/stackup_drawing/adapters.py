"""Adapters that build or serialise a :class:`StackupDrawingSpec` (#2152).

* :func:`from_json` / :func:`to_json` - the spec's JSON interchange form.
* :func:`from_schedule_assembly` - maps
  :class:`digitalmodel.drilling_riser.schedule_assembly.ScheduleAssembly`
  (duck typed: only ``rsu_id``, ``geometry`` and ``model.items`` are read).

Extractors tied to one workbook or report layout stay in the private data
repositories that own those documents; they produce the same spec JSON.
"""

from __future__ import annotations

from typing import Any

from digitalmodel.drilling_riser.stackup_drawing.schema import (
    NOT_FOUND,
    Datums,
    Provenance,
    ReferenceValue,
    StackupComponent,
    StackupDrawingSpec,
    TitleBlock,
)
from digitalmodel.drilling_riser.stackup_drawing.schema import (
    ComponentType as CT,
)

__all__ = ["from_json", "from_schedule_assembly", "to_json"]


def from_json(text: str) -> StackupDrawingSpec:
    """Parse spec JSON text (inverse of :func:`to_json`)."""
    return StackupDrawingSpec.from_json(text)


def to_json(spec: StackupDrawingSpec) -> str:
    """Serialise ``spec`` to deterministic, indented JSON text."""
    return spec.to_json()


#: ScheduleAssembly ``component_type`` -> (drawing type, label).
_SA_TYPES: dict[str, tuple[CT, str]] = {
    "riser_joint": (CT.RISER_JOINT_BARE, "bare joints"),
    "riser_joint_buoyant": (CT.RISER_JOINT_BUOYANT, "buoyant joints"),
    "pup_joint": (CT.PUP_JOINT, "pup joint"),
    "termination_joint": (CT.TERMINATION_JOINT, "termination joint"),
    "telescopic_joint": (CT.TELESCOPIC_JOINT_OUTER, "Telescopic joint"),
}


def from_schedule_assembly(
    assembly: Any,
    *,
    ordered_top_down: bool = False,
    title: str = "Riser stack-up",
    configuration: str = "",
) -> StackupDrawingSpec:
    """Map a ``ScheduleAssembly`` (tensioned string only) to a spec.

    What the assembly carries: ``model.items`` (component record + count; the
    record has ``component_id``, ``component_type`` and optional
    ``length_m``) and ``geometry`` (water depth, drill-floor and string-base
    elevations above mudline). It carries NO diameters, NO per-item elevations
    and no guaranteed vertical order (one loader appends buoyancy classes
    last). Hence:

    * every OD (and buoyancy OD) is ``NOT_FOUND`` and draws as "n/a";
    * elevations are stacked upward from the string base only when the caller
      asserts ``ordered_top_down=True``; they stay ``NOT_FOUND`` above the
      first item without a length (such items cannot be drawn, and the
      reconciler reports them);
    * LMRP/BOP/wellhead and the parts above the outer barrel are absent, so
      the string does not span mudline to drill floor. When every item is
      stacked, that shortfall is recorded as the ``closure_residual_m``
      reference total (basis ``adapter``) and as an open gap, so the drawing
      states it rather than hiding it.
    """
    geo = assembly.geometry
    wd = float(geo.water_depth_m)
    floor_above_ml = float(geo.drill_floor_above_mudline_m)
    rsu = str(getattr(assembly, "rsu_id", ""))
    handle = f"ScheduleAssembly({rsu})"

    def p(path: str) -> Provenance:
        return Provenance(f"{handle}.{path}", "adapter")

    datums = Datums(
        water_depth_m=wd,
        drill_floor_el_m=floor_above_ml - wd,
        air_gap_m=floor_above_ml - wd,
        mudline_el_m=-wd,
        provenance={
            "water_depth_m": p("geometry.water_depth_m"),
            "drill_floor_el_m": p("geometry.drill_floor_above_mudline_m"),
            "air_gap_m": p("geometry.drill_floor_above_mudline_m"),
            "mudline_el_m": p("geometry.water_depth_m"),
        },
    )
    comps: list[StackupComponent] = []
    for i, item in enumerate(assembly.model.items, start=1):
        rec = item.component
        ctype_raw = str(rec.get("component_type", ""))
        cid_src = str(rec.get("component_id", f"item{i}"))
        if ctype_raw == "flexjoint":
            ctype, label = (
                (CT.LOWER_FLEX_JOINT, "Lower flex joint (LFJ)")
                if "lower" in cid_src.lower()
                else (CT.UPPER_FLEX_JOINT, "Upper flex joint (UFJ)")
            )
        else:
            ctype, label = _SA_TYPES.get(ctype_raw, (CT.OTHER, "Component"))
        length = rec.get("length_m")
        comps.append(
            StackupComponent(
                id=f"c{i:02d}-{ctype.value.replace('_', '-')}",
                type=ctype,
                label=label,
                count=int(item.count),
                joint_length_m=float(length) if length is not None else NOT_FOUND,
                top_el_m=NOT_FOUND,
                bottom_el_m=NOT_FOUND,
                od_in=NOT_FOUND,
                buoyancy_od_in=NOT_FOUND if ctype == CT.RISER_JOINT_BUOYANT else None,
                source=f"{handle}.model.items[{i - 1}] {cid_src}",
                provenance={
                    "count": p(f"model.items[{i - 1}].count"),
                    "joint_length_m": p(f"model.items[{i - 1}].component.length_m"),
                },
            )
        )

    # drawing-level entries: no single component row carries them (#2158)
    gaps = [
        {
            "component_id": None,
            "item": "od_in",
            "status": "open",
            "detail": "ScheduleAssembly carries no diameters",
        },
        {
            "component_id": None,
            "item": "subsea stack",
            "status": "open",
            "detail": "LMRP/BOP/wellhead/conductor are outside the tensioned string",
        },
    ]
    reference: dict[str, ReferenceValue] = {}
    if ordered_top_down:
        base_ml = float(geo.string_base_above_mudline_m)
        z = base_ml - wd
        stacked_all = True
        for c in reversed(comps):
            if not c.known("joint_length_m"):
                stacked_all = False
                break
            c.bottom_el_m = z
            z += int(c.count) * float(c.joint_length_m)
            c.top_el_m = z
            c.provenance["top_el_m"] = p(
                "stacked from geometry.string_base_above_mudline_m"
            )
            c.provenance["bottom_el_m"] = c.provenance["top_el_m"]
        if stacked_all and comps:
            string_len = sum(int(c.count) * float(c.joint_length_m) for c in comps)
            residual = string_len - floor_above_ml
            reference["closure_residual_m"] = ReferenceValue(
                residual,
                f"{handle}.geometry",
                "adapter",
                "tensioned string length minus drill floor above mudline",
            )
            gaps.append(
                {
                    "component_id": None,
                    "item": "closure",
                    "status": "open",
                    "detail": (
                        f"tensioned string only: {base_ml:.3f} m from mudline to "
                        f"string base and {floor_above_ml - wd - z:.3f} m from "
                        "string top to drill floor are not in the ScheduleAssembly"
                    ),
                }
            )
    return StackupDrawingSpec(
        spec_id=f"schedule-assembly-{rsu}".lower(),
        datums=datums,
        components=comps,
        title_block=TitleBlock(
            title=title, configuration=configuration or "Tensioned string only"
        ),
        reference_totals=reference,
        gaps=gaps,
    )
