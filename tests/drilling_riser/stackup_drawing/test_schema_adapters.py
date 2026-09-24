"""Schema round trip and adapter tests for the riser stack-up drawing (#2152)."""

from __future__ import annotations

import json

import pytest

from digitalmodel.drilling_riser.assembly import RiserStackupModel
from digitalmodel.drilling_riser.schedule_assembly import (
    ScheduleAssembly,
    StringGeometry,
)
from digitalmodel.drilling_riser.stackup_drawing import (
    NOT_FOUND,
    ComponentType,
    StackupComponent,
    StackupDrawingSpec,
    from_json,
    from_schedule_assembly,
    reconcile,
    render,
    to_json,
)

FT = 0.3048


# -- schema ---------------------------------------------------------------------------


def test_json_round_trip_is_lossless(spec, spec_text):
    again = from_json(to_json(spec))

    assert isinstance(again, StackupDrawingSpec)
    assert again == spec
    assert to_json(again) == to_json(spec)
    assert json.loads(to_json(spec)) == json.loads(spec_text)


def test_validate_accepts_the_fixture(spec):
    assert spec.validate() == []


def test_validate_flags_duplicate_ids_and_order(spec):
    spec.components[1].id = spec.components[0].id
    problems = spec.validate()
    assert "duplicate component ids" in problems

    spec2 = from_json(to_json(spec))
    spec2.components[1].id = "c02-upper-flex-joint"
    spec2.components[0], spec2.components[2] = spec2.components[2], spec2.components[0]
    assert any("out of top-to-bottom order" in p for p in spec2.validate())


def test_render_refuses_an_invalid_spec(spec):
    spec.components[1].id = spec.components[0].id
    with pytest.raises(ValueError, match="invalid spec"):
        render(spec)


def test_label_must_not_carry_digits():
    with pytest.raises(ValueError, match="digits"):
        StackupComponent(
            id="x",
            type=ComponentType.PUP_JOINT,
            label="pup joint 10 ft",
            count=1,
            joint_length_m=3.048,
            top_el_m=0.0,
            bottom_el_m=-3.048,
        )


# -- ScheduleAssembly adapter ----------------------------------------------------------


def _record(slug: str, component_type: str, count: int, length_m: float) -> dict:
    return {
        "component_id": f"DEMO/{slug}",
        "component_type": component_type,
        "weight_air_kn": 10.0,
        "submerged_weight_kn": 8.0,
        "count": count,
        "length_m": length_m,
    }


def _demo_assembly() -> ScheduleAssembly:
    """Minimal, generic tensioned string (outer barrel .. lower flex joint)."""
    records = [
        _record("telescopic-outer-barrel", "telescopic_joint", 1, 50 * FT),
        _record("pup-25ft", "pup_joint", 1, 25 * FT),
        _record("slick-joint", "riser_joint", 4, 75 * FT),
        _record("buoyancy-joint", "riser_joint_buoyant", 6, 75 * FT),
        _record("termination-joint", "termination_joint", 1, 35 * FT),
        _record("lower-flexjoint", "flexjoint", 1, 3.0),
    ]
    return ScheduleAssembly(
        rsu_id="DEMO",
        model=RiserStackupModel.from_records(records, rsu_id="DEMO"),
        geometry=StringGeometry(
            water_depth_m=800.0,
            drill_floor_above_mudline_m=825.0,
            string_base_above_mudline_m=15.0,
            internal_area_m2=0.2,
        ),
        assumptions=("generic test string",),
    )


def test_from_schedule_assembly_maps_types_and_marks_od_not_found():
    spec = from_schedule_assembly(_demo_assembly(), ordered_top_down=True)

    types = [c.type for c in spec.components]
    assert types == [
        ComponentType.TELESCOPIC_JOINT_OUTER,
        ComponentType.PUP_JOINT,
        ComponentType.RISER_JOINT_BARE,
        ComponentType.RISER_JOINT_BUOYANT,
        ComponentType.TERMINATION_JOINT,
        ComponentType.LOWER_FLEX_JOINT,
    ]
    assert all(c.od_in == NOT_FOUND for c in spec.components)
    assert spec.datums.water_depth_m == pytest.approx(800.0)
    assert spec.datums.drill_floor_el_m == pytest.approx(25.0)
    # stacked upward from the string base, contiguous
    assert spec.components[-1].bottom_el_m == pytest.approx(15.0 - 800.0)
    for upper, lower in zip(spec.components, spec.components[1:]):
        assert upper.bottom_el_m == pytest.approx(lower.top_el_m)
    assert spec.validate() == []


def test_from_schedule_assembly_renders_na_and_reconciles():
    spec = from_schedule_assembly(_demo_assembly(), ordered_top_down=True)
    svg = render(spec)
    report = reconcile(spec, svg)

    assert report["result"] == "pass", {
        k: v["failures"] for k, v in report["checks"].items()
    }
    # every NOT_FOUND OD is shown as grey n/a, never as a number
    na_od = svg.count('<tspan class="na" data-field="od_in">n/a</tspan>')
    assert na_od == len(spec.components)
    assert 'data-od-in="NOT_FOUND"' in svg
    # the string does not reach the drill floor or the mudline: the gap is
    # flagged, not hidden
    notes = " ".join(report["checks"]["d_totals"]["notes"])
    assert "does NOT close" in notes
    assert any(g["item"] == "od_in" for g in spec.gaps)


def test_from_schedule_assembly_unordered_leaves_elevations_not_found():
    spec = from_schedule_assembly(_demo_assembly())

    assert all(c.top_el_m == NOT_FOUND for c in spec.components)
    assert all(c.bottom_el_m == NOT_FOUND for c in spec.components)
