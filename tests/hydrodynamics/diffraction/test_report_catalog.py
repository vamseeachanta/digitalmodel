"""Tests for the standardized per-structure-type diffraction report catalog (#282)."""

from __future__ import annotations

import pytest

from digitalmodel.hydrodynamics.diffraction.report_catalog import (
    StructureType,
    diffraction_report_skeleton,
    list_structure_types,
    report_catalog,
)
from digitalmodel.reporting import Provenance, ReportSkeleton

_CORE_REQUIRED = {
    "executive_summary",
    "identification",
    "mesh_quality",
    "stability",
    "added_mass_diagonal",
    "damping_diagonal",
    "load_raos",
    "dof_sections",
    "validation_sanity",
    "assumptions",
}


@pytest.mark.parametrize("st", list(StructureType))
def test_every_structure_type_yields_valid_skeleton(st: StructureType) -> None:
    sk = diffraction_report_skeleton(st)
    assert isinstance(sk, ReportSkeleton)
    assert st.value in sk.title
    assert sk.require_provenance is True
    # the standard core required slots are present for every hull type
    required = set(sk.required_block_keys())
    assert _CORE_REQUIRED <= required


def test_string_structure_type_accepted() -> None:
    sk = diffraction_report_skeleton("fpso")
    assert "fpso" in sk.title


def test_ship_shaped_requires_roll_damping_not_natural_periods() -> None:
    for name in ("barge", "ship", "fpso", "lngc"):
        req = set(diffraction_report_skeleton(name).required_block_keys())
        assert "roll_damping" in req
        assert "natural_periods" not in req


def test_column_stabilized_requires_natural_periods_not_roll_damping() -> None:
    for name in ("spar", "semisubmersible", "tlp"):
        req = set(diffraction_report_skeleton(name).required_block_keys())
        assert "natural_periods" in req
        assert "roll_damping" not in req


def test_catalog_covers_all_types() -> None:
    cat = report_catalog()
    assert set(cat) == set(list_structure_types())
    assert len(cat) == len(StructureType)


def test_skeleton_is_provenance_gated_and_renders_pending() -> None:
    sk = diffraction_report_skeleton(StructureType.SPAR)
    # incomplete until required slots filled + provenance declared
    assert not sk.completeness().complete
    html = sk.build_html()
    assert "slot pending (required)" in html
    assert 'id="natural_periods"' in html  # spar requires it


def test_filled_skeleton_with_provenance_is_complete() -> None:
    sk = diffraction_report_skeleton(StructureType.SHIP)
    content = {k: f"<p>{k}</p>" for k in sk.required_block_keys()}
    status = sk.completeness(content, provenance=Provenance().add("spec", "s.yml"))
    assert status.complete and status.missing_blocks == []


def test_unknown_structure_type_rejected() -> None:
    with pytest.raises(ValueError):
        diffraction_report_skeleton("submarine")


# --- live wiring: fill slots from DiffractionReportData (#282) -------------


def _report_data():
    from digitalmodel.hydrodynamics.diffraction.report_data_models import (
        DiffractionReportData,
    )

    return DiffractionReportData(
        vessel_name="Test FPSO",
        hull_type="fpso",
        executive_warnings=["check roll"],
        benchmark_html_sections={"dof_sections": "<div>RAO plots</div>"},
    )


def test_build_content_invokes_existing_builders():
    from digitalmodel.hydrodynamics.diffraction.report_catalog import (
        build_diffraction_report_content,
    )

    content = build_diffraction_report_content(_report_data())
    # builders that always render from minimal data
    assert "executive_summary" in content and "identification" in content
    # benchmark fragment is picked up
    assert content["dof_sections"] == "<div>RAO plots</div>"
    # a slot whose data is absent stays unfilled
    assert "roll_damping" not in content


def test_render_structured_report_gates_on_completeness():
    from digitalmodel.hydrodynamics.diffraction.report_catalog import (
        render_structured_report,
    )

    out, status = render_structured_report(
        "fpso", _report_data(), provenance=Provenance().add("solver_queue", "run9")
    )
    assert "<!DOCTYPE html>" in out and "run9" in out and "RAO plots" in out
    # FPSO is ship-shaped -> roll_damping required and unfilled -> incomplete
    assert not status.complete
    assert "roll_damping" in status.missing_blocks


def test_render_structured_report_complete_when_all_slots_filled(tmp_path):
    from digitalmodel.hydrodynamics.diffraction.report_catalog import (
        diffraction_report_skeleton,
        render_structured_report,
    )

    sk = diffraction_report_skeleton("ship")
    extra = {k: f"<p>{k}</p>" for k in sk.required_block_keys()}
    out_path = tmp_path / "ship_report.html"
    out, status = render_structured_report(
        "ship",
        _report_data(),
        provenance=Provenance().add("spec", "s.yml"),
        extra_blocks=extra,
        output_path=out_path,
    )
    assert out == out_path and out_path.exists()
    assert status.complete and status.missing_blocks == []
