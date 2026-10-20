"""Quantity semantics with synthetic policy inputs, not code acceptance limits.

Hand calculation: a caller supplies capacity/demand = 60/80 = 0.75,
allowable = 1, and a linear limit rule. A stress limit of 80 MPa becomes
80 * 0.75 / 1 = 60 MPa. Bands (0.1, 0, 2 years) are illustrative inputs:
0.75 < 1 implies DERATE; 1 <= 1.05 < 1 + 0.1 implies MONITOR.
The scaling is algebraic bookkeeping, not qualification of a stress method.
"""

import json

import pytest

from digitalmodel.asset_integrity.assessment.ffs_decision import (
    DecisionBands,
    Verdict,
    decide,
    reduced_mawp,
    scaled_limit,
)
from digitalmodel.asset_integrity.assessment.ffs_report import (
    FFSReport,
    ReportMethodology,
)
from digitalmodel.asset_integrity.applicability import Applicability
from digitalmodel.units import Q_


BANDS = DecisionBands(monitor_band=0.1, derate_floor=0, repair_life_yr=2)
METHOD = ReportMethodology(
    name="Caller-supplied membrane capacity/demand screen",
    qualification="Illustrative only; combined loading NOT_EVALUATED.",
    references=("Independent hand calculation in test module docstring",),
)


def stress_decision(**kwargs):
    return decide(
        0.75,
        1,
        5,
        "pipeline",
        margin_label="capacity/demand",
        bands=BANDS,
        derate=scaled_limit(
            Q_(80, "MPa"),
            "[pressure]",
            label="axial stress limit",
            quantity="axial_stress",
            action="REDUCE AXIAL STRESS",
        ),
        **kwargs,
    )


def test_axial_stress_is_not_a_pressure_rating():
    decision = stress_decision()
    result = decision.to_dict()
    assert decision.verdict is Verdict.DERATE
    assert result["action"] == "REDUCE AXIAL STRESS"
    assert result["margin_label"] == "capacity/demand"
    assert result["derated_quantity"] == "axial_stress"
    assert result["derated"] == pytest.approx(60)
    assert result["derated_units"] == "MPa"
    assert "rerated_mawp_psi" not in result
    assert "rsf" not in result and "rsf_a" not in result
    assert "capacity/demand" in result["governing_criterion"]
    assert "RSF" not in result["governing_criterion"]
    assert "MAWP" not in result["governing_criterion"]
    json.dumps(result)


def test_untagged_rating_semantics_do_not_infer_mawp_from_dimensions():
    result = decide(
        0.75,
        1,
        5,
        "pipeline",
        bands=BANDS,
        margin_label="capacity/demand",
        derate=scaled_limit(Q_(80, "MPa"), "[pressure]"),
    ).to_dict()
    assert result["derated"] == pytest.approx(60)
    assert result["derated_quantity"] == "limit"
    assert "rerated_mawp_psi" not in result
    assert result["action"] == "REDUCE LIMIT"


def test_margin_override_requires_explicit_bands():
    with pytest.raises(ValueError, match="bands"):
        decide(1.05, 1, 5, "pipeline", margin_label="capacity/demand")


def test_capacity_monitor_uses_supplied_bands_and_wording():
    result = decide(1.05, 1, 5, "pipeline", margin_label="capacity/demand", bands=BANDS)
    assert result.verdict is Verdict.MONITOR
    assert "RSF" not in result.governing_criterion
    assert "rsf" not in result.to_dict()


def test_non_rsf_policy_does_not_emit_rsf_aliases():
    result = decide(1.05, 1, 5, "mooring_chain", bands=BANDS).to_dict()
    assert result["margin_label"] == "RF"
    assert "rsf" not in result and "rsf_a" not in result


def test_legacy_pressure_payload_retains_rsf_and_mawp():
    """Hand calculation: 1000 * 0.72 / 0.9 = 800 psi (synthetic inputs)."""
    result = decide(
        0.72, 0.9, 5, "pressure", derate=reduced_mawp(Q_(1000, "psi"))
    ).to_dict()
    assert result["rsf"] == 0.72 and result["rsf_a"] == 0.9
    assert result["rerated_mawp_psi"] == pytest.approx(800)
    assert result["derated_quantity"] == "mawp"
    assert result["action"] == "RE_RATE"


def test_no_rating_preserves_legacy_null_mawp():
    assert decide(0.72, 0.9, 5, "pressure").to_dict()["rerated_mawp_psi"] is None


def test_applicability_flag_keeps_escalate_action():
    app = Applicability(ok=False, flags=["outside"], notes=["Outside synthetic screen"])
    decision = stress_decision(applicability=app)
    assert decision.verdict is Verdict.ESCALATE
    assert decision.action == "ESCALATE"
    assert "reference only" in decision.governing_criterion


@pytest.mark.parametrize("label", ["", "   "])
def test_empty_margin_label_rejected(label):
    with pytest.raises(ValueError, match="margin_label"):
        decide(1.05, 1, 5, "pipeline", margin_label=label, bands=BANDS)


def test_footer_never_infers_a_standard():
    footer = FFSReport._html_foot()
    assert "API 579" not in footer
    assert "NOT_EVALUATED" in footer


def test_footer_displays_explicit_method_and_qualification():
    footer = FFSReport._html_foot(methodology=METHOD)
    assert METHOD.name in footer and METHOD.qualification in footer
    assert METHOD.references[0] in footer
    assert "API 579" not in footer


def test_methodology_and_flags_are_escaped():
    app = Applicability(ok=False, flags=["<flag>"], notes=["<note>"])
    method = ReportMethodology("<method>", "<qualification>", ("<reference>",))
    footer = FFSReport._html_foot(app, methodology=method)
    for text in ("flag", "note", "method", "qualification", "reference"):
        assert f"<{text}>" not in footer
        assert f"&lt;{text}&gt;" in footer


def test_generic_screening_report_uses_methodology_without_part5_claim():
    report = FFSReport.generate_screening_html(
        "illustrative-component",
        "Axial membrane screen",
        stress_decision().to_dict(),
        {"Inputs and answer": {"demand (MPa)": 80, "limit (MPa)": 60}},
        ["Synthetic values; no method qualification."],
        methodology=METHOD,
    )
    assert METHOD.name in report and METHOD.qualification in report
    assert "API 579" not in report
    assert "REDUCE AXIAL STRESS" in report
    assert report.count("</html>") == 1


def test_stress_with_rsf_margin_still_has_no_pressure_action():
    """The ratio can be RSF while the reduced operating quantity is stress."""
    result = decide(
        0.75,
        1,
        5,
        "pipeline",
        bands=BANDS,
        derate=scaled_limit(Q_(80, "MPa"), "[pressure]", quantity="axial_stress"),
    ).to_dict()
    assert result["action"] == "REDUCE LIMIT"
    assert result["rsf"] == 0.75
    assert "rerated_mawp_psi" not in result


def test_explicit_mawp_requires_pressure_dimensions():
    from digitalmodel.asset_integrity.assessment.ffs_decision import (
        Derating,
        UnitTagError,
    )

    with pytest.raises(UnitTagError):
        Derating(Q_(80, "kN"), "Invalid pressure rating", quantity="mawp")


def test_worked_example():
    """Independent expected answer: 60/80 = 0.75; 80 * 0.75 = 60 MPa."""
    import runpy
    from pathlib import Path

    example = (
        Path(__file__).resolve().parents[2]
        / "examples/domains/asset_integrity/quantity_semantics.py"
    )
    decision, report = runpy.run_path(str(example))["worked_example"]()
    assert decision["margin"] == pytest.approx(0.75)
    assert decision["derated"] == pytest.approx(60)
    assert decision["action"] == "REDUCE AXIAL STRESS"
    assert "MPa" in report and "NOT_EVALUATED" in report
    assert "API 579" not in report


def test_metal_loss_report_forwards_methodology_to_footer():
    """Geometry is synthetic metadata; only text propagation is checked."""
    import pandas as pd

    decision = decide(0.75, 1, 5, "pressure", bands=BANDS).to_dict()
    report = FFSReport.generate_html(
        pd.DataFrame([[0.4]]),
        decision,
        "illustrative-component",
        nominal_od_in=10,
        nominal_wt_in=0.5,
        t_min_in=0.3,
        design_code="caller-supplied",
        design_pressure_psi=100,
        methodology=METHOD,
    )
    assert METHOD.name in report and METHOD.qualification in report
    assert "Edition criteria are applied" not in report


def test_generic_screening_without_methodology_is_unqualified():
    report = FFSReport.generate_screening_html(
        "illustrative-component",
        "Generic screen",
        stress_decision().to_dict(),
        {},
        [],
    )
    assert "Methodology and code qualification: NOT_EVALUATED" in report
    assert "API 579" not in report
